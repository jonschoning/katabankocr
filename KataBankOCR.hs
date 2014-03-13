module KataBankOCR (Status (..), Account, createAccount, parseAccountOnly, parseAccount, isValid, guessIfNotOK) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.List (transpose, delete, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

data Account = Acct { acctnum :: AccountNum
                    , status :: Status
                    , ambs :: [AccountNum] }
               deriving (Eq)

data Status = OK | ERR | ILL | AMB
              deriving (Eq, Show, Enum)

type AccountNum = String
type Digit = String
type AccountWithDigits = (Account, [Digit])

instance Show Account where
  show = concat . sequence [acctnum, disps . status, dispa . ambs]
    where 
      disps OK = ""
      disps s = ' ' : show s
      dispa [] = ""
      dispa s = ' ' : show s

-- create
createAccount :: AccountNum -> Account
createAccount = Acct <$> id <*> initialStatus <*> const []

-- tries to find a (unique) correct account number
guessIfNotOK :: AccountWithDigits -> Account
guessIfNotOK (acct@(Acct _ OK _), _) = acct
guessIfNotOK (acct, digits) = result (length validGuesses)
  where 
    validGuesses = filter isValid $ map (fst . createAccountFromDigits) (generateDigitLists digits)
    result n | n == 0 = acct { status = ILL }
             | n > 1  = acct { status = AMB, ambs = sort (map acctnum validGuesses) }
             | otherwise = head validGuesses

generateDigitLists :: [Digit] -> [[Digit]]
generateDigitLists = generateReplacements (concatMap generateDigits . (:[]))
    
generateDigits :: Digit -> [Digit]
generateDigits = generateReplacements (`delete` " |_")

generateReplacements :: (a -> [a]) -> [a] -> [[a]]
generateReplacements replacements = (map <$> substituteAtIndex <*> replacements . atIndex =<<) . mapIndex
  where 
    atIndex = uncurry $ flip (!!)
    mapIndex x = map (\i -> (i, x)) [0..(length x-1)]
    substituteAtIndex (i, xs) b = let (a, c) = splitAt i xs in a ++ [b] ++ drop 1 c

-- status
initialStatus :: AccountNum -> Status
initialStatus s 
  | '?' `elem` s = ILL
  | isChecksumValid s = OK
  | otherwise = ERR

isValid :: Account -> Bool
isValid (Acct _ OK _) = True
isValid (Acct {}) = False

-- checksum
isChecksumValid :: AccountNum -> Bool
isChecksumValid a 
  | any (not . isDigit) a = False
  | otherwise = 0 == checksum a

checksum :: AccountNum -> Int
checksum = (`mod` 11) . sum . zipWith (*) [9, 8..1] . map digitToInt

-- parsing
parseAccountOnly :: [String] -> Account
parseAccountOnly = fst . parseAccount

parseAccount :: [String] -> AccountWithDigits
parseAccount = createAccountFromDigits . makeDigitsFromStrings

createAccountFromDigits :: [Digit] -> AccountWithDigits
createAccountFromDigits = createAccount . (maybe "?" show . parseDigit =<<) &&& id

parseDigit :: Digit -> Maybe Integer
parseDigit  = flip M.lookup ocrMap

makeDigitsFromStrings :: [String] -> [Digit]
makeDigitsFromStrings = map concat . transpose . map (chunksOf 3)

ocrMap :: M.Map Digit Integer
ocrMap = M.fromList $ (`zip` [0..9]) . makeDigitsFromStrings $
         [" _     _  _     _  _  _  _  _ ",
          "| |  | _| _||_||_ |_   ||_||_|",
          "|_|  ||_  _|  | _||_|  ||_| _|"]
