module KataBankOCR (Status (..), Account, pretty, createAccount, parseAccountOnly, parseAccount, isValid, guessIfNotOK) where

import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit, toUpper)
import Data.List (transpose, delete, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

data Account = Acct { acctnum :: AccountNum
                    , status :: Status
                    , ambs :: [AccountNum] }
               deriving (Eq, Show)

data Status = OK | Err | Ill | Amb
              deriving (Eq, Show, Enum)

type AccountNum = String
type Digit = String
type AccountWithDigits = (Account, [Digit])

-- create
createAccount :: AccountNum -> Account
createAccount num = Acct { acctnum = num
                         , status = initialStatus num
                         , ambs = []
                         }

pretty :: Account -> String
pretty = concat . sequence [acctnum, disps . status, dispa . ambs]
  where 
    disps OK = ""
    disps s = ' ' : map toUpper (show s)
    dispa [] = ""
    dispa s = ' ' : show s

-- tries to find a (unique) correct account number
guessIfNotOK :: AccountWithDigits -> Account
guessIfNotOK (acct@(Acct _ OK _), _) = acct
guessIfNotOK (acct, digits) = guess
  where 
    validGuesses = filter isValid $ map (fst . createAccountFromDigits) (generateDigitLists digits)
    numValid = length validGuesses
    guess | numValid == 0 = acct { status = Ill }
          | numValid > 1  = acct { status = Amb, ambs = sort (map acctnum validGuesses) }
          | otherwise = head validGuesses

generateDigitLists :: [Digit] -> [[Digit]]
generateDigitLists = generateReplacements (concatMap generateDigits . (:[]))
    
generateDigits :: Digit -> [Digit]
generateDigits = generateReplacements (`delete` " |_")

generateReplacements :: (a -> [a]) -> [a] -> [[a]]
generateReplacements replacementsFor xs = 
    concatMap (\i -> substituteAtIndex i xs `map` (replacementsFor $ xs !! i)) $ zipWith const [0 ..] xs

substituteAtIndex :: Int -> [a] -> a -> [a]
substituteAtIndex i xs x = take i xs ++ [x] ++ drop (i + 1) xs

-- status
initialStatus :: AccountNum -> Status
initialStatus s 
  | '?' `elem` s      = Ill
  | isChecksumValid s = OK
  | otherwise         = Err

isValid :: Account -> Bool
isValid (Acct _ OK _) = True
isValid  _            = False

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
