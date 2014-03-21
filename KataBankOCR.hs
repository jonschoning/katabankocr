module KataBankOCR (Status (..), Account, pretty, createAccount, parseAccountOnly, parseAccount, isValid, guessIfNotOK) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.List (transpose, delete, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map.Strict as M

data Account = Acct { acctnum :: AccountNum
                    , status :: Status
                    , ambiguousAcctNums :: [AccountNum] }
               deriving (Eq, Show)

data Status = OK | BadChecksum | Illegible | Ambiguous
              deriving (Eq, Show, Enum)

type AccountNum = String
type Digit = String
type AccountWithDigits = (Account, [Digit])

illegibleSymbol :: Char
illegibleSymbol = '?'

-- create
createAccount :: AccountNum -> Account
createAccount num = Acct { acctnum = num
                         , status = initialStatus num
                         , ambiguousAcctNums = []
                         }

pretty :: Account -> String
pretty = concat . sequence [acctnum, prettys . status, prettya . ambiguousAcctNums]
  where 
    prettys OK = ""
    prettys Illegible = " ILL"
    prettys Ambiguous = " AMB"
    prettys BadChecksum = " ERR"
    prettya [] = ""
    prettya s = ' ' : show s

-- tries to find a (unique) correct account number
guessIfNotOK :: AccountWithDigits -> Account
guessIfNotOK (acct@(Acct _ OK _), _) = acct
guessIfNotOK awd@(acct, _) = guess
  where 
    validGuesses = generateValidGuesses awd
    numValid = length validGuesses
    guess | numValid == 0 = acct { status = Illegible }
          | numValid > 1  = acct { status = Ambiguous, ambiguousAcctNums = sort (map acctnum validGuesses) }
          | otherwise = head validGuesses

generateValidGuesses :: AccountWithDigits -> [Account]
generateValidGuesses (Acct num _ _, digits) = do
  digitAtIndex  <- zip digits [0..]
  newDigit      <- generateDigits $ fst digitAtIndex
  let mNewDigit = parseDigit newDigit
  guard (isJust mNewDigit)
  let newacct   = createAccount $ substituteAtIndex (snd digitAtIndex) num $ (head.show) (fromJust mNewDigit)
  guard (isValid newacct)
  return newacct

generateDigits :: Digit -> [Digit]
generateDigits = generateReplacements (`delete` " |_")

generateReplacements :: (a -> [a]) -> [a] -> [[a]]
generateReplacements replacementsFor xs = 
  concatMap (\i -> substituteAtIndex i xs `map` replacementsFor (xs !! i)) $ zipWith const [0 ..] xs

substituteAtIndex :: Int -> [a] -> a -> [a]
substituteAtIndex i xs x = take i xs ++ [x] ++ drop (i + 1) xs

-- status
initialStatus :: AccountNum -> Status
initialStatus s 
  | illegibleSymbol `elem` s 
                      = Illegible
  | isChecksumValid s = OK
  | otherwise         = BadChecksum

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
createAccountFromDigits = createAccount . concatMap (maybe [illegibleSymbol] show . parseDigit) &&& id

parseDigit :: Digit -> Maybe Integer
parseDigit  = flip M.lookup ocrMap

makeDigitsFromStrings :: [String] -> [Digit]
makeDigitsFromStrings = map concat . transpose . map (chunksOf 3)

ocrMap :: M.Map Digit Integer
ocrMap = M.fromList $ (`zip` [0..9]) . makeDigitsFromStrings $
         [" _     _  _     _  _  _  _  _ ",
          "| |  | _| _||_||_ |_   ||_||_|",
          "|_|  ||_  _|  | _||_|  ||_| _|"]
