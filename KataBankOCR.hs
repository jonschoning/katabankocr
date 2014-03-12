module KataBankOCR (Status (..), Account, createAccount, parseAccount, isValid) where

import Control.Applicative (liftA2)
import Data.Char (digitToInt, isDigit)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

data Account = Acct { num :: AccountNum, status :: Status }
               deriving (Eq)

data Status = OK | ERR | ILL | AMB
              deriving (Eq, Show, Enum)

instance Show Account where
  show = showWithStatus

type AccountNum = String
type AccountWithGuesses = (Account, [AccountNum])
type DigitLines = [String]

-- create
createAccount :: AccountNum -> Account
createAccount = liftA2 Acct id initialStatus

-- tries to find a (unique) correct account number
guess :: Account -> AccountWithGuesses
guess = undefined

-- status
initialStatus :: AccountNum -> Status
initialStatus s 
  | any (== '?') s = ILL
  | isChecksumValid s = OK
  | otherwise = ERR

showWithStatus :: Account -> String
showWithStatus = liftA2 (++) num (disp . status)
  where disp OK = ""
        disp s = ' ' : show s

isValid :: Account -> Bool
isValid (Acct _ OK) = True
isValid (Acct _ _) = False

-- checksum
isChecksumValid :: AccountNum -> Bool
isChecksumValid a 
  | any (not . isDigit) a = False
  | otherwise = 0 == checksum a

checksum :: AccountNum -> Int
checksum = (`mod` 11) . sum . zipWith (*) [9, 8..1] . map digitToInt

-- parsing
parseAccount :: String -> Account
parseAccount = createAccount . concatMap (maybe "?" show . parseAccountDigit) . makeDigitLines

parseAccountDigit :: DigitLines -> Maybe Integer
parseAccountDigit  = (flip M.lookup ocrMap) . concat

makeDigitLines :: String -> [DigitLines]
makeDigitLines = transpose . map (chunksOf 3) . lines

ocrMap :: M.Map String Integer
ocrMap = M.fromList $ (`zip` [0..9]) . map concat . makeDigitLines $
         " _     _  _     _  _  _  _  _ \n\ 
         \| |  | _| _||_||_ |_   ||_||_|\n\ 
         \|_|  ||_  _|  | _||_|  ||_| _|\n"   
