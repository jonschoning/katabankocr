module KataBankOCR (Status (..), Account, createAccount, parseAccount, isValid, guess) where

import Control.Applicative (liftA3)
import Data.Char (digitToInt, isDigit)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

data Account = Acct { num :: AccountNum
                    , status :: Status
                    , ambs :: [AccountNum] }
               deriving (Eq)

data Status = OK | ERR | ILL | AMB
              deriving (Eq, Show, Enum)

instance Show Account where
  show = showAccount

type AccountNum = String
type DigitLines = [String]

-- create
createAccount :: AccountNum -> Account
createAccount = liftA3 Acct id initialStatus (const [])

-- tries to find a (unique) correct account number
guess :: Account -> Account
guess = id

-- status
initialStatus :: AccountNum -> Status
initialStatus s 
  | any (== '?') s = ILL
  | isChecksumValid s = OK
  | otherwise = ERR

showAccount :: Account -> String
showAccount = concat . sequence [num, shows . status, showa . ambs]
  where shows OK = ""
        shows s = ' ' : show s
        showa [] = ""
        showa s = ' ' : show s

isValid :: Account -> Bool
isValid (Acct _ OK _) = True
isValid (Acct _ _ _) = False

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
