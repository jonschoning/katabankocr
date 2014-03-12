module KataBankOCR (Status (..), Account, createAccount, parseAccount, isChecksumValid, withStatus) where

import Control.Applicative (liftA2)
import Data.Char (digitToInt, isDigit)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

type Account = String

type DigitLines = [String]

data Status = OK | ERR | ILL
              deriving (Eq, Show, Enum)

createAccount :: String -> Account
createAccount = id

parseAccount :: String -> Account
parseAccount = concatMap (maybe "?" show . parseAccountDigit) . makeDigitLines

parseAccountDigit :: DigitLines -> Maybe Integer
parseAccountDigit  = (flip M.lookup ocrMap) . concat

makeDigitLines :: String -> [DigitLines]
makeDigitLines = transpose . map (chunksOf 3) . lines
  
isChecksumValid :: Account -> Bool
isChecksumValid a 
  | any (not . isDigit) a = False
  | otherwise = (0 ==) . (`mod` 11) . sum . zipWith (*) [9, 8..1] . map digitToInt $ a

withStatus :: Account -> String
withStatus = liftA2 (++) id (disp . status)
  where disp OK = ""
        disp s = ' ' : show s

status :: Account -> Status
status s 
  | any (== '?') s = ILL
  | isChecksumValid s = OK
  | otherwise = ERR

ocrMap :: M.Map String Integer
ocrMap = M.fromList $ (`zip` [0..9]) . map concat . makeDigitLines $
         " _     _  _     _  _  _  _  _ \n\ 
         \| |  | _| _||_||_ |_   ||_||_|\n\ 
         \|_|  ||_  _|  | _||_|  ||_| _|\n"   
