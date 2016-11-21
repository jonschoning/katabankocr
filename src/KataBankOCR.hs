module KataBankOCR
  ( Status(..)
  , Account
  , pretty
  , createAccount
  , parseAccountOnly
  , parseAccount
  , isStatusOk
  , guessIfNotOK
  , guess
  ) where

import Data.Char (digitToInt, isDigit)
import Data.List (transpose, delete, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

data Account = Acct
  { acctnum :: AccountNum
  , status :: Status
  , ambiguousAcctNums :: [AccountNum]
  } deriving (Eq, Show)

data Status
  = OK
  | BadChecksum
  | Illegible
  | Ambiguous
  deriving (Eq, Show, Enum)

type AccountNum = String

type Digit = String

type AccountWithDigits = (Account, [Digit])

illegibleSymbol :: Char
illegibleSymbol = '?'

-- create
createAccount :: AccountNum -> Account
createAccount num =
  Acct
  { acctnum = num
  , status = calculateStatus num
  , ambiguousAcctNums = []
  }

pretty :: Account -> String
pretty a = acctnum a ++ prettystatus a ++ prettyacctnums a
  where
    prettystatus Acct {status = OK} = ""
    prettystatus Acct {status = Illegible} = " ILL"
    prettystatus Acct {status = Ambiguous} = " AMB"
    prettystatus Acct {status = BadChecksum} = " ERR"
    prettyacctnums Acct {ambiguousAcctNums = []} = ""
    prettyacctnums Acct {ambiguousAcctNums = s} = ' ' : show s

guessIfNotOK :: AccountWithDigits -> Account
guessIfNotOK awd@(acct, _) =
  if isStatusOk acct
    then acct
    else guess awd

-- tries to find a (unique) correct account number
guess :: AccountWithDigits -> Account
guess awd@(acct, _) =
  let guesses = generateGuesses awd
  in case guesses of
       [] ->
         acct
         { status = Illegible
         }
       [one] -> one
       _ ->
         acct
         { status = Ambiguous
         , ambiguousAcctNums = sort $ acctnum <$> guesses
         }

generateGuesses :: AccountWithDigits -> [Account]
generateGuesses (Acct num _ _, digits) =
  [ acct'
  | (i, oldDigit) <- zip [0 ..] digits 
  , Just integer' <- parseDigit <$> genNewDigits oldDigit 
  , let acctNum' = updateListAt num i (showInt integer') 
  , let acct' = createAccount acctNum' 
  , isStatusOk acct' ]
  where
    genNewDigits :: Digit -> [Digit]
    genNewDigits digitChars =
      [ updateListAt digitChars i newChar
      | (i, oldChar) <- zip [0 ..] digitChars 
      , newChar <- delete oldChar " |_" ]
    showInt = head . show

updateListAt :: [a] -> Int -> a -> [a]
updateListAt xs i x' = take i xs ++ [x'] ++ drop (i + 1) xs

{-# INLINE updateListAt #-}

-- status
calculateStatus :: AccountNum -> Status
calculateStatus s
  | illegibleSymbol `elem` s = Illegible
  | isChecksumValid s = OK
  | otherwise = BadChecksum

isStatusOk :: Account -> Bool
isStatusOk (Acct _ OK _) = True
isStatusOk _ = False

-- checksum
isChecksumValid :: AccountNum -> Bool
isChecksumValid a
  | any (not . isDigit) a = False
  | otherwise = 0 == checksum a

checksum :: AccountNum -> Int
checksum = (`mod` 11) . sum . zipWith (*) [9,8 .. 1] . map digitToInt

-- parsing
parseAccountOnly :: [String] -> Account
parseAccountOnly = fst . parseAccount

parseAccount :: [String] -> AccountWithDigits
parseAccount = createAccountFromDigits . makeDigitsFromStrings

createAccountFromDigits :: [Digit] -> AccountWithDigits
createAccountFromDigits digits = (createAccount acctNum, digits)
  where
    acctNum =
      [ maybe illegibleSymbol showInt d
      | d <- parseDigit <$> digits ]
    showInt = head . show

parseDigit :: Digit -> Maybe Integer
parseDigit = flip M.lookup ocrMap

makeDigitsFromStrings :: [String] -> [Digit]
makeDigitsFromStrings = map concat . transpose . map (chunksOf 3)

ocrMap :: M.Map Digit Integer
ocrMap =
  M.fromList $
  (`zip` [0 .. 9]) . makeDigitsFromStrings $
  [ " _     _  _     _  _  _  _  _ "
  , "| |  | _| _||_||_ |_   ||_||_|"
  , "|_|  ||_  _|  | _||_|  ||_| _|"
  ]
