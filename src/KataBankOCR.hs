{-# LANGUAGE OverloadedStrings #-}

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
import Data.List (transpose, delete, sort, unfoldr)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS

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

type AccountNum = BS.ByteString

type Digit = BS.ByteString

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

pretty :: Account -> BS.ByteString
pretty a = acctnum a <> prettystatus a <> prettyacctnums a
  where
    prettystatus :: Account -> BS.ByteString
    prettystatus Acct {status = OK} = ""
    prettystatus Acct {status = Illegible} = " ILL"
    prettystatus Acct {status = Ambiguous} = " AMB"
    prettystatus Acct {status = BadChecksum} = " ERR"
    prettyacctnums :: Account -> BS.ByteString
    prettyacctnums Acct {ambiguousAcctNums = []} = ""
    prettyacctnums Acct {ambiguousAcctNums = s} = " " <> BS.pack (show s)

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
  , let acctNum' = updateBSAt num i (showInt integer') 
  , let acct' = createAccount acctNum' 
  , isStatusOk acct' ]
  where
    genNewDigits :: Digit -> [Digit]
    genNewDigits digitChars =
      [ updateBSAt digitChars i (BS.singleton newChar)
      | (i, oldChar) <- zip [0 ..] (BS.unpack digitChars) 
      , newChar <- delete oldChar " |_" ]
    showInt = BS.pack . show

updateBSAt :: BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
updateBSAt xs i x' = BS.take i xs <> x' <> BS.drop (i + 1) xs

{-# INLINE updateBSAt #-}

-- status
calculateStatus :: AccountNum -> Status
calculateStatus s
  | illegibleSymbol `BS.elem` s = Illegible
  | isChecksumValid s = OK
  | otherwise = BadChecksum

isStatusOk :: Account -> Bool
isStatusOk (Acct _ OK _) = True
isStatusOk _ = False

-- checksum
isChecksumValid :: AccountNum -> Bool
isChecksumValid a
  | BS.any (not . isDigit) a = False
  | otherwise = 0 == checksum a

checksum :: AccountNum -> Int
checksum = (`mod` 11) . sum . zipWith (*) [9,8 .. 1] . BS.foldr go []
  where
    go c acc = digitToInt c : acc

-- parsing
parseAccountOnly :: [BS.ByteString] -> Account
parseAccountOnly = fst . parseAccount

parseAccount :: [BS.ByteString] -> AccountWithDigits
parseAccount = createAccountFromDigits . makeDigitsFromStrings

createAccountFromDigits :: [Digit] -> AccountWithDigits
createAccountFromDigits digits = (createAccount acctNum, digits)
  where
    acctNum =
      BS.concat
        [ maybe (BS.singleton illegibleSymbol) showInt d
        | d <- parseDigit <$> digits ]
    showInt = BS.pack . show

parseDigit :: Digit -> Maybe Integer
parseDigit = flip M.lookup ocrMap

chunksOfBS :: Int -> BS.ByteString -> [BS.ByteString]
chunksOfBS i =
  unfoldr
    (\x ->
        if BS.null x
          then Nothing
          else Just (BS.splitAt i x))

makeDigitsFromStrings :: [BS.ByteString] -> [Digit]
makeDigitsFromStrings = fmap BS.concat . transpose . fmap (chunksOfBS 3)

ocrMap :: M.Map Digit Integer
ocrMap =
  M.fromList $
  (`zip` [0 .. 9]) . makeDigitsFromStrings $
  [ " _     _  _     _  _  _  _  _ "
  , "| |  | _| _||_||_ |_   ||_||_|"
  , "|_|  ||_  _|  | _||_|  ||_| _|"
  ]
