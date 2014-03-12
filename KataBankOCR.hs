module KataBankOCR (Account, createAccount, parseAccount) where

data Account = Account
               deriving (Eq, Show)

parseAccount :: String -> Account
parseAccount = undefined

createAccount :: String -> Account
createAccount = undefined
