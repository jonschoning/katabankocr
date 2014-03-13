import KataBankOCR (parseAccountOnly, parseAccount, isValid, guess)

import Control.Exception (bracket)
import Data.List (intersperse)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do args <- getArgs
          if (length args /= 1) then error "missing input file name" else
            bracket (openFile (head args) ReadMode) hClose loop
  where 
    read4lines = sequence . replicate 4 . hGetLine
    process4Lines = (putStrLn . show . guessIfInvalid . parseAccount =<<)
    guessIfInvalid acct = if isValid (fst acct) then fst acct else guess acct
    loop handle = do
      isEof <- hIsEOF handle
      if isEof then return () else (process4Lines (read4lines handle) >> loop handle) 
