import KataBankOCR (pretty, parseAccount, guessIfNotOK)

import System.Environment (getArgs)
import Control.Monad (replicateM, unless)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 
    then error "missing input file name" 
    else withFile (head args) ReadMode loop
  where 
    get4lines = replicateM 4 . hGetLine
    process4Lines = putStrLn . pretty . guessIfNotOK . parseAccount
    loop handle = do
      isEof <- hIsEOF handle
      unless isEof $
        get4lines handle >>= process4Lines  >> loop handle 
