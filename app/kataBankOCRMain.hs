import KataBankOCR (pretty, parseAccount, guessIfNotOK)

import System.Environment (getArgs)
import Control.Monad (replicateM, unless)
import System.IO
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
    then error "missing input file name"
    else withFile (head args) ReadMode loop
  where
    get4lines = replicateM 4 . BS.hGetLine
    process4Lines = BS.putStrLn . pretty . guessIfNotOK . parseAccount
    loop handle = do
      isEof <- hIsEOF handle
      unless isEof $ get4lines handle >>= process4Lines >> loop handle
