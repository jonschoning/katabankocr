import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

import KataBankOCR (Status (..), Account, pretty, parseAccountOnly, parseAccount, createAccount, isValid, guessIfNotOK)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList kataBankOCRTests ]

acct = createAccount
guess = guessIfNotOK

kataBankOCRTests :: [Test]
kataBankOCRTests = [

  testCase "use case 1: parse account numbers" $ do   

     acct "000000000" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "| || || || || || || || || |",
          "|_||_||_||_||_||_||_||_||_|"]

     acct "111111111" @=? parseAccountOnly
         ["                           ",
          "  |  |  |  |  |  |  |  |  |",
          "  |  |  |  |  |  |  |  |  |"]

     acct "222222222" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          " _| _| _| _| _| _| _| _| _|",
          "|_ |_ |_ |_ |_ |_ |_ |_ |_ "]

     acct "333333333" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          " _| _| _| _| _| _| _| _| _|",
          " _| _| _| _| _| _| _| _| _|"]

     acct "444444444" @=? parseAccountOnly
         ["                           ",
          "|_||_||_||_||_||_||_||_||_|",
          "  |  |  |  |  |  |  |  |  |"]

     acct "555555555" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
          " _| _| _| _| _| _| _| _| _|"]

     acct "666666666" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
          "|_||_||_||_||_||_||_||_||_|"]

     acct "777777777" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "  |  |  |  |  |  |  |  |  |",
          "  |  |  |  |  |  |  |  |  |"]

     acct "888888888" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "|_||_||_||_||_||_||_||_||_|",
          "|_||_||_||_||_||_||_||_||_|"]

     acct "999999999" @=? parseAccountOnly
         [" _  _  _  _  _  _  _  _  _ ",
          "|_||_||_||_||_||_||_||_||_|",
          " _| _| _| _| _| _| _| _| _|"]

     acct "123456789" @=? parseAccountOnly
         ["    _  _     _  _  _  _  _ ",
          "  | _| _||_||_ |_   ||_||_|",
          "  ||_  _|  | _||_|  ||_| _|"]

   , testCase "use case 2: calculate checksums" $ do    

     True  @=? isValid (acct "345882865")
     False @=? isValid (acct "3458?2865")
     False @=? isValid (acct "345882866") 

   , testCase "use case 3: show account status" $ do    

     "457508000"     @=? pretty (acct "457508000")
     "664371495 ERR" @=? pretty (acct "664371495")
     "86110??36 ILL" @=? pretty (acct "86110??36") 

   , testCase "use case 4: guess acct number" $ do    

     "711111111" @=? pretty (guess (parseAccount
         ["                           ",
          "  |  |  |  |  |  |  |  |  |",
          "  |  |  |  |  |  |  |  |  |"]))

     "777777177" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          "  |  |  |  |  |  |  |  |  |",
          "  |  |  |  |  |  |  |  |  |"]))

     "200800000" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          " _|| || || || || || || || |",
          "|_ |_||_||_||_||_||_||_||_|"]))

     "333393333" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          " _| _| _| _| _| _| _| _| _|",
          " _| _| _| _| _| _| _| _| _|"]))

     "888888888 AMB [\"888886888\",\"888888880\",\"888888988\"]" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          "|_||_||_||_||_||_||_||_||_|",
          "|_||_||_||_||_||_||_||_||_|"]))

     "555555555 AMB [\"555655555\",\"559555555\"]" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
          " _| _| _| _| _| _| _| _| _|"]))

     "666666666 AMB [\"666566666\",\"686666666\"]" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          "|_ |_ |_ |_ |_ |_ |_ |_ |_ ",
          "|_||_||_||_||_||_||_||_||_|"]))

     "999999999 AMB [\"899999999\",\"993999999\",\"999959999\"]" @=? pretty (guess (parseAccount
         [" _  _  _  _  _  _  _  _  _ ",
          "|_||_||_||_||_||_||_||_||_|",
          " _| _| _| _| _| _| _| _| _|"]))

     "490067715 AMB [\"490067115\",\"490067719\",\"490867715\"]" @=? pretty (guess (parseAccount
         ["    _  _  _  _  _  _     _ ",
          "|_||_|| || ||_   |  |  ||_ ",
          "  | _||_||_||_|  |  |  | _|"]))

     "123456789" @=? pretty (guess (parseAccount
         ["    _  _     _  _  _  _  _ ",
          " _| _| _||_||_ |_   ||_||_|",
          "  ||_  _|  | _||_|  ||_| _|"]))

     "000000051" @=? pretty (guess (parseAccount
         [" _     _  _  _  _  _  _    ",
          "| || || || || || || ||_   |",
          "|_||_||_||_||_||_||_| _|  |"]))

     "490867715" @=? pretty (guess (parseAccount
         ["    _  _  _  _  _  _     _ ",
          "|_||_|| ||_||_   |  |  | _ ",
          "  | _||_||_||_|  |  |  | _|"]))
  ]
