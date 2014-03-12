import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

import KataBankOCR (Account, parseAccount, createAccount)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList kataBankOCRTests ]

acct :: String -> Account
acct = createAccount

kataBankOCRTests :: [Test]
kataBankOCRTests =
  [testCase "use case 1" $ do   
   acct "000000000" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\
\| || || || || || || || || |\n\
\|_||_||_||_||_||_||_||_||_|\n"
   acct "111111111" @=? parseAccount "\
\                           \n\
\  |  |  |  |  |  |  |  |  |\n\
\  |  |  |  |  |  |  |  |  |\n"

   acct "222222222" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\ _| _| _| _| _| _| _| _| _|\n\ 
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n"   

   acct "333333333" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\ _| _| _| _| _| _| _| _| _|\n\ 
\ _| _| _| _| _| _| _| _| _|\n"   

   acct "444444444" @=? parseAccount "\
\                           \n\ 
\|_||_||_||_||_||_||_||_||_|\n\ 
\  |  |  |  |  |  |  |  |  |\n"   

   acct "555555555" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n\ 
\ _| _| _| _| _| _| _| _| _|\n"   

   acct "666666666" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\|_ |_ |_ |_ |_ |_ |_ |_ |_ \n\ 
\|_||_||_||_||_||_||_||_||_|\n"   

   acct "777777777" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\  |  |  |  |  |  |  |  |  |\n\ 
\  |  |  |  |  |  |  |  |  |\n"   

   acct "888888888" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\|_||_||_||_||_||_||_||_||_|\n\ 
\|_||_||_||_||_||_||_||_||_|\n"   

   acct "999999999" @=? parseAccount "\
\ _  _  _  _  _  _  _  _  _ \n\ 
\|_||_||_||_||_||_||_||_||_|\n\ 
\ _| _| _| _| _| _| _| _| _|\n"   

   acct "123456789" @=? parseAccount "\
\    _  _     _  _  _  _  _ \n\ 
\  | _| _||_||_ |_   ||_||_|\n\ 
\  ||_  _|  | _||_|  ||_| _|\n"   
  ]
