{-# LANGUAGE OverloadedStrings #-} 

import Test.HUnit ((@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

import KataBankOCR

main :: IO ()
main = exitProperly $ runTestTT $ TestList [TestList kataBankOCRTests]
  where
    exitProperly m = do
      counts <- m
      exitWith $
        if failures counts /= 0 || errors counts /= 0
          then ExitFailure 1
          else ExitSuccess

kataBankOCRTests :: [Test]
kataBankOCRTests =
  let 
      testCase label assertion = TestLabel label (TestCase assertion)
  in [ testCase "use case 1: parse account numbers" $
       do createAccount "000000000" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "| || || || || || || || || |"
              , "|_||_||_||_||_||_||_||_||_|"
              ]
          createAccount "111111111" @=?
            parseAccountOnly
              [ "                           "
              , "  |  |  |  |  |  |  |  |  |"
              , "  |  |  |  |  |  |  |  |  |"
              ]
          createAccount "222222222" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , " _| _| _| _| _| _| _| _| _|"
              , "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
              ]
          createAccount "333333333" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , " _| _| _| _| _| _| _| _| _|"
              , " _| _| _| _| _| _| _| _| _|"
              ]
          createAccount "444444444" @=?
            parseAccountOnly
              [ "                           "
              , "|_||_||_||_||_||_||_||_||_|"
              , "  |  |  |  |  |  |  |  |  |"
              ]
          createAccount "555555555" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
              , " _| _| _| _| _| _| _| _| _|"
              ]
          createAccount "666666666" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
              , "|_||_||_||_||_||_||_||_||_|"
              ]
          createAccount "777777777" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "  |  |  |  |  |  |  |  |  |"
              , "  |  |  |  |  |  |  |  |  |"
              ]
          createAccount "888888888" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "|_||_||_||_||_||_||_||_||_|"
              , "|_||_||_||_||_||_||_||_||_|"
              ]
          createAccount "999999999" @=?
            parseAccountOnly
              [ " _  _  _  _  _  _  _  _  _ "
              , "|_||_||_||_||_||_||_||_||_|"
              , " _| _| _| _| _| _| _| _| _|"
              ]
          createAccount "123456789" @=?
            parseAccountOnly
              [ "    _  _     _  _  _  _  _ "
              , "  | _| _||_||_ |_   ||_||_|"
              , "  ||_  _|  | _||_|  ||_| _|"
              ]
     , testCase "use case 2: calculate checksums" $
       do True @=? isStatusOk (createAccount "345882865")
          False @=? isStatusOk (createAccount "3458?2865")
          False @=? isStatusOk (createAccount "345882866")
     , testCase "use case 2: show account status" $
       do "457508000" @=? pretty (createAccount "457508000")
          "664371495 ERR" @=? pretty (createAccount "664371495")
          "86110??36 ILL" @=? pretty (createAccount "86110??36")
     , testCase "use case 4: guessIfNotOK createAccount number" $
       do "711111111" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ "                           "
                    , "  |  |  |  |  |  |  |  |  |"
                    , "  |  |  |  |  |  |  |  |  |"
                    ]))
          "777777177" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , "  |  |  |  |  |  |  |  |  |"
                    , "  |  |  |  |  |  |  |  |  |"
                    ]))
          "200800000" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , " _|| || || || || || || || |"
                    , "|_ |_||_||_||_||_||_||_||_|"
                    ]))
          "333393333" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , " _| _| _| _| _| _| _| _| _|"
                    , " _| _| _| _| _| _| _| _| _|"
                    ]))
          "888888888 AMB [\"888886888\",\"888888880\",\"888888988\"]" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , "|_||_||_||_||_||_||_||_||_|"
                    , "|_||_||_||_||_||_||_||_||_|"
                    ]))
          "555555555 AMB [\"555655555\",\"559555555\"]" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                    , " _| _| _| _| _| _| _| _| _|"
                    ]))
          "666666666 AMB [\"666566666\",\"686666666\"]" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , "|_ |_ |_ |_ |_ |_ |_ |_ |_ "
                    , "|_||_||_||_||_||_||_||_||_|"
                    ]))
          "999999999 AMB [\"899999999\",\"993999999\",\"999959999\"]" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _  _  _  _  _  _  _  _  _ "
                    , "|_||_||_||_||_||_||_||_||_|"
                    , " _| _| _| _| _| _| _| _| _|"
                    ]))
          "490067715 AMB [\"490067115\",\"490067719\",\"490867715\"]" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ "    _  _  _  _  _  _     _ "
                    , "|_||_|| || ||_   |  |  ||_ "
                    , "  | _||_||_||_|  |  |  | _|"
                    ]))
          "123456789" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ "    _  _     _  _  _  _  _ "
                    , " _| _| _||_||_ |_   ||_||_|"
                    , "  ||_  _|  | _||_|  ||_| _|"
                    ]))
          "000000051" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ " _     _  _  _  _  _  _    "
                    , "| || || || || || || ||_   |"
                    , "|_||_||_||_||_||_||_| _|  |"
                    ]))
          "490867715" @=?
            pretty
              (guessIfNotOK
                 (parseAccount
                    [ "    _  _  _  _  _  _     _ "
                    , "|_||_|| ||_||_   |  |  | _ "
                    , "  | _||_||_||_|  |  |  | _|"
                    ]))
     ]
