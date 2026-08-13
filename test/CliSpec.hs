module CliSpec (tests) where

import Cli (CliOptions (..), parseArgs, parseCoord, parseCoordPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Cli"
    [ parseCoordTests
    , parseCoordPathTests
    , parseArgsTests
    ]

parseCoordTests :: TestTree
parseCoordTests =
  testGroup
    "parseCoord"
    [ testCase "a valid coordinate" $ parseCoord "2,3" @?= Just (2, 3)
    , testCase "negative components are valid" $ parseCoord "-1,0" @?= Just (-1, 0)
    , testCase "missing comma is malformed" $ parseCoord "23" @?= Nothing
    , testCase "non-numeric components are malformed" $ parseCoord "a,b" @?= Nothing
    , testCase "trailing junk after a component is malformed" $
        parseCoord "2x,3" @?= Nothing
    ]

parseCoordPathTests :: TestTree
parseCoordPathTests =
  testGroup
    "parseCoordPath"
    [ testCase "a valid coord=path" $
        parseCoordPath "0,0=/tmp/pipe" @?= Just ((0, 0), "/tmp/pipe")
    , testCase "a path containing its own '=' is kept whole" $
        parseCoordPath "1,2=/tmp/a=b" @?= Just ((1, 2), "/tmp/a=b")
    , testCase "missing '=' is malformed" $ parseCoordPath "0,0/tmp/pipe" @?= Nothing
    , testCase "an empty path is malformed" $ parseCoordPath "0,0=" @?= Nothing
    , testCase "a malformed coordinate is malformed" $
        parseCoordPath "x,y=/tmp/pipe" @?= Nothing
    ]

parseArgsTests :: TestTree
parseArgsTests =
  testGroup
    "parseArgs"
    [ testCase "empty argv" $ parseArgs [] @?= Right (CliOptions [] [])
    , testCase "a single --in" $
        parseArgs ["--in", "0,0=/tmp/in"]
          @?= Right (CliOptions [((0, 0), "/tmp/in")] [])
    , testCase "a single --out" $
        parseArgs ["--out", "1,0=/tmp/out"]
          @?= Right (CliOptions [] [((1, 0), "/tmp/out")])
    , testCase "repeated --in and --out together" $
        parseArgs
          ["--in", "0,0=/tmp/a", "--out", "1,0=/tmp/b", "--in", "0,1=/tmp/c"]
          @?= Right
            ( CliOptions
                [((0, 0), "/tmp/a"), ((0, 1), "/tmp/c")]
                [((1, 0), "/tmp/b")]
            )
    , testCase "--in with no value is an error" $
        case parseArgs ["--in"] of
          Left _ -> pure ()
          Right _ -> fail "expected a parse error"
    , testCase "--out with no value is an error" $
        case parseArgs ["--out"] of
          Left _ -> pure ()
          Right _ -> fail "expected a parse error"
    , testCase "a malformed --in value is an error" $
        case parseArgs ["--in", "not-a-spec"] of
          Left _ -> pure ()
          Right _ -> fail "expected a parse error"
    , testCase "an unrecognized flag is an error" $
        case parseArgs ["--bogus"] of
          Left _ -> pure ()
          Right _ -> fail "expected a parse error"
    ]
