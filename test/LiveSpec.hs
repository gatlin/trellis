module LiveSpec (tests) where

import Formula (Expr (..), ExprF (..))
import Live (LiveSpec (..), literal, parseLiveSpec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Live"
    [ parseLiveSpecTests
    , literalTests
    ]

parseLiveSpecTests :: TestTree
parseLiveSpecTests =
  testGroup
    "parseLiveSpec"
    [ testCase "a shell-interval spec" $
        parseLiveSpec "!5s echo hi" @?= Just (ShellInterval 5 "echo hi")
    , testCase "a fractional interval" $
        parseLiveSpec "!0.5s date" @?= Just (ShellInterval 0.5 "date")
    , testCase "a multi-word command" $
        parseLiveSpec "!1s echo hello world"
          @?= Just (ShellInterval 1 "echo hello world")
    , testCase "a tail spec" $
        parseLiveSpec "!tail /tmp/fifo" @?= Just (TailFile "/tmp/fifo")
    , testCase "an interval missing its 's' suffix is malformed" $
        parseLiveSpec "!5x echo hi" @?= Nothing
    , testCase "a non-numeric interval is malformed" $
        parseLiveSpec "!abcs echo hi" @?= Nothing
    , testCase "a shell spec with no command is rejected" $
        parseLiveSpec "!5s" @?= Nothing
    , testCase "a tail spec with no path is rejected" $
        parseLiveSpec "!tail" @?= Nothing
    , testCase "text with no leading ! is not a spec" $
        parseLiveSpec "hello" @?= Nothing
    , testCase "an ordinary formula is not a spec" $
        parseLiveSpec "@0,0+1" @?= Nothing
    , testCase "the empty string is not a spec" $
        parseLiveSpec "" @?= Nothing
    ]

literalTests :: TestTree
literalTests =
  testGroup
    "literal"
    [ testCase "a numeric string becomes NumLitF" $
        literal "10" @?= Expr (NumLitF 10)
    , testCase "a negative numeric string becomes NumLitF" $
        literal "-5" @?= Expr (NumLitF (-5))
    , testCase "a non-numeric string becomes StrLitF" $
        literal "hello" @?= Expr (StrLitF "hello")
    , testCase "surrounding whitespace is trimmed before classifying" $
        literal "  42  " @?= Expr (NumLitF 42)
    , testCase "the empty string becomes an empty StrLitF" $
        literal "" @?= Expr (StrLitF "")
    ]
