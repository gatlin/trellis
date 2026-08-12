module FormulaSpec (tests) where

import qualified Data.Map.Strict as Map
import Formula (Expr, Value (..), compile, evaluated, renderExpr)
import Parser (parseExpr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

{- | Parse and evaluate a formula against a given sheet of other cells,
as if it were sitting at (0, 0).
-}
evalFormula :: Map.Map (Int, Int) Expr -> String -> Value
evalFormula cellMap src = case parseExpr src of
  Left err -> VErr err
  Right expr -> compile (0, 0) expr (evaluated cellMap)

eval :: String -> Value
eval = evalFormula Map.empty

isErr :: Value -> Bool
isErr (VErr _) = True
isErr _ = False

tests :: TestTree
tests =
  testGroup
    "Formula"
    [ arithmeticTests
    , stringTests
    , booleanTests
    , comparisonTests
    , conversionTests
    , ifTests
    , blankTests
    , parseFailureTests
    , roundTripTests
    ]

arithmeticTests :: TestTree
arithmeticTests =
  testGroup
    "arithmetic"
    [ testCase "operator precedence" $ eval "1+2*3" @?= VNum 7
    , testCase "division by zero" $ eval "1/0" @?= VErr "#DIV/0!"
    ]

stringTests :: TestTree
stringTests =
  testGroup
    "strings"
    [testCase "concatenation" $ eval "\"foo\"&\"bar\"" @?= VStr "foobar"]

booleanTests :: TestTree
booleanTests =
  testGroup
    "booleans"
    [ testCase "literal" $ eval "TRUE" @?= VBool True
    , testCase "AND" $ eval "AND(TRUE,FALSE)" @?= VBool False
    , testCase "OR" $ eval "OR(TRUE,FALSE)" @?= VBool True
    , testCase "NOT" $ eval "NOT(TRUE)" @?= VBool False
    ]

comparisonTests :: TestTree
comparisonTests =
  testGroup
    "comparisons"
    [ testCase "numeric ordering" $ eval "1<2" @?= VBool True
    , testCase "lexicographic ordering" $ eval "\"abc\"<\"abd\"" @?= VBool True
    , testCase "cross-type comparison is an error" $
        assertBool "expected a VErr" (isErr (eval "1=\"1\""))
    ]

conversionTests :: TestTree
conversionTests =
  testGroup
    "explicit conversion"
    [ testCase "STR" $ eval "STR(5)" @?= VStr "5"
    , testCase "NUM" $ eval "NUM(\"5\")" @?= VNum 5
    , testCase "NUM on unparseable text is an error" $
        assertBool "expected a VErr" (isErr (eval "NUM(\"abc\")"))
    ]

ifTests :: TestTree
ifTests =
  testGroup
    "IF"
    [ testCase "takes the matching branch" $
        eval "IF(1<2,\"yes\",\"no\")" @?= VStr "yes"
    , testCase "is short-circuiting, not branch-type-checked" $
        -- Regression test: IF must not force the untaken branch. [^1]
        eval "IF(TRUE,1,\"no\")" @?= VNum 1
    , testCase "the untaken branch's error never happens" $
        evalFormula Map.empty "IF(@9,9=0,0,5/@9,9)" @?= VNum 0
    ]

blankTests :: TestTree
blankTests =
  testGroup
    "blank coercion"
    [testCase "a blank ref reads as zero in arithmetic" $ eval "@9,9+5" @?= VNum 5]

parseFailureTests :: TestTree
parseFailureTests =
  testGroup
    "parse failures"
    [ testCase "an incomplete expression fails to parse" $
        case parseExpr "1+" of
          Left _ -> pure ()
          Right e -> assertBool ("expected a parse failure, got " ++ show e) False
    ]

{- | Re-committing a formula unchanged must not silently change what it
means - the whole reason 'renderExpr' has to track operator precedence
rather than parenthesizing everything or nothing.
-}
roundTripTests :: TestTree
roundTripTests =
  testGroup
    "renderExpr round-trips through parseExpr"
    [testCase src (roundTrips src) | src <- cases]
 where
  cases =
    [ "1+2*3"
    , "(1+2)*3"
    , "\"foo\"&\"bar\""
    , "IF(1<2,\"yes\",\"no\")"
    , "NUM(\"5\")"
    , "-5"
    , "@1,2+@3,4"
    ]
  roundTrips src = case parseExpr src of
    Left err -> assertBool ("failed to parse " ++ src ++ ": " ++ err) False
    Right expr1 -> case parseExpr (renderExpr expr1) of
      Left err -> assertBool ("re-parsing rendered " ++ src ++ " failed: " ++ err) False
      Right expr2 -> expr1 @?= expr2

{- [^1]:
Regression test for a mid-epic-1 correction: an earlier version of the
design required both branches to share a type, which would have meant
forcing both - breaking the division-by-zero guard idiom below. This
should return the taken branch's value, not VErr.
-}
