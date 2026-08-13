module FormulaSpec (tests) where

import qualified Data.Map.Strict as Map
import Formula (
  Expr (..),
  ExprF (..),
  Value (..),
  adjustRefs,
  compile,
  evaluated,
  renderExpr,
 )
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
    , rangeTests
    , adjustRefsTests
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

numAt :: (Int, Int) -> Double -> ((Int, Int), Expr)
numAt pos n = (pos, Expr (NumLitF n))

strAt :: (Int, Int) -> String -> ((Int, Int), Expr)
strAt pos s = (pos, Expr (StrLitF s))

rangeTests :: TestTree
rangeTests =
  testGroup
    "ranges"
    [ testCase "SUM over a rectangle" $
        evalFormula grid "SUM(@0,0:1,1)" @?= VNum 10
    , testCase "AVERAGE over a rectangle" $
        evalFormula grid "AVERAGE(@0,0:1,1)" @?= VNum 2.5
    , testCase "COUNT counts numeric cells" $
        evalFormula grid "COUNT(@0,0:1,1)" @?= VNum 4
    , testCase "MIN over a rectangle" $
        evalFormula grid "MIN(@0,0:1,1)" @?= VNum 1
    , testCase "MAX over a rectangle" $
        evalFormula grid "MAX(@0,0:1,1)" @?= VNum 4
    , testCase "a bare ref is its own 1x1 range" $
        evalFormula grid "SUM(@0,0)" @?= VNum 1
    , testCase "corners can be given in either order" $
        evalFormula grid "SUM(@1,1:0,0)" @?= VNum 10
    , testCase "SUM of an all-blank range is 0" $
        evalFormula grid "SUM(@9,9:10,10)" @?= VNum 0
    , testCase "COUNT of an all-blank range is 0" $
        evalFormula grid "COUNT(@9,9:10,10)" @?= VNum 0
    , testCase "AVERAGE of an empty range is an error" $
        assertBool "expected a VErr" (isErr (evalFormula grid "AVERAGE(@9,9:10,10)"))
    , testCase "MIN of an empty range is an error" $
        assertBool "expected a VErr" (isErr (evalFormula grid "MIN(@9,9:10,10)"))
    , testCase "a non-numeric cell in the range is a strict error" $
        assertBool "expected a VErr" (isErr (evalFormula mixedGrid "SUM(@0,0:1,1)"))
    , testCase "a bare ref parses the same as its own explicit self-range" $
        parseExpr "SUM(@2,3)" @?= parseExpr "SUM(@2,3:2,3)"
    ]
 where
  grid = Map.fromList [numAt (0, 0) 1, numAt (1, 0) 2, numAt (0, 1) 3, numAt (1, 1) 4]
  mixedGrid = Map.fromList [numAt (0, 0) 1, strAt (1, 0) "x", numAt (0, 1) 3, numAt (1, 1) 4]

adjustRefsTests :: TestTree
adjustRefsTests =
  testGroup
    "adjustRefs"
    [ testCase "shifts a plain ref" $
        fmap (adjustRefs (1, 1)) (parseExpr "@2,3") @?= parseExpr "@3,4"
    , testCase "shifts both corners of a range" $
        fmap (adjustRefs (2, 3)) (parseExpr "SUM(@0,0:1,1)") @?= parseExpr "SUM(@2,3:3,4)"
    , testCase "leaves a literal untouched" $
        fmap (adjustRefs (5, 5)) (parseExpr "42") @?= parseExpr "42"
    , testCase "recurses through arithmetic" $
        fmap (adjustRefs (1, 0)) (parseExpr "@0,0+@1,1") @?= parseExpr "@1,0+@2,1"
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
    , "SUM(@0,0:3,2)"
    , "AVERAGE(@0,0:3,2)"
    , "COUNT(@0,0:3,2)"
    , "MIN(@0,0:3,2)"
    , "MAX(@0,0:3,2)"
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
