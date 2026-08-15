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
    , mathFnTests
    , stringFnTests
    , extendedAggregateTests
    , adjustRefsTests
    , roundTripTests
    , prefixCollisionTests
    , randTests
    , unquotedStringTests
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

boolAt :: (Int, Int) -> Bool -> ((Int, Int), Expr)
boolAt pos b = (pos, Expr (BoolLitF b))

-- | A cell whose formula evaluates to a 'VErr', for testing error propagation.
errAt :: (Int, Int) -> ((Int, Int), Expr)
errAt pos = (pos, Expr (ToNumberF (Expr (StrLitF "abc"))))

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

mathFnTests :: TestTree
mathFnTests =
  testGroup
    "math built-ins"
    [ testCase "ABS" $ eval "ABS(-5)" @?= VNum 5
    , testCase "SQRT" $ eval "SQRT(16)" @?= VNum 4
    , testCase "SQRT of a negative number is an error" $
        assertBool "expected a VErr" (isErr (eval "SQRT(-1)"))
    , testCase "LOG is base 10" $ eval "LOG(100)" @?= VNum 2
    , testCase "LOG of a non-positive number is an error" $
        assertBool "expected a VErr" (isErr (eval "LOG(-1)"))
    , testCase "LN" $ eval "LN(1)" @?= VNum 0
    , testCase "EXP" $ eval "EXP(0)" @?= VNum 1
    , testCase "SIGN" $ eval "SIGN(-7)" @?= VNum (-1)
    , testCase "INT floors toward negative infinity" $ eval "INT(-3.5)" @?= VNum (-4)
    , testCase "TRUNC truncates toward zero" $ eval "TRUNC(-3.5)" @?= VNum (-3)
    , testCase "CEILING" $ eval "CEILING(3.2)" @?= VNum 4
    , testCase "FLOOR" $ eval "FLOOR(3.8)" @?= VNum 3
    , testCase "MOD" $ eval "MOD(7,3)" @?= VNum 1
    , testCase "MOD's sign follows the divisor" $ eval "MOD(-7,3)" @?= VNum 2
    , testCase "MOD by zero is an error" $ eval "MOD(7,0)" @?= VErr "#DIV/0!"
    , testCase "POWER" $ eval "POWER(2,10)" @?= VNum 1024
    , testCase "ROUND is half-away-from-zero, not half-to-even" $
        eval "ROUND(2.5,0)" @?= VNum 3
    , testCase "ROUND away from zero on the negative side too" $
        eval "ROUND(-2.5,0)" @?= VNum (-3)
    , testCase "ROUNDUP always rounds away from zero" $ eval "ROUNDUP(2.1,0)" @?= VNum 3
    , testCase "ROUNDDOWN always truncates toward zero" $
        eval "ROUNDDOWN(2.9,0)" @?= VNum 2
    ]

stringFnTests :: TestTree
stringFnTests =
  testGroup
    "string built-ins"
    [ testCase "LEN" $ eval "LEN(\"hello\")" @?= VNum 5
    , testCase "UPPER" $ eval "UPPER(\"hello\")" @?= VStr "HELLO"
    , testCase "LOWER" $ eval "LOWER(\"HELLO\")" @?= VStr "hello"
    , testCase "TRIM strips ends and collapses internal runs" $
        eval "TRIM(\"  a   b  \")" @?= VStr "a b"
    , testCase "LEFT" $ eval "LEFT(\"hello\",3)" @?= VStr "hel"
    , testCase "RIGHT" $ eval "RIGHT(\"hello\",3)" @?= VStr "llo"
    , testCase "MID" $ eval "MID(\"hello\",2,3)" @?= VStr "ell"
    , testCase "FIND returns a 1-based position" $
        eval "FIND(\"lo\",\"hello\")" @?= VNum 4
    , testCase "FIND is an error when not found" $
        assertBool "expected a VErr" (isErr (eval "FIND(\"xyz\",\"hello\")"))
    , testCase "SUBSTITUTE replaces every occurrence" $
        eval "SUBSTITUTE(\"hello world\",\"o\",\"0\")" @?= VStr "hell0 w0rld"
    , testCase "REPT" $ eval "REPT(\"ab\",3)" @?= VStr "ababab"
    ]

extendedAggregateTests :: TestTree
extendedAggregateTests =
  testGroup
    "extended aggregates"
    [ testCase "PRODUCT" $ evalFormula grid "PRODUCT(@0,0:3,0)" @?= VNum 24
    , testCase "PRODUCT of an all-blank range is 1" $
        evalFormula grid "PRODUCT(@9,9:10,10)" @?= VNum 1
    , testCase "MEDIAN of an even count averages the middle two" $
        evalFormula grid "MEDIAN(@0,0:3,0)" @?= VNum 2.5
    , testCase "MEDIAN of an empty range is an error" $
        assertBool "expected a VErr" (isErr (evalFormula grid "MEDIAN(@9,9:10,10)"))
    , testCase "VAR" $ evalFormula grid "VAR(@0,0:3,0)" @?= VNum 1.25
    , testCase "VAR of an empty range is an error" $
        assertBool "expected a VErr" (isErr (evalFormula grid "VAR(@9,9:10,10)"))
    , testCase "STDEV is the square root of VAR" $
        evalFormula grid "STDEV(@0,0:3,0)" @?= VNum (sqrt 1.25)
    , testCase "COUNTA counts non-blank cells of any type" $
        evalFormula mixedTypeGrid "COUNTA(@0,0:3,0)" @?= VNum 3
    , testCase "COUNTA still propagates a VErr" $
        assertBool "expected a VErr" (isErr (evalFormula errGrid "COUNTA(@0,0:1,0)"))
    ]
 where
  grid = Map.fromList [numAt (0, 0) 1, numAt (1, 0) 2, numAt (2, 0) 3, numAt (3, 0) 4]
  mixedTypeGrid = Map.fromList [numAt (0, 0) 1, strAt (1, 0) "x", boolAt (2, 0) True]
  errGrid = Map.fromList [numAt (0, 0) 1, errAt (1, 0)]

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
    , "ROUND(2.5,0)"
    , "MID(\"hello\",2,3)"
    , "SUBSTITUTE(\"a\",\"b\",\"c\")"
    ]
  roundTrips src = case parseExpr src of
    Left err -> assertBool ("failed to parse " ++ src ++ ": " ++ err) False
    Right expr1 -> case parseExpr (renderExpr expr1) of
      Left err -> assertBool ("re-parsing rendered " ++ src ++ " failed: " ++ err) False
      Right expr2 -> expr1 @?= expr2

{- | 'COUNT' is a strict prefix of 'COUNTA', and 'ROUND' of 'ROUNDUP'\/
'ROUNDDOWN' - confirms attoparsec's backtracking resolves these
correctly regardless of 'callP's own ordering (see "Parser"'s [^2]).
-}
prefixCollisionTests :: TestTree
prefixCollisionTests =
  testGroup
    "prefix-colliding built-in names"
    [ testCase "COUNTA doesn't get cut short as COUNT" $
        evalFormula (Map.fromList [strAt (0, 0) "x"]) "COUNTA(@0,0)" @?= VNum 1
    , testCase "ROUNDUP doesn't get cut short as ROUND" $
        eval "ROUNDUP(2.1,0)" @?= VNum 3
    , testCase "ROUNDDOWN doesn't get cut short as ROUND" $
        eval "ROUNDDOWN(2.9,0)" @?= VNum 2
    ]

randTests :: TestTree
randTests =
  testGroup
    "RAND"
    [ testCase "parses and produces a VNum" $
        case eval "RAND()" of
          VNum _ -> pure ()
          other -> assertBool ("expected VNum, got " ++ show other) False
    , testCase "value is in [0,1)" $
        case eval "RAND()" of
          VNum n -> assertBool "RAND should be in [0,1)" (n >= 0 && n < 1)
          other -> assertBool ("expected VNum, got " ++ show other) False
    , testCase "different cells get different values" $
        let v1 = compile (0, 0) (Expr RandF) (evaluated Map.empty)
            v2 = compile (1, 0) (Expr RandF) (evaluated Map.empty)
         in assertBool "different cells should produce different values" (v1 /= v2)
    , testCase "round-trips through renderExpr" $
        case parseExpr "RAND()" of
          Left err -> assertBool ("failed to parse: " ++ err) False
          Right expr -> case parseExpr (renderExpr expr) of
            Left err -> assertBool ("re-parse failed: " ++ err) False
            Right expr2 -> expr @?= expr2
    ]

unquotedStringTests :: TestTree
unquotedStringTests =
  testGroup
    "unquoted strings"
    [ testCase "a bare word parses as a string" $ eval "hello" @?= VStr "hello"
    , testCase "mixed-case bare word" $ eval "Hello" @?= VStr "Hello"
    , testCase "bare word with digits" $ eval "foo123" @?= VStr "foo123"
    , testCase "bare word with underscore" $ eval "my_var" @?= VStr "my_var"
    , testCase "single letter" $ eval "a" @?= VStr "a"
    , testCase "a word that looks like a function name but isn't called" $
        eval "SUM" @?= VStr "SUM"
    , testCase "a word that is a prefix of a function name" $
        eval "SUMX" @?= VStr "SUMX"
    , testCase "bare word in concatenation" $
        eval "hello&\" world\"" @?= VStr "hello world"
    , testCase "bare word in comparison" $
        eval "hello=\"hello\"" @?= VBool True
    , testCase "bare word in IF" $
        eval "IF(TRUE,hello,\"no\")" @?= VStr "hello"
    , testCase "bare word in STR" $
        eval "STR(hello)" @?= VStr "hello"
    , testCase "bare word in NUM is an error" $
        assertBool "expected a VErr" (isErr (eval "NUM(hello)"))
    , testCase "existing function calls still work" $
        eval "ABS(-5)" @?= VNum 5
    , testCase "existing booleans still work" $
        eval "TRUE" @?= VBool True
    , testCase "existing cell refs still work" $
        evalFormula (Map.fromList [numAt (1, 2) 42]) "@1,2" @?= VNum 42
    , testCase "existing quoted strings still work" $
        eval "\"hello\"" @?= VStr "hello"
    , testCase "bare word followed by operator is a parse error" $
        case parseExpr "hello world" of
          Left _ -> pure ()
          Right e -> assertBool ("expected parse failure, got " ++ show e) False
    ]

{- [^1]:
Regression test for a mid-epic-1 correction: an earlier version of the
design required both branches to share a type, which would have meant
forcing both - breaking the division-by-zero guard idiom below. This
should return the taken branch's value, not VErr.
-}
