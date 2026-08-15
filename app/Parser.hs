{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Parser
Description: Turns what a user types into a cell into an 'Expr'.

Grammar, loosest to tightest binding:

@
expr     ::= compare
compare  ::= additive (('=' | '<>' | '<=' | '>=' | '<' | '>') additive)?
additive ::= term (('+' | '-' | '&') term)*
term     ::= factor (('*' | '/') factor)*
factor   ::= number | string | bool | ref | call | '(' expr ')' | '-' factor | bareword
bareword ::= [a-zA-Z_][a-zA-Z0-9_]*   -- unquoted string literal
ref      ::= '\@' int ',' int
call     ::= 'IF' '(' expr ',' expr ',' expr ')'
           | 'RAND' '(' ')'
           | 'AND' '(' expr ',' expr ')' | 'OR' '(' expr ',' expr ')'
           | 'NOT' '(' expr ')' | 'STR' '(' expr ')' | 'NUM' '(' expr ')'
           | fn1 '(' expr ')' | fn2 '(' expr ',' expr ')'
           | fn3 '(' expr ',' expr ',' expr ')'
           | agg '(' range ')'
fn1      ::= 'ABS' | 'SQRT' | 'LOG' | 'LN' | 'EXP' | 'SIGN' | 'INT' | 'TRUNC'
           | 'CEILING' | 'FLOOR' | 'LEN' | 'UPPER' | 'LOWER' | 'TRIM'
fn2      ::= 'MOD' | 'POWER' | 'ROUND' | 'ROUNDUP' | 'ROUNDDOWN'
           | 'LEFT' | 'RIGHT' | 'FIND' | 'REPT'
fn3      ::= 'MID' | 'SUBSTITUTE'
agg      ::= 'SUM' | 'AVERAGE' | 'COUNT' | 'COUNTA' | 'MIN' | 'MAX'
           | 'PRODUCT' | 'MEDIAN' | 'VAR' | 'STDEV'
range    ::= '\@' int ',' int (':' int ',' int)?
@

A cell reference is written as an absolute coordinate, e.g. @\@2,3@ means
"cell (2,3)", regardless of which cell the formula itself lives in.
Comparisons don't chain (@1<2<3@ isn't valid syntax). A range is only
ever valid as an aggregate function's own argument, never a general
expression - @SUM(\@0,0:3,2)@ parses, @\@0,0:3,2 + 1@ doesn't. [^1] [^2]
-}
module Parser (parseExpr) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T
import Formula (
  AggOp (..),
  CompareOp (..),
  Expr (..),
  ExprF (..),
  Fn1Op (..),
  Fn2Op (..),
  Fn3Op (..),
  Op (..),
 )

parseExpr :: String -> Either String Expr
parseExpr =
  parseOnly (skipSpace *> compareP <* skipSpace <* endOfInput) . T.pack

compareP :: Parser Expr
compareP = do
  a <- additiveP
  ( do
      skipSpace
      op <- compareOpP
      skipSpace
      Expr . CompareF op a <$> additiveP
    )
    <|> pure a

-- | Longest-match-first so e.g. @<>@ doesn't get consumed as a bare @<@.
compareOpP :: Parser CompareOp
compareOpP =
  choice
    [ CNeq <$ string "<>"
    , CLte <$ string "<="
    , CGte <$ string ">="
    , CLt <$ string "<"
    , CGt <$ string ">"
    , CEq <$ string "="
    ]

additiveP :: Parser Expr
additiveP = termP >>= rest
 where
  rest acc =
    ( do
        skipSpace
        ctor <-
          ((\a b -> Expr (ArithF Add a b)) <$ char '+')
            <|> ((\a b -> Expr (ArithF Sub a b)) <$ char '-')
            <|> ((\a b -> Expr (ConcatF a b)) <$ char '&')
        skipSpace
        t <- termP
        rest (ctor acc t)
    )
      <|> pure acc

termP :: Parser Expr
termP = factorP >>= rest
 where
  rest acc =
    ( do
        skipSpace
        op <- (Mul <$ char '*') <|> (Div <$ char '/')
        skipSpace
        f <- factorP
        rest (Expr (ArithF op acc f))
    )
      <|> pure acc

factorP :: Parser Expr
factorP = skipSpace *> choice [refP, callP, boolP, stringP, negP, parensP, litP, bareWordP]

litP :: Parser Expr
litP = Expr . NumLitF <$> double

stringP :: Parser Expr
stringP = Expr . StrLitF <$> (char '"' *> many' chunk <* char '"')
 where
  chunk = (char '\\' *> char '"') <|> satisfy (/= '"')

boolP :: Parser Expr
boolP =
  (Expr (BoolLitF True) <$ string "TRUE")
    <|> (Expr (BoolLitF False) <$ string "FALSE")

negP :: Parser Expr
negP = char '-' *> skipSpace *> (asNegative <$> factorP)
 where
  asNegative (Expr (NumLitF n)) = Expr (NumLitF (negate n))
  asNegative e = Expr (ArithF Sub (Expr (NumLitF 0)) e)

{- | A bare word (identifier-like token) that didn't match any known
function, keyword, or cell reference - parsed as an unquoted string
literal. Placed last in 'factorP' so all other atoms take priority.
-}
bareWordP :: Parser Expr
bareWordP = Expr . StrLitF <$> word
 where
  word = (:) <$> (satisfy isAlpha <|> char '_') <*> many (satisfy isAlphaNum <|> char '_')

-- | The @x, y@ pair inside a '@x,y' reference, shared with 'rangeP'.
coordP :: Parser (Int, Int)
coordP = do
  x <- signed decimal
  skipSpace
  _ <- char ','
  skipSpace
  y <- signed decimal
  pure (x, y)

refP :: Parser Expr
refP = do
  _ <- char '@'
  (x, y) <- coordP
  pure (Expr (RefF (x, y)))

{- | An aggregate function's argument: a bare '\@x,y' (its own 1x1
range) or an explicit '\@x,y:x2,y2' rectangle, corners in either order.
-}
rangeP :: Parser ((Int, Int), (Int, Int))
rangeP = do
  _ <- char '@'
  start <- coordP
  end <- endP <|> pure start
  pure (start, end)
 where
  endP = do
    skipSpace
    _ <- char ':'
    skipSpace
    coordP

parensP :: Parser Expr
parensP = char '(' *> skipSpace *> compareP <* skipSpace <* char ')'

callP :: Parser Expr
callP =
  choice
    [ randP
    , ifP
    , andP
    , orP
    , notP
    , strP
    , numP
    , sumP
    , avgP
    , countaP
    , countP
    , minP
    , maxP
    , productP
    , medianP
    , varP
    , stdevP
    , absP
    , sqrtP
    , logP
    , lnP
    , expP
    , signP
    , truncP
    , intP
    , ceilingP
    , floorP
    , lenP
    , upperP
    , lowerP
    , trimP
    , modP
    , powerP
    , roundUpP
    , roundDownP
    , roundP
    , leftP
    , rightP
    , findP
    , reptP
    , midP
    , substituteP
    ]

randP :: Parser Expr
randP = string "RAND" *> skipSpace *> char '(' *> skipSpace *> char ')' *> pure (Expr RandF)

ifP :: Parser Expr
ifP = do
  _ <- string "IF"
  c <- arg1
  _ <- comma
  t <- arg
  _ <- comma
  Expr . IfF c t <$> argClose

andP, orP :: Parser Expr
andP = mk2 "AND" (\a b -> Expr (AndF a b))
orP = mk2 "OR" (\a b -> Expr (OrF a b))

notP, strP, numP :: Parser Expr
notP = mk1 "NOT" (Expr . NotF)
strP = mk1 "STR" (Expr . ToStringF)
numP = mk1 "NUM" (Expr . ToNumberF)

sumP
  , avgP
  , countP
  , minP
  , maxP
  , productP
  , medianP
  , varP
  , stdevP
  , countaP ::
    Parser Expr
sumP = aggCallP "SUM" SumOp
avgP = aggCallP "AVERAGE" AvgOp
countP = aggCallP "COUNT" CountOp
minP = aggCallP "MIN" MinOp
maxP = aggCallP "MAX" MaxOp
productP = aggCallP "PRODUCT" ProductOp
medianP = aggCallP "MEDIAN" MedianOp
varP = aggCallP "VAR" VarOp
stdevP = aggCallP "STDEV" StdevOp
-- \| Ordered before 'countP' in 'callP' - "COUNT" is a strict prefix of
-- "COUNTA", though attoparsec's backtracking means the order doesn't
-- actually matter (see [^2]).
countaP = aggCallP "COUNTA" CountAOp

-- | An aggregate-function call: @NAME(range)@, fixed single-range arity.
aggCallP :: T.Text -> AggOp -> Parser Expr
aggCallP name op = do
  _ <- string name
  skipSpace
  _ <- char '('
  skipSpace
  (start, end) <- rangeP
  skipSpace
  _ <- char ')'
  pure (Expr (RangeF op start end))

-- | The fourteen one-argument math\/string built-ins: @NAME(expr)@.
absP
  , sqrtP
  , logP
  , lnP
  , expP
  , signP
  , intP
  , truncP
  , ceilingP
  , floorP ::
    Parser Expr
lenP, upperP, lowerP, trimP :: Parser Expr
absP = mk1 "ABS" (Expr . Call1F AbsOp)
sqrtP = mk1 "SQRT" (Expr . Call1F SqrtOp)
logP = mk1 "LOG" (Expr . Call1F LogOp)
lnP = mk1 "LN" (Expr . Call1F LnOp)
expP = mk1 "EXP" (Expr . Call1F ExpOp)
signP = mk1 "SIGN" (Expr . Call1F SignOp)
intP = mk1 "INT" (Expr . Call1F IntOp)
truncP = mk1 "TRUNC" (Expr . Call1F TruncOp)
ceilingP = mk1 "CEILING" (Expr . Call1F CeilingOp)
floorP = mk1 "FLOOR" (Expr . Call1F FloorOp)

lenP = mk1 "LEN" (Expr . Call1F LenOp)
upperP = mk1 "UPPER" (Expr . Call1F UpperOp)
lowerP = mk1 "LOWER" (Expr . Call1F LowerOp)
trimP = mk1 "TRIM" (Expr . Call1F TrimOp)

-- | The nine two-argument math\/string built-ins: @NAME(expr, expr)@.
modP, powerP, roundP, roundUpP, roundDownP :: Parser Expr
leftP, rightP, findP, reptP :: Parser Expr
modP = mk2 "MOD" (\a b -> Expr (Call2F ModOp a b))
powerP = mk2 "POWER" (\a b -> Expr (Call2F PowerOp a b))
-- \| Ordered before 'roundP' in 'callP' - "ROUND" is a strict prefix of
-- both, same reasoning as 'countaP' above.
roundUpP = mk2 "ROUNDUP" (\a b -> Expr (Call2F RoundUpOp a b))
roundDownP = mk2 "ROUNDDOWN" (\a b -> Expr (Call2F RoundDownOp a b))
roundP = mk2 "ROUND" (\a b -> Expr (Call2F RoundOp a b))

leftP = mk2 "LEFT" (\a b -> Expr (Call2F LeftOp a b))
rightP = mk2 "RIGHT" (\a b -> Expr (Call2F RightOp a b))
findP = mk2 "FIND" (\a b -> Expr (Call2F FindOp a b))
reptP = mk2 "REPT" (\a b -> Expr (Call2F ReptOp a b))

-- | The two three-argument string built-ins: @NAME(expr, expr, expr)@.
midP, substituteP :: Parser Expr
midP = mk3 "MID" (\a b c -> Expr (Call3F MidOp a b c))
substituteP = mk3 "SUBSTITUTE" (\a b c -> Expr (Call3F SubstituteOp a b c))

-- | A one-argument built-in: @NAME(expr)@.
mk1 :: T.Text -> (Expr -> Expr) -> Parser Expr
mk1 name build = build <$> (string name *> arg1Close)

-- | A two-argument built-in: @NAME(expr, expr)@.
mk2 :: T.Text -> (Expr -> Expr -> Expr) -> Parser Expr
mk2 name build = do
  _ <- string name
  a <- arg1
  _ <- comma
  build a <$> argClose

-- | A three-argument built-in: @NAME(expr, expr, expr)@.
mk3 :: T.Text -> (Expr -> Expr -> Expr -> Expr) -> Parser Expr
mk3 name build = do
  _ <- string name
  a <- arg1
  _ <- comma
  b <- arg
  _ <- comma
  build a b <$> argClose

comma :: Parser ()
comma = skipSpace *> char ',' *> skipSpace

-- | The first argument of a call, right after its opening paren.
arg1 :: Parser Expr
arg1 = skipSpace *> char '(' *> skipSpace *> compareP <* skipSpace

-- | A single-argument call's only argument, through the closing paren.
arg1Close :: Parser Expr
arg1Close = arg1 <* char ')'

-- | A non-final argument, with no paren on either side.
arg :: Parser Expr
arg = compareP <* skipSpace

-- | A call's final argument, through the closing paren.
argClose :: Parser Expr
argClose = compareP <* skipSpace <* char ')'

{- [^1]:
A comparison produces a boolean, which has no meaningful further
comparison, so chaining would just be a footgun rather than a useful
shorthand.
-}

{- [^2]:
Each built-in is its own grammar rule with a fixed argument count, not one
generic @name(args...)@ rule - arity is a fact the grammar enforces, so a
malformed call like @IF(a,b)@ just doesn't parse.
-}
