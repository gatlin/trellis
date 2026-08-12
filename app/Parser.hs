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
factor   ::= number | string | bool | ref | call | '(' expr ')' | '-' factor
ref      ::= '\@' int ',' int
call     ::= 'IF' '(' expr ',' expr ',' expr ')'
           | 'AND' '(' expr ',' expr ')' | 'OR' '(' expr ',' expr ')'
           | 'NOT' '(' expr ')' | 'STR' '(' expr ')' | 'NUM' '(' expr ')'
@

A cell reference is written as an absolute coordinate, e.g. @\@2,3@ means
"cell (2,3)", regardless of which cell the formula itself lives in.
Comparisons don't chain (@1<2<3@ isn't valid syntax). [^1] [^2]
-}
module Parser (parseExpr) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Text as T
import Formula (CompareOp (..), Expr (..), ExprF (..), Op (..))

parseExpr :: String -> Either String Expr
parseExpr = parseOnly (skipSpace *> compareP <* skipSpace <* endOfInput) . T.pack

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
factorP = skipSpace *> choice [refP, callP, boolP, stringP, negP, parensP, litP]

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

refP :: Parser Expr
refP = do
  _ <- char '@'
  x <- signed decimal
  skipSpace
  _ <- char ','
  skipSpace
  y <- signed decimal
  pure (Expr (RefF (x, y)))

parensP :: Parser Expr
parensP = char '(' *> skipSpace *> compareP <* skipSpace <* char ')'

callP :: Parser Expr
callP = choice [ifP, andP, orP, notP, strP, numP]

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
