{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module: Formula
Description: A small typed formula language and its evaluation over a 'Sheet2'.

Numbers, text, and booleans are distinct 'Value's - nothing converts between
them silently except 'VBlank', which reads as each operator's own identity
(@0@, @""@, 'False'), same as every real spreadsheet treats absence. [^1]
-}
module Formula (
  Expr (..),
  ExprF (..),
  Op (..),
  CompareOp (..),
  AggOp (..),
  Fn1Op (..),
  Fn2Op (..),
  Fn3Op (..),
  Value (..),
  Coord,
  blank,
  fromPair,
  toPair,
  compile,
  evaluated,
  window,
  renderExpr,
  showValue,
  adjustRefs,
) where

import Data.Char (isSpace)
import Data.Functor.Rep (Representable (..))
import Data.List (intercalate)
import Data.Time.Calendar (Day, toGregorian)
import qualified Data.Map.Strict as Map
import GHC.IO.Unsafe (unsafePerformIO)
import Formula.Builtins (
  AggOp (..),
  Fn1Op (..),
  Fn2Op (..),
  Fn3Op (..),
  Value (..),
  aggName,
  aggregate,
  apply1,
  apply2,
  apply3,
  boolean,
  fn1Name,
  fn2Name,
  fn3Name,
  formatDate,
  formatNum,
  numeric,
  showValue,
  text,
 )
import Trellis.Lists (Counted (..))
import Trellis.Sheet (
  Coordinate,
  Nat2,
  Ref (..),
  Sheet2,
  belowBy,
  cell,
  d2,
  evaluate,
  getRef,
  go,
  rightBy,
  take,
  (&),
 )

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data CompareOp = CEq | CNeq | CLt | CLte | CGt | CGte
  deriving (Eq, Show)

{- | 'Expr's pattern functor: every recursive position replaced by a type
variable, so 'Functor'\/'Foldable'\/'Traversable' derive for free. [^2]
-}
data ExprF a
  = BlankF
  | NumLitF Double
  | StrLitF String
  | BoolLitF Bool
  | RefF (Int, Int)
  | {- | An aggregate function over a rectangle of cells, given by two
    absolute corners in either order - e.g. @SUM(\@0,0:3,2)@. Only ever
    produced by a dedicated aggregate-function grammar rule (see
    "Parser"), never a general expression position.
    -}
    RangeF AggOp (Int, Int) (Int, Int)
  | ArithF Op a a
  | ConcatF a a
  | CompareF CompareOp a a
  | AndF a a
  | OrF a a
  | NotF a
  | ToStringF a
  | ToNumberF a
  | IfF a a a
  | {- | A zero-argument pseudo-random number in @[0,1)@, deterministic
    per cell position.
    -}
    RandF
  | {- | Today's date (no time component).
    -}
    TodayF
  | {- | Today's date - same as 'TodayF' since we have no time component.
    -}
    NowF
  | -- | A one-argument built-in - see "Formula.Builtins".
    Call1F Fn1Op a
  | -- | A two-argument built-in.
    Call2F Fn2Op a a
  | -- | A three-argument built-in.
    Call3F Fn3Op a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | What's typed into a cell, once parsed - the fixed point of 'ExprF'.
newtype Expr = Expr (ExprF Expr)
  deriving (Eq, Show)

-- | The formula a position with nothing stored is treated as having.
blank :: Expr
blank = Expr BlankF

-- | A 2-dimensional coordinate for indexing a 'Sheet2'.
type Coord = Coordinate Nat2

fromPair :: (Int, Int) -> Coord
fromPair (x, y) = Abs x ::: Abs y ::: CountedNil

toPair :: Coord -> (Int, Int)
toPair (x ::: y ::: _) = (getRef x, getRef y)

-- Operators -----------------------------------------------------------------

arith :: Op -> Value -> Value -> Value
arith op a b = case (numeric a, numeric b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> case op of
    Add -> VNum (x + y)
    Sub -> VNum (x - y)
    Mul -> VNum (x * y)
    Div
      | y == 0 -> VErr "#DIV/0!"
      | otherwise -> VNum (x / y)

concatV :: Value -> Value -> Value
concatV a b = case (text a, text b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> VStr (x ++ y)

{- | Blank coerces to whatever the other side is (an empty number, text, or
'False'), matching arithmetic's "blank reads as zero" leniency. Genuinely
different types, and any ordering comparison between booleans, are errors.
-}
compareV :: CompareOp -> Value -> Value -> Value
compareV _ (VErr e) _ = VErr e
compareV _ _ (VErr e) = VErr e
compareV op a b = case (a, b) of
  (VBlank, VBlank) -> ordResult EQ
  (VBlank, VNum y) -> ordResult (compare 0 y)
  (VNum x, VBlank) -> ordResult (compare x 0)
  (VBlank, VStr y) -> ordResult (compare "" y)
  (VStr x, VBlank) -> ordResult (compare x "")
  (VBlank, VBool y) -> boolResult False y
  (VBool x, VBlank) -> boolResult x False
  (VNum x, VNum y) -> ordResult (compare x y)
  (VStr x, VStr y) -> ordResult (compare x y)
  (VBool x, VBool y) -> boolResult x y
  _ -> VErr ("can't compare " ++ tag a ++ " and " ++ tag b)
 where
  ordResult o = VBool $ case op of
    CEq -> o == EQ
    CNeq -> o /= EQ
    CLt -> o == LT
    CLte -> o /= GT
    CGt -> o == GT
    CGte -> o /= LT
  boolResult x y
    | op == CEq = VBool (x == y)
    | op == CNeq = VBool (x /= y)
    | otherwise = VErr "booleans can only be compared with = or <>"
  tag VBlank = "blank"
  tag (VNum _) = "a number"
  tag (VStr _) = "text"
  tag (VBool _) = "a boolean"
  tag (VErr _) = "an error"

andV, orV :: Value -> Value -> Value
andV a b = case (boolean a, boolean b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> VBool (x && y)
orV a b = case (boolean a, boolean b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> VBool (x || y)

notV :: Value -> Value
notV a = case boolean a of
  Left e -> VErr e
  Right x -> VBool (not x)

toStringV :: Value -> Value
toStringV (VErr e) = VErr e
toStringV VBlank = VStr ""
toStringV (VStr s) = VStr s
toStringV (VBool b) = VStr (if b then "TRUE" else "FALSE")
toStringV (VNum n) = VStr (formatNum n)

toNumberV :: Value -> Value
toNumberV (VErr e) = VErr e
toNumberV VBlank = VNum 0
toNumberV (VNum n) = VNum n
toNumberV (VBool _) = VErr "can't convert a boolean to a number"
toNumberV (VStr s) = case reads (dropWhile isSpace s) of
  [(n, rest)] | all isSpace rest -> VNum n
  _ -> VErr ("can't convert " ++ show s ++ " to a number")

{- | Short-circuiting, deliberately: only the chosen branch is ever forced,
so the classic "avoid a division by zero" idiom works here the same as in
every other spreadsheet. [^3]
-}
ifV :: Value -> Value -> Value -> Value
ifV c t e = case boolean c of
  Left err -> VErr err
  Right True -> t
  Right False -> e

-- Compiling and evaluating --------------------------------------------------

{- | Shifts every 'RefF'\/'RangeF' inside an 'Expr' by a relative offset,
leaving everything else untouched - what the fill-drag gesture uses to
replicate a formula "with adjustments" rather than verbatim, reusing
'ExprF's derived 'Functor' instance to recurse through every other
constructor for free.
-}
adjustRefs :: (Int, Int) -> Expr -> Expr
adjustRefs (dx, dy) = shift
 where
  shift (Expr (RefF (x, y))) = Expr (RefF (x + dx, y + dy))
  shift (Expr (RangeF op (x0, y0) (x1, y1))) =
    Expr (RangeF op (x0 + dx, y0 + dy) (x1 + dx, y1 + dy))
  shift (Expr e) = Expr (fmap shift e)

{- | Turn a cell's source into the rule its value is derived from, given the
absolute position it's compiled for. [^4]
-}
compile :: (Int, Int) -> Expr -> (Sheet2 Value -> Value)
compile _ (Expr BlankF) = const VBlank
compile _ (Expr (NumLitF n)) = const (VNum n)
compile _ (Expr (StrLitF s)) = const (VStr s)
compile _ (Expr (BoolLitF b)) = const (VBool b)
compile (hx, hy) (Expr (RefF (tx, ty))) =
  cell (d2 (rightBy (tx - hx) & belowBy (ty - hy)))
compile (hx, hy) (Expr (RangeF op (x0, y0) (x1, y1))) =
  \sh ->
    let (minX, minY) = (min x0 x1, min y0 y1)
        cols = abs (x1 - x0) + 1
        rows = abs (y1 - y0) + 1
        vals =
          concat $
            Trellis.Sheet.take
              (rightBy (cols - 1) & belowBy (rows - 1))
              (go (rightBy (minX - hx) & belowBy (minY - hy)) sh)
     in aggregate op vals
compile here (Expr (ArithF op a b)) =
  \sh -> arith op (compile here a sh) (compile here b sh)
compile here (Expr (ConcatF a b)) =
  \sh -> concatV (compile here a sh) (compile here b sh)
compile here (Expr (CompareF op a b)) =
  \sh -> compareV op (compile here a sh) (compile here b sh)
compile here (Expr (AndF a b)) =
  \sh -> andV (compile here a sh) (compile here b sh)
compile here (Expr (OrF a b)) =
  \sh -> orV (compile here a sh) (compile here b sh)
compile here (Expr (NotF a)) =
  notV . compile here a
compile here (Expr (ToStringF a)) =
  toStringV . compile here a
compile here (Expr (ToNumberF a)) =
  toNumberV . compile here a
compile (hx, hy) (Expr RandF) = const (VNum (pseudoRand hx hy))
compile here (Expr (IfF c t e)) =
  \sh -> ifV (compile here c sh) (compile here t sh) (compile here e sh)
compile here (Expr (Call1F op a)) =
  apply1 op . compile here a
compile here (Expr (Call2F op a b)) =
  \sh -> apply2 op (compile here a sh) (compile here b sh)
compile here (Expr (Call3F op a b c)) =
  \sh -> apply3 op (compile here a sh) (compile here b sh) (compile here c sh)

{- | Compile every cell (each against its own absolute position) and
resolve the whole sheet in one comonadic pass.
-}
evaluated :: Map.Map (Int, Int) Expr -> Sheet2 Value
evaluated cellMap =
  evaluate $
    tabulate
      ( \crd ->
          let p = toPair crd in compile p (Map.findWithDefault blank p cellMap)
      )

{- | The inverse of "Parser"'s grammar: turns an 'Expr' back into the text
that produces it, for pre-filling an edit buffer. Parenthesizes by each
operator's own precedence, tracked while walking the tree.
-}
renderExpr :: Expr -> String
renderExpr = render 0
 where
  render _ (Expr BlankF) = ""
  render _ (Expr (NumLitF n)) = formatNum n
  render _ (Expr (StrLitF s)) = quote s
  render _ (Expr (BoolLitF b)) = if b then "TRUE" else "FALSE"
  render _ (Expr (RefF (x, y))) = "@" ++ show x ++ "," ++ show y
  render _ (Expr (RangeF op (x0, y0) (x1, y1))) =
    aggName op
      ++ "(@"
      ++ show x0
      ++ ","
      ++ show y0
      ++ ":"
      ++ show x1
      ++ ","
      ++ show y1
      ++ ")"
  render minPrec (Expr (ArithF op a b)) =
    chain minPrec (arithPrec op) (arithSym op) a b
  render minPrec (Expr (ConcatF a b)) = chain minPrec additivePrec "&" a b
  render minPrec (Expr (CompareF op a b)) =
    wrap
      minPrec
      comparePrec
      (render additivePrec a ++ compareSym op ++ render additivePrec b)
  render _ (Expr (AndF a b)) = call "AND" [a, b]
  render _ (Expr (OrF a b)) = call "OR" [a, b]
  render _ (Expr (NotF a)) = call "NOT" [a]
  render _ (Expr (ToStringF a)) = call "STR" [a]
  render _ (Expr (ToNumberF a)) = call "NUM" [a]
  render _ (Expr (IfF c t e)) = call "IF" [c, t, e]
  render _ (Expr RandF) = "RAND()"
  render _ (Expr (Call1F op a)) = call (fn1Name op) [a]
  render _ (Expr (Call2F op a b)) = call (fn2Name op) [a, b]
  render _ (Expr (Call3F op a b c)) = call (fn3Name op) [a, b, c]

  chain minPrec p s a b = wrap minPrec p (render p a ++ s ++ render (p + 1) b)
  wrap minPrec p str = if p < minPrec then "(" ++ str ++ ")" else str
  call name xs = name ++ "(" ++ intercalate ", " (map (render 0) xs) ++ ")"

  comparePrec = 0 :: Int
  additivePrec = 1 :: Int
  termPrec = 2 :: Int

  arithPrec Add = additivePrec
  arithPrec Sub = additivePrec
  arithPrec Mul = termPrec
  arithPrec Div = termPrec

  arithSym Add = "+"
  arithSym Sub = "-"
  arithSym Mul = "*"
  arithSym Div = "/"

  compareSym CEq = "="
  compareSym CNeq = "<>"
  compareSym CLt = "<"
  compareSym CLte = "<="
  compareSym CGt = ">"
  compareSym CGte = ">="

  quote s = "\"" ++ concatMap escape s ++ "\""
  escape '"' = "\\\""
  escape c = [c]

{- | Deterministic pseudo-random in @[0,1)@ derived from cell coordinates.
Each cell gets a distinct stable value; re-evaluating the same sheet
yields the same numbers.
-}
pseudoRand :: Int -> Int -> Double
pseudoRand x y =
  let h = abs (x * 374761393 + y * 668265263 + 1013904223) `mod` 100000
   in fromIntegral h / 100000

{- | A finite window of already-evaluated cells, @cols@ wide and @rows@
tall, with @(x, y)@ as its top-left corner. Outer list is rows, inner is
columns.
-}
window :: (Int, Int) -> Int -> Int -> Sheet2 Value -> [[Value]]
window (x, y) cols rows sh =
  Trellis.Sheet.take
    (rightBy cols & belowBy rows)
    (go (rightBy x & belowBy y) sh)

{- [^1]:
A cell reference can't be given a fixed type ahead of time - what's at
@\@2,3@ can change as other cells change - so type-checking happens at
evaluation time, together with evaluation itself, rather than as a separate
static pass that would have to lie about validating a formula in isolation.
-}

{- [^2]:
'Data.Foldable.toList' on an 'Expr' gives every cell it references, which
dependency tracking (not built yet) would want. That's the whole reason
this is a pattern functor plus a fixed point rather than an ordinary
self-recursive ADT: it costs nothing today and leaves that door open.
-}

{- [^3]:
'boolean' inspects the condition, but 't'\/'e' are returned untouched, so
Haskell's own laziness does the rest - the untaken branch's error, if it
would have had one, never happens. An earlier version of this plan required
both branches to produce the same 'Value' shape, which would have meant
forcing both, defeating exactly this idiom; correctness won out over that
extra strictness.
-}

{- [^4]:
Needed only to turn a 'RefF's absolute target into the relative offset
'cell'\/'go' understand. There is no cycle detection: a genuine circular
reference diverges under 'evaluate's laziness rather than reporting an
error - a known, deliberately deferred gap, not an oversight.
-}
