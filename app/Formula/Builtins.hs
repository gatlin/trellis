{- |
Module: Formula.Builtins
Description: Everything about what a built-in function actually does to
'Value's - the one place to look when adding the next one. "Formula"
itself only knows the shape a call takes ('Formula.ExprF's 'Call1F'\/
'Call2F'\/'Call3F'\/'RangeF'), not what any particular one computes.
-}
module Formula.Builtins (
  Value (..),
  numeric,
  text,
  boolean,
  formatNum,
  formatDate,
  showValue,
  Fn1Op (..),
  Fn2Op (..),
  Fn3Op (..),
  AggOp (..),
  apply1,
  apply2,
  apply3,
  aggregate,
  fn1Name,
  fn2Name,
  fn3Name,
  aggName,
) where

import Data.Char (isSpace, toLower, toUpper)
import Data.List (isPrefixOf, sort)
import Data.Time.Calendar (
  Day,
  addGregorianMonthsClip,
  diffDays,
  toGregorian,
 )

-- | What a cell displays once evaluated.
data Value
  = VBlank
  | VNum Double
  | VStr String
  | VBool Bool
  | VDate Day
  | VErr String
  deriving (Eq, Show)

-- Typed views of a 'Value' -------------------------------------------------

numeric :: Value -> Either String Double
numeric VBlank = Right 0
numeric (VNum n) = Right n
numeric (VErr e) = Left e
numeric (VStr _) = Left "expected a number, got text (try NUM(...))"
numeric (VBool _) = Left "expected a number, got a boolean"
numeric (VDate _) = Left "expected a number, got a date (try YEAR(...), MONTH(...), or DAY(...))"

text :: Value -> Either String String
text VBlank = Right ""
text (VStr s) = Right s
text (VErr e) = Left e
text (VNum _) = Left "expected text, got a number (try STR(...))"
text (VBool _) = Left "expected text, got a boolean (try STR(...))"
text (VDate d) = Right (formatDate d)

boolean :: Value -> Either String Bool
boolean VBlank = Right False
boolean (VBool b) = Right b
boolean (VErr e) = Left e
boolean (VNum _) = Left "expected a boolean, got a number"
boolean (VStr _) = Left "expected a boolean, got text"
boolean (VDate _) = Left "expected a boolean, got a date"

formatNum :: Double -> String
formatNum n
  | n == fromIntegral r = show r
  | otherwise = show n
 where
  r = round n :: Integer

formatDate :: Day -> String
formatDate d =
  let (y, m, day) = toGregorian d
      pad2 n = if n < 10 then "0" ++ show n else show n
   in show y ++ "-" ++ pad2 m ++ "-" ++ pad2 day

-- | How a 'Value' actually displays, in a cell or published out to a pipe.
showValue :: Value -> String
showValue VBlank = ""
showValue (VErr e) = e
showValue (VStr s) = s
showValue (VBool b) = if b then "TRUE" else "FALSE"
showValue (VNum n) = formatNum n
showValue (VDate d) = formatDate d

-- Scalar built-ins, grouped by arity ----------------------------------------

-- | One-argument built-ins - math on a number, or a property\/transform of text.
data Fn1Op
  = AbsOp
  | SqrtOp
  | LogOp
  | LnOp
  | ExpOp
  | SignOp
  | IntOp
  | TruncOp
  | CeilingOp
  | FloorOp
  | LenOp
  | UpperOp
  | LowerOp
  | TrimOp
  | YearOp
  | MonthOp
  | DayOp
  | DateOp
  deriving (Eq, Show)

-- | Two-argument built-ins.
data Fn2Op
  = ModOp
  | PowerOp
  | RoundOp
  | RoundUpOp
  | RoundDownOp
  | LeftOp
  | RightOp
  | FindOp
  | ReptOp
  deriving (Eq, Show)

-- | Three-argument built-ins.
data Fn3Op
  = MidOp
  | SubstituteOp
  | DateAddOp
  | DateDiffOp
  deriving (Eq, Show)

-- | An aggregate function applied to a range - see 'Formula.ExprF's 'RangeF'.
data AggOp
  = SumOp
  | AvgOp
  | CountOp
  | MinOp
  | MaxOp
  | ProductOp
  | MedianOp
  | VarOp
  | StdevOp
  | CountAOp
  deriving (Eq, Show)

mapNum :: (Double -> Value) -> Value -> Value
mapNum f v = either VErr f (numeric v)

mapNum2 :: (Double -> Double -> Value) -> Value -> Value -> Value
mapNum2 f a b = case (numeric a, numeric b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> f x y

mapText :: (String -> Value) -> Value -> Value
mapText f v = either VErr f (text v)

mapText2 :: (String -> String -> Value) -> Value -> Value -> Value
mapText2 f a b = case (text a, text b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right x, Right y) -> f x y

-- | A text argument and a numeric count argument, as most string built-ins take.
mapTextNum ::
  (String -> Double -> Either String Value) -> Value -> Value -> Value
mapTextNum f a b = case (text a, numeric b) of
  (Left e, _) -> VErr e
  (_, Left e) -> VErr e
  (Right s, Right n) -> either VErr id (f s n)

dateVal :: Value -> Either String Day
dateVal (VDate d) = Right d
dateVal (VErr e) = Left e
dateVal VBlank = Left "expected a date, got blank"
dateVal (VNum _) = Left "expected a date, got a number (try DATE(...))"
dateVal (VStr _) = Left "expected a date, got text (try DATE(...))"
dateVal (VBool _) = Left "expected a date, got a boolean"

{- | Domain errors ('SQRT' of a negative number, 'LOG'\/'LN' of a
non-positive one) would otherwise leak a silent @NaN@ into a 'VNum' -
caught here and turned into a proper error instead.
-}
guardNaN :: String -> Double -> Value
guardNaN msg x
  | isNaN x = VErr msg
  | otherwise = VNum x

apply1 :: Fn1Op -> Value -> Value
apply1 AbsOp = mapNum (VNum . abs)
apply1 SqrtOp =
  mapNum (guardNaN "SQRT is not defined for a negative number" . sqrt)
apply1 LogOp =
  mapNum (guardNaN "LOG is not defined for a non-positive number" . logBase 10)
apply1 LnOp =
  mapNum (guardNaN "LN is not defined for a non-positive number" . log)
apply1 ExpOp = mapNum (VNum . exp)
apply1 SignOp = mapNum (VNum . signum)
apply1 IntOp = mapNum (VNum . fromIntegral . (floor :: Double -> Integer))
apply1 TruncOp = mapNum (VNum . fromIntegral . (truncate :: Double -> Integer))
apply1 CeilingOp = mapNum (VNum . fromIntegral . (ceiling :: Double -> Integer))
apply1 FloorOp = mapNum (VNum . fromIntegral . (floor :: Double -> Integer))
apply1 LenOp = mapText (VNum . fromIntegral . length)
apply1 UpperOp = mapText (VStr . map toUpper)
apply1 LowerOp = mapText (VStr . map toLower)
-- \| 'words'\/'unwords' already strip leading\/trailing whitespace and
-- collapse internal runs to a single space - exactly Excel's TRIM, for free.
apply1 TrimOp = mapText (VStr . unwords . words)
apply1 YearOp = \v -> case dateVal v of
  Left e -> VErr e
  Right d -> VNum (fromIntegral (fst (toGregorian d)))
apply1 MonthOp = \v -> case dateVal v of
  Left e -> VErr e
  Right d -> VNum (fromIntegral (snd (toGregorian d)))
apply1 DayOp = \v -> case dateVal v of
  Left e -> VErr e
  Right d -> VNum (fromIntegral (snd (snd (toGregorian d))))
apply1 DateOp = mapText $ \s -> case reads (dropWhile isSpace s) of
  [(d, rest)] | all isSpace rest -> VDate d
  _ -> VErr ("can't parse date: " ++ s)

apply2 :: Fn2Op -> Value -> Value -> Value
apply2 ModOp = mapNum2 modOp
apply2 PowerOp = mapNum2 (\x y -> VNum (x ** y))
apply2 RoundOp = mapNum2 (\x n -> VNum (roundTo (round n) x))
apply2 RoundUpOp = mapNum2 (\x n -> VNum (roundUpTo (round n) x))
apply2 RoundDownOp = mapNum2 (\x n -> VNum (roundDownTo (round n) x))
apply2 LeftOp = mapTextNum $ \s n ->
  if n < 0
    then Left "LEFT's count can't be negative"
    else Right (VStr (take (round n) s))
apply2 RightOp = mapTextNum $ \s n ->
  if n < 0
    then Left "RIGHT's count can't be negative"
    else Right (VStr (reverse (take (round n) (reverse s))))
apply2 FindOp = mapText2 findOp
apply2 ReptOp = mapTextNum $ \s n ->
  if n < 0
    then Left "REPT's count can't be negative"
    else Right (VStr (concat (replicate (round n) s)))

apply3 :: Fn3Op -> Value -> Value -> Value -> Value
apply3 MidOp s start len = case (text s, numeric start, numeric len) of
  (Left e, _, _) -> VErr e
  (_, Left e, _) -> VErr e
  (_, _, Left e) -> VErr e
  (Right str, Right st, Right ln)
    | st < 1 -> VErr "MID's start must be at least 1"
    | ln < 0 -> VErr "MID's length can't be negative"
    | otherwise -> VStr (take (round ln) (drop (round st - 1) str))
apply3 SubstituteOp s old new = case (text s, text old, text new) of
  (Left e, _, _) -> VErr e
  (_, Left e, _) -> VErr e
  (_, _, Left e) -> VErr e
  (Right str, Right o, Right n) -> VStr (substitute o n str)
apply3 DateAddOp date n unit = case (dateVal date, numeric n, text unit) of
  (Left e, _, _) -> VErr e
  (_, Left e, _) -> VErr e
  (_, _, Left e) -> VErr e
  (Right d, Right num, Right u) -> case u of
    (c : _) -> case toLower c of
      'y' -> VDate (addGregorianMonthsClip (round num * 12) d)
      'm' -> VDate (addGregorianMonthsClip (round num) d)
      'd' -> VDate (d + round num)
      _ -> VErr "DATEADD unit must be \"Y\", \"M\", or \"D\""
    [] -> VErr "DATEADD unit must be \"Y\", \"M\", or \"D\""
apply3 DateDiffOp d1 d2 unit = case (dateVal d1, dateVal d2, text unit) of
  (Left e, _, _) -> VErr e
  (_, Left e, _) -> VErr e
  (_, _, Left e) -> VErr e
  (Right a, Right b, Right u) -> case u of
    (c : _) -> case toLower c of
      'y' -> VNum (fromIntegral (fst (toGregorian b) - fst (toGregorian a)))
      'm' -> VNum (fromIntegral ((fst (toGregorian b) - fst (toGregorian a)) * 12 + (snd (toGregorian b) - snd (toGregorian a))))
      'd' -> VNum (fromIntegral (diffDays b a))
      _ -> VErr "DATEDIF unit must be \"Y\", \"M\", or \"D\""
    [] -> VErr "DATEDIF unit must be \"Y\", \"M\", or \"D\""

{- | Remainder, sign following the divisor - the Excel\/Python convention,
not 'Prelude.mod' (which doesn't work on 'Double' anyway).
-}
modOp :: Double -> Double -> Value
modOp _ 0 = VErr "#DIV/0!"
modOp x y = VNum (x - y * fromIntegral (floor (x / y) :: Integer))

{- | Round-half-away-from-zero, not 'Prelude.round's round-half-to-even -
the convention every spreadsheet user actually expects.
-}
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (halfAwayFromZero (x * factor)) / factor
 where
  factor = 10 ** fromIntegral n
  halfAwayFromZero y
    | y >= 0 = floor (y + 0.5) :: Integer
    | otherwise = ceiling (y - 0.5)

-- | Always rounds away from zero, regardless of the dropped digits.
roundUpTo :: Int -> Double -> Double
roundUpTo n x = fromIntegral (awayFromZero (x * factor)) / factor
 where
  factor = 10 ** fromIntegral n
  awayFromZero y
    | y >= 0 = ceiling y :: Integer
    | otherwise = floor y

-- | Always truncates toward zero, regardless of the dropped digits.
roundDownTo :: Int -> Double -> Double
roundDownTo n x = fromIntegral (truncate (x * factor) :: Integer) / factor
 where
  factor = 10 ** fromIntegral n

findOp :: String -> String -> Value
findOp needle haystack = case findIndex' needle haystack of
  Just i -> VNum (fromIntegral (i + 1))
  Nothing -> VErr "text not found"

-- | The first position (0-based) at which @needle@ occurs in @haystack@.
findIndex' :: String -> String -> Maybe Int
findIndex' needle = go 0
 where
  go i s
    | needle `isPrefixOf` s = Just i
  go _ [] = Nothing
  go i (_ : rest) = go (i + 1) rest

-- | Every occurrence of @old@ in the third argument, replaced with @new@.
substitute :: String -> String -> String -> String
substitute "" _ s = s
substitute old new s = go s
 where
  go [] = []
  go str@(c : cs)
    | old `isPrefixOf` str = new ++ go (drop (length old) str)
    | otherwise = c : go cs

-- Aggregates ------------------------------------------------------------

{- | Folds the values a range covers: 'VBlank' is skipped (identity, same
treatment blanks get everywhere else in "Formula"), a 'VErr' anywhere
propagates, and anything else non-numeric is a strict error rather than
a silent skip - same no-implicit-coercion stance 'numeric' already
takes. 'SUM'\/'COUNT'\/'PRODUCT' are well-defined over an all-blank
range (0, 0, and 1 respectively); 'AVERAGE'\/'MIN'\/'MAX'\/'MEDIAN'\/
'VAR'\/'STDEV' of one are undefined, so they error instead of guessing.

'COUNTA' doesn't fit this numeric-strict pipeline at all - it counts
non-blank cells of any type - so it's handled directly here rather than
through 'reduceAgg', before the numeric check ever runs. A 'VErr' still
propagates first, for consistency with every other aggregate.
-}
aggregate :: AggOp -> [Value] -> Value
aggregate CountAOp vals = case [e | VErr e <- vals] of
  (e : _) -> VErr e
  [] -> VNum (fromIntegral (length (filter (/= VBlank) vals)))
aggregate op vals = case traverse asNumbers vals of
  Left e -> VErr e
  Right nss -> reduceAgg op (concat nss)
 where
  asNumbers VBlank = Right []
  asNumbers (VNum n) = Right [n]
  asNumbers (VErr e) = Left e
  asNumbers (VStr _) = Left "expected a number, got text (try NUM(...))"
  asNumbers (VBool _) = Left "expected a number, got a boolean"
  asNumbers (VDate _) = Left "expected a number, got a date"

reduceAgg :: AggOp -> [Double] -> Value
reduceAgg SumOp ns = VNum (sum ns)
reduceAgg CountOp ns = VNum (fromIntegral (length ns))
reduceAgg AvgOp [] = VErr "AVERAGE of an empty range"
reduceAgg AvgOp ns = VNum (sum ns / fromIntegral (length ns))
reduceAgg MinOp [] = VErr "MIN of an empty range"
reduceAgg MinOp ns = VNum (minimum ns)
reduceAgg MaxOp [] = VErr "MAX of an empty range"
reduceAgg MaxOp ns = VNum (maximum ns)
reduceAgg ProductOp ns = VNum (product ns)
reduceAgg MedianOp [] = VErr "MEDIAN of an empty range"
reduceAgg MedianOp ns = VNum (median ns)
reduceAgg VarOp [] = VErr "VAR of an empty range"
reduceAgg VarOp ns = VNum (variance ns)
reduceAgg StdevOp [] = VErr "STDEV of an empty range"
reduceAgg StdevOp ns = VNum (sqrt (variance ns))
-- \| Unreachable in practice - 'aggregate' handles 'CountAOp' itself,
-- before 'reduceAgg' is ever called - but a harmless real answer beats
-- an incomplete pattern match or a crash if that ever changes.
reduceAgg CountAOp ns = VNum (fromIntegral (length ns))

median :: [Double] -> Double
median xs =
  let sorted = sort xs
      n = length sorted
   in if odd n
        then sorted !! (n `div` 2)
        else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2

{- | Population variance (divides by @n@, not @n-1@) - simpler than a
sample variant, and never divides by zero at a single-element range.
-}
variance :: [Double] -> Double
variance xs =
  let m = sum xs / fromIntegral (length xs)
   in sum [(x - m) ^ (2 :: Int) | x <- xs] / fromIntegral (length xs)

-- Names, shared by Parser's grammar rules and Formula.renderExpr -----------

fn1Name :: Fn1Op -> String
fn1Name AbsOp = "ABS"
fn1Name SqrtOp = "SQRT"
fn1Name LogOp = "LOG"
fn1Name LnOp = "LN"
fn1Name ExpOp = "EXP"
fn1Name SignOp = "SIGN"
fn1Name IntOp = "INT"
fn1Name TruncOp = "TRUNC"
fn1Name CeilingOp = "CEILING"
fn1Name FloorOp = "FLOOR"
fn1Name LenOp = "LEN"
fn1Name UpperOp = "UPPER"
fn1Name LowerOp = "LOWER"
fn1Name TrimOp = "TRIM"
fn1Name YearOp = "YEAR"
fn1Name MonthOp = "MONTH"
fn1Name DayOp = "DAY"
fn1Name DateOp = "DATE"

fn2Name :: Fn2Op -> String
fn2Name ModOp = "MOD"
fn2Name PowerOp = "POWER"
fn2Name RoundOp = "ROUND"
fn2Name RoundUpOp = "ROUNDUP"
fn2Name RoundDownOp = "ROUNDDOWN"
fn2Name LeftOp = "LEFT"
fn2Name RightOp = "RIGHT"
fn2Name FindOp = "FIND"
fn2Name ReptOp = "REPT"

fn3Name :: Fn3Op -> String
fn3Name MidOp = "MID"
fn3Name SubstituteOp = "SUBSTITUTE"
fn3Name DateAddOp = "DATEADD"
fn3Name DateDiffOp = "DATEDIF"

aggName :: AggOp -> String
aggName SumOp = "SUM"
aggName AvgOp = "AVERAGE"
aggName CountOp = "COUNT"
aggName MinOp = "MIN"
aggName MaxOp = "MAX"
aggName ProductOp = "PRODUCT"
aggName MedianOp = "MEDIAN"
aggName VarOp = "VAR"
aggName StdevOp = "STDEV"
aggName CountAOp = "COUNTA"
