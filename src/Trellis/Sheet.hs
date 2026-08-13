{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module : Trellis.Sheet
Description : Spreadsheets, what else?
Maintainer : gatlin@niltag.net

A 'Sheet' is a multi-dimensional convolutional computational space. [^1]
-}
module Trellis.Sheet (
  -- * Defining and executing sheets.
  evaluate,
  evaluateF,
  cell,
  cells,
  sheet,
  change,
  sheetFromNested,
  indexedSheet,

  -- * Inexhaustible streams
  Stream (..),
  (<:>),
  repeat,
  zipWith,
  iterateStream,
  unfoldStream,
  takeStream,
  takeStreamNat,

  -- * Tapes: bidirectional streams with a focus
  Tape (..),
  tapeOf,
  moveL,
  moveR,
  iterate,
  enumerate,
  unfold,

  -- * References, indices, and coordinates
  Ref (..),
  getRef,
  CombineRefTypes (..),
  RefType (..),

  -- * Reference lists
  RefList,
  CombineRefLists (..),
  merge,
  diff,
  eitherFromRef,
  getMovement,
  dimensional,

  -- ** Coordinates
  Coordinate,
  Indexed (..),
  Cross (..),

  -- ** Indexed
  Indexable,
  indices,

  -- * Type-level 'Tape' manipulation with 'Coordinate's and 'Ref's.

  -- ** Take
  Take (..),
  tapeTake,

  -- ** View
  View (..),
  tapeView,

  -- ** Go
  Go (..),
  tapeGo,
  Signed (..),

  -- * Insertion
  InsertBase (..),
  InsertNested (..),
  AddNest,
  AsNestedAs,

  -- * Dimensions
  DimensionalAs (..),
  slice,
  insert,

  -- ** First Dimension
  Abs1,
  Rel1,
  Nat1,
  nat1,
  Sheet1,
  ISheet1,
  here1,
  d1,
  columnAt,
  column,
  rightBy,
  leftBy,
  right,
  left,

  -- ** Second Dimension
  Abs2,
  Rel2,
  Nat2,
  nat2,
  Sheet2,
  ISheet2,
  here2,
  d2,
  rowAt,
  row,
  belowBy,
  aboveBy,
  above,
  below,

  -- ** Third Dimension
  Abs3,
  Rel3,
  Nat3,
  nat3,
  Sheet3,
  ISheet3,
  here3,
  d3,
  levelAt,
  level,
  outwardBy,
  inwardBy,
  outward,
  inward,

  -- ** Fourth Dimension
  Abs4,
  Rel4,
  Nat4,
  nat4,
  Sheet4,
  ISheet4,
  here4,
  d4,
  spaceAt,
  space,
  anaBy,
  cataBy,
  ana,
  cata,

  -- * Utilities
  (&&&),
)
where

import Trellis.Lists (
  Conic (..),
  Counted (..),
  HasLength (type Length),
  Nil,
  Replicable (type Replicate),
  Tackable (tack, type Tack),
  count,
  heterogenize,
  homogenize,
  nth,
  padTo,
  replicate,
  (:-:),
 )
import qualified Trellis.Lists as L
import Trellis.Nested (
  F,
  N,
  Nested (..),
  NestedCountable (type NestedCount),
  NestedNTimes,
  UnNest,
  unNest,
 )
import Trellis.Peano (
  Natural (..),
  ReifyNatural (..),
  S,
  Z,
  type (+),
  type (-),
  type (<),
  type (<=),
 )

import Prelude hiding (
  iterate,
  repeat,
  replicate,
  take,
  zipWith,
 )
import qualified Prelude as P

import Control.Comonad
import Data.Distributive
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Kind (Type)

-- = Util

(&&&) :: (t -> a) -> (t -> b) -> t -> (a, b)
f &&& g = \v -> (f v, g v)

-- = Stream

{- | An inexhaustible source of @t@ values; unlike '[]' it can never be
empty or run out.
-}
data Stream t = Cons t (Stream t)

deriving instance (Show t) => Show (Stream t)

(<:>) :: t -> Stream t -> Stream t
(<:>) = Cons

-- | Construct an infinite 'Stream a' from a value of type @a@.
repeat :: t -> Stream t
repeat x = Cons x (repeat x)

-- | Combines two infinite streams element-wise using the provided function.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- | Repeatedly apply @step@ to a @value@ to generate the next one.
iterateStream :: (value -> value) -> value -> Stream value
iterateStream step v = Cons v (iterateStream step (step v))

unfoldStream ::
  (state -> (value, state)) ->
  state ->
  Stream value
unfoldStream step state =
  let (result, state') = step state
   in Cons result (unfoldStream step state')

-- | Prepends a finite '[value]' onto an inexhaustible 'Stream value'.
prefixStream :: [value] -> Stream value -> Stream value
prefixStream xs ys = foldr Cons ys xs

-- | Capture 'n' values from an infinite 'Stream value'.
takeStream :: Int -> Stream value -> [value]
takeStream n ~(Cons x xs)
  | n == 0 = []
  | n > 0 = x : takeStream (n - 1) xs
  | otherwise = error "takeStream: negative argument"

{- | Type-safe alternative to 'takeStream': trades the convenience of
@error@ on a bad count for a 'Natural' witness of how many items to take.
-}
takeStreamNat ::
  (Z < S n) =>
  Natural n ->
  Stream value ->
  [value]
takeStreamNat Zero _ = []
takeStreamNat (Succ n) ~(Cons x xs) = x : takeStreamNat n xs

instance Functor Stream where
  fmap f ~(Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Monad Stream where
  return = pure
  s >>= f = _join (fmap f s)
   where
    _join :: Stream (Stream a) -> Stream a
    _join ~(Cons xs xss) =
      Cons (extract xs) (_join (fmap (\(Cons _ t1) -> t1) xss))

instance Comonad Stream where
  extract (Cons x _) = x
  duplicate s@(Cons _ xs) = Cons s (duplicate xs)

instance ComonadApply Stream where
  (<@>) = (<*>)

-- = Tape

{- | Two infinite 'Stream's of @a@ joined at a center 'focus', named for
Turing machine tape.
-}
data Tape a = Tape
  { viewL :: Stream a
  , focus :: a
  , viewR :: Stream a
  }
  deriving (Functor)

-- | Utilities to move a 'Tape' one cell to the left or right.
moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs

{- | Builds a 'Tape' from a seed and two generator functions: 'prev' for the
left 'Stream', 'next' for the right.
-}
iterate :: (t -> t) -> (t -> t) -> t -> Tape t
iterate prev next seed =
  Tape
    (iterateStream prev (prev seed))
    seed
    (iterateStream next (next seed))

{- | Unfolds a 'Tape' from a seed, a 'focus' function, and two 'Stream' unfold
functions.
-}
unfold ::
  (c -> (a, c)) ->
  (c -> a) ->
  (c -> (a, c)) ->
  c ->
  Tape a
unfold prev center next seed =
  Tape
    (unfoldStream prev seed)
    (center seed)
    (unfoldStream next seed)

-- | Constructs a 'Tape' from any 'Enum'erable type.
enumerate :: (Enum a) => a -> Tape a
enumerate = iterate pred succ

instance Comonad Tape where
  extract = focus
  duplicate = iterate moveL moveR

instance ComonadApply Tape where
  Tape ls c rs <@> Tape ls' c' rs' =
    Tape (ls <@> ls') (c c') (rs <@> rs')

instance Distributive Tape where
  distribute =
    unfold
      (fmap (extract . moveL) &&& fmap moveL)
      (fmap extract)
      (fmap (extract . moveR) &&& fmap moveR)

instance Applicative Tape where
  (<*>) = (<@>)
  pure = Tape <$> pure <*> id <*> pure

instance Representable Tape where
  type Rep Tape = Int
  index t n
    | n > 0 = index (moveR t) (n - 1)
    | n < 0 = index (moveL t) (n + 1)
    | otherwise = extract t
  {-# INLINE index #-}
  tabulate describe = fmap describe (enumerate 0)
  {-# INLINE tabulate #-}

-- | 'Tape' unit.
tapeOf :: a -> Tape a
tapeOf = pure

-- = References

-- | There are two kinds reference types: 'Relative' and 'Absolute'.
data RefType = Relative | Absolute

{- | A @Ref@ is a 'RefType'-indexed @Int@. 'Absolute' denotes a coordinate in
a fixed frame; 'Relative' denotes a distance applied to another reference.
-}
data Ref (t :: RefType)
  = (t ~ 'Relative) => Rel Int
  | (t ~ 'Absolute) => Abs Int

deriving instance Show (Ref t)
deriving instance Eq (Ref t)
deriving instance Ord (Ref t)

instance Num (Ref 'Relative) where
  Rel x + Rel y = Rel (x + y)
  Rel x * Rel y = Rel (x * y)
  abs (Rel x) = Rel (abs x)
  negate (Rel x) = Rel (negate x)
  signum (Rel x) = Rel (signum x)
  fromInteger = Rel . fromIntegral

instance Num (Ref 'Absolute) where
  Abs x + Abs y = Abs (x + y)
  Abs x * Abs y = Abs (x * y)
  abs (Abs x) = Abs (abs x)
  negate (Abs x) = Abs (negate x)
  signum (Abs x) = Abs (signum x)
  fromInteger = Abs . fromIntegral

instance Enum (Ref 'Relative) where
  fromEnum (Rel r) = r
  toEnum = Rel

instance Enum (Ref 'Absolute) where
  fromEnum (Abs r) = r
  toEnum = Abs

-- | Strip the type-level info and get the raw @Int@ beneath a @Ref@.
getRef :: Ref t -> Int
getRef (Abs x) = x
getRef (Rel x) = x

class CombineRefTypes (a :: RefType) (b :: RefType) where
  type Combine a b :: RefType
  combine :: Ref a -> Ref b -> Ref (Combine a b)

instance CombineRefTypes 'Absolute 'Relative where
  type Combine 'Absolute 'Relative = 'Absolute
  combine (Abs a) (Rel b) = Abs (a + b)

instance CombineRefTypes 'Relative 'Absolute where
  type Combine 'Relative 'Absolute = 'Absolute
  combine (Rel a) (Abs b) = Abs (a + b)

instance CombineRefTypes 'Relative 'Relative where
  type Combine 'Relative 'Relative = 'Relative
  combine (Rel a) (Rel b) = Rel (a + b)

-- | A 'RefList' is a 'Conic' list of references.
type RefList = Conic Ref

infixr 4 &

-- | Combines two 'Conic' lists (useful since 'RefList' is one).
type family a & b where
  (a :-: as) & (b :-: bs) = Combine a b :-: (as & bs)
  Nil & bs = bs
  as & Nil = as

{- | Combine two reference lists elementwise; trailing elements of the
longer list are kept as-is.
-}
class CombineRefLists as bs where
  (&) :: RefList as -> RefList bs -> RefList (as & bs)

instance
  ( CombineRefTypes a b
  , CombineRefLists as bs
  ) =>
  CombineRefLists (a :-: as) (b :-: bs)
  where
  (a :-: as) & (b :-: bs) = combine a b :-: (as & bs)

instance CombineRefLists Nil (b :-: bs) where
  ConicNil & bs = bs

instance CombineRefLists (a :-: as) Nil where
  as & ConicNil = as

instance CombineRefLists Nil Nil where
  ConicNil & ConicNil = ConicNil

{- | Merge a list of relative references into a same-length list of
absolute references.
-}
merge ::
  (ReifyNatural n) =>
  Counted n (Ref 'Relative) ->
  Counted n (Ref 'Absolute) ->
  Counted n (Ref 'Absolute)
merge rs as = (\(Rel r) (Abs a) -> Abs (r + a)) <$> rs <*> as

{- | The relative movement from an absolute coordinate to the location
given by a mixed list of relative/absolute coordinates.
-}
diff ::
  Counted n (Either (Ref 'Relative) (Ref 'Absolute)) ->
  Counted n (Ref 'Absolute) ->
  Counted n (Ref 'Relative)
diff (Left (Rel r) ::: rs) (Abs _ ::: is) = Rel r ::: diff rs is
diff (Right (Abs r) ::: rs) (Abs i ::: is) = Rel (r - i) ::: diff rs is
diff CountedNil _ = CountedNil

{- | Forget a @Ref@'s type-level absolute/relative distinction, casting it
to 'Left' (relative) or 'Right' (absolute).
-}
eitherFromRef :: Ref t -> Either (Ref 'Relative) (Ref 'Absolute)
eitherFromRef (Rel r) = Left (Rel r)
eitherFromRef (Abs a) = Right (Abs a)

{- | Given an n-dimensional reference and coordinate, the relative movement
from the coordinate to the location the reference specifies.
-}
getMovement ::
  (Length ts <= n, ((n - Length ts) + Length ts) ~ n) =>
  RefList ts ->
  Counted n (Ref 'Absolute) ->
  Counted n (Ref 'Relative)
getMovement refs coords =
  padTo (count coords) (Left (Rel 0)) (homogenize eitherFromRef refs)
    `diff` coords

{- | Prepend (n - 1) zero-valued relative references to a reference, placing
it at the nth dimension.
-}
dimensional ::
  (Tackable t (Replicate n 'Relative)) =>
  Natural (S n) ->
  Ref t ->
  RefList (Tack t (Replicate n 'Relative))
dimensional (Succ n) a = tack a (heterogenize id (replicate n (Rel 0)))

--  | An 'n'-dimensional vector of 'Absolute' 'Ref'erences.
type Coordinate n = Counted n (Ref 'Absolute)

instance (ReifyNatural n) => Num (Coordinate n) where
  c1 + c2 = fmap (uncurry (+)) (L.zip c1 c2)
  c1 * c2 = fmap (uncurry (*)) (L.zip c1 c2)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . Abs . fromInteger

-- | A 'Nested' functor paired with a valid 'index' to one of its members.
data Indexed ts a = Indexed
  { origin :: Coordinate (NestedCount ts)
  , unindexed :: Nested ts a
  }

instance (Functor (Nested ts)) => Functor (Indexed ts) where
  fmap f (Indexed i t) = Indexed i (fmap f t)

class Cross n t where
  cross :: Counted n (t a) -> Nested (NestedNTimes n t) (Counted n a)

instance (Functor t) => Cross (S Z) t where
  cross (t ::: _) = Flat $ (::: CountedNil) <$> t

instance
  ( Cross (S n) t
  , Functor t
  , Functor (Nested (NestedNTimes (S n) t))
  ) =>
  Cross (S (S n)) t
  where
  cross (t ::: ts) =
    Nest $ (\xs -> (::: xs) <$> t) <$> cross ts

type Indexable ts =
  ( Cross (NestedCount ts) Tape
  , ts ~ NestedNTimes (NestedCount ts) Tape
  )

instance (ComonadApply (Nested ts), Indexable ts) => Comonad (Indexed ts) where
  extract = extract . unindexed
  duplicate it =
    Indexed (origin it) $
      Indexed
        <$> indices (origin it)
          <@> duplicate (unindexed it)

instance
  (ComonadApply (Nested ts), Indexable ts) =>
  ComonadApply (Indexed ts)
  where
  (Indexed i fs) <@> (Indexed _ xs) = Indexed i (fs <@> xs)

indices ::
  (Cross n Tape) =>
  Coordinate n ->
  Nested (NestedNTimes n Tape) (Coordinate n)
indices = cross . fmap enumerate

{- | Take all the items between the current 'focus' and the item specified by
the 'Relative' 'Ref'erence and return them as a list '[a]'.
-}
tapeTake :: Ref 'Relative -> Tape a -> [a]
tapeTake (Rel r) t | r > 0 = focus t : takeStream r (viewR t)
tapeTake (Rel r) t | r < 0 = focus t : takeStream (abs r) (viewL t)
tapeTake _ _ = []

class Take r t where
  type ListFrom t a
  take :: RefList r -> t a -> ListFrom t a

instance
  (Take Nil (Nested ts), Functor (Nested ts)) =>
  Take Nil (Nested (N ts Tape))
  where
  type ListFrom (Nested (N ts Tape)) a = ListFrom (Nested ts) [a]
  take _ = take (Rel 0 :-: ConicNil)

instance Take ('Relative :-: Nil) (Nested (F Tape)) where
  type ListFrom (Nested (F Tape)) a = [a]
  take (r :-: _) (Flat t) = tapeTake r t

instance
  ( Functor (Nested ts)
  , Take rs (Nested ts)
  ) =>
  Take ('Relative :-: rs) (Nested (N ts Tape))
  where
  type ListFrom (Nested (N ts Tape)) a = ListFrom (Nested ts) [a]
  take (r :-: rs) (Nest t) = take rs . fmap (tapeTake r) $ t

instance
  ( Take (Replicate (NestedCount ts) 'Relative) (Nested ts)
  , Length r <= NestedCount ts
  , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
  ) =>
  Take r (Indexed ts)
  where
  type ListFrom (Indexed ts) a = ListFrom (Nested ts) a
  take r (Indexed i t) = take (heterogenize id (getMovement r i)) t

tapeView :: Ref 'Relative -> Tape a -> Stream a
tapeView (Rel r) t | r >= 0 = focus t <:> viewR t
tapeView (Rel _) t = focus t <:> viewL t

class View r t where
  -- | Type of an n-dimensional stream extracted from an n-dimensional sheet.
  type StreamFrom t a

  view :: RefList r -> t a -> StreamFrom t a

instance View Nil (Nested (F Tape)) where
  type StreamFrom (Nested (F Tape)) a = Stream a
  view _ (Flat t) = tapeView (Rel 0) t

instance
  ( View Nil (Nested ts)
  , Functor (Nested ts)
  ) =>
  View Nil (Nested (N ts Tape))
  where
  type StreamFrom (Nested (N ts Tape)) a = StreamFrom (Nested ts) (Stream a)
  view _ = view (Rel 0 :-: ConicNil)

instance View ('Relative :-: Nil) (Nested (F Tape)) where
  type StreamFrom (Nested (F Tape)) a = (Stream a)
  view (r :-: _) (Flat t) = tapeView r t

instance
  ( Functor (Nested ts)
  , View rs (Nested ts)
  ) =>
  View ('Relative :-: rs) (Nested (N ts Tape))
  where
  type StreamFrom (Nested (N ts Tape)) a = StreamFrom (Nested ts) (Stream a)
  view (r :-: rs) (Nest t) = view rs . fmap (tapeView r) $ t

instance
  ( View (Replicate (NestedCount ts) 'Relative) (Nested ts)
  , Length r <= NestedCount ts
  , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
  ) =>
  View r (Indexed ts)
  where
  type StreamFrom (Indexed ts) a = StreamFrom (Nested ts) a
  view r (Indexed i t) = view (heterogenize id (getMovement r i)) t

tapeGo :: Ref 'Relative -> Tape a -> Tape a
tapeGo (Rel r) = fpow (abs r) (if r > 0 then moveR else moveL)
 where
  fpow n = foldr (.) id . P.replicate n

class Go r t where
  -- | Move to the location specified by the given @RefList@.
  go :: RefList r -> t a -> t a

instance Go ('Relative :-: Nil) (Nested (F Tape)) where
  go (r :-: _) (Flat t) = Flat $ tapeGo r t

instance Go Nil (Nested ts) where go _ = id

instance
  ( Go rs (Nested ts)
  , Functor (Nested ts)
  ) =>
  Go ('Relative :-: rs) (Nested (N ts Tape))
  where
  go (r :-: rs) (Nest t) = Nest . go rs . fmap (tapeGo r) $ t

instance
  ( Go (Replicate (NestedCount ts) 'Relative) (Nested ts)
  , Length r <= NestedCount ts
  , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
  , ReifyNatural (NestedCount ts)
  ) =>
  Go r (Indexed ts)
  where
  go r (Indexed i t) =
    let move = getMovement r i
     in Indexed (merge move i) (go (heterogenize id move) t)

{- | An 'f a' tagged 'Positive' or 'Negative' to set insertion direction;
'Negative' inserts opposite the usual direction.
-}
data Signed f a = Positive (f a) | Negative (f a)
  deriving (Eq, Ord, Show)

class InsertBase l where
  insertBase :: l a -> Tape a -> Tape a

instance InsertBase Tape where
  insertBase t _ = t

instance InsertBase Stream where
  insertBase (Cons x xs) (Tape ls _ _) = Tape ls x xs

instance InsertBase (Signed Stream) where
  insertBase (Positive (Cons x xs)) (Tape ls _ _) = Tape ls x xs
  insertBase (Negative (Cons x xs)) (Tape _ _ rs) = Tape xs x rs

instance InsertBase [] where
  insertBase [] t = t
  insertBase (x : xs) (Tape ls c rs) =
    Tape ls x (prefixStream xs (Cons c rs))

instance InsertBase (Signed []) where
  insertBase (Positive []) t = t
  insertBase (Negative []) t = t
  insertBase (Positive (x : xs)) (Tape ls c rs) =
    Tape ls x (prefixStream xs (Cons c rs))
  insertBase (Negative (x : xs)) (Tape ls c rs) =
    Tape (prefixStream xs (Cons c ls)) x rs

class InsertNested l t where
  insertNested :: l a -> t a -> t a

instance (InsertBase l) => InsertNested (Nested (F l)) (Nested (F Tape)) where
  insertNested (Flat l) (Flat t) = Flat $ insertBase l t

instance
  ( InsertBase l
  , InsertNested (Nested ls) (Nested ts)
  , Functor (Nested ls)
  , Applicative (Nested ts)
  ) =>
  InsertNested (Nested (N ls l)) (Nested (N ts Tape))
  where
  insertNested (Nest l) (Nest t) =
    Nest $ insertNested (insertBase <$> l) (pure id) <*> t

instance (InsertNested l (Nested ts)) => InsertNested l (Indexed ts) where
  insertNested l (Indexed i t) = Indexed i (insertNested l t)

type family AddNest x where
  AddNest (Nested fs (f x)) = Nested (N fs f) x

type family AsNestedAs x y where
  (f x) `AsNestedAs` (Nested (F g) b) = Nested (F f) x
  x `AsNestedAs` y = AddNest (x `AsNestedAs` UnNest y)

class NestedAs x y where
  asNestedAs :: x -> y -> x `AsNestedAs` y

instance
  (AsNestedAs (f a) (Nested (F g) b) ~ Nested (F f) a) =>
  NestedAs (f a) (Nested (F g) b)
  where
  x `asNestedAs` _ = Flat x

instance
  ( AsNestedAs (f a) (UnNest (Nested (N g h) b)) ~ Nested fs (f' a')
  , AddNest (Nested fs (f' a')) ~ Nested (N fs f') a'
  , NestedAs (f a) (UnNest (Nested (N g h) b))
  ) =>
  NestedAs (f a) (Nested (N g h) b)
  where
  x `asNestedAs` y = Nest (x `asNestedAs` unNest y)

{- | Lifts an n-deep nested structure (lists-of-lists, etc.) into an explicit
'Nested' type, so it can be inserted into a sheet without manually
annotating its depth.
-}
class DimensionalAs (x :: Type) (y :: Type) where
  type AsDimensionalAs x y
  asDimensionalAs :: x -> y -> x `AsDimensionalAs` y

-- ! These instances break everything:
instance
  ( NestedAs x (Nested ts y)
  , AsDimensionalAs x (Nested ts y) ~ AsNestedAs x (Nested ts y)
  ) =>
  DimensionalAs x (Nested ts y)
  where
  type x `AsDimensionalAs` (Nested ts y) = x `AsNestedAs` Nested ts y
  asDimensionalAs = asNestedAs

instance (NestedAs x (Nested ts y)) => DimensionalAs x (Indexed ts y) where
  type x `AsDimensionalAs` (Indexed ts y) = x `AsNestedAs` Nested ts y
  x `asDimensionalAs` (Indexed _ t) = x `asNestedAs` t

{- | Combination of 'go' and 'take': moves to the location specified by the
first argument, then takes the amount specified by the second argument.
-}
slice :: (Take r' t, Go r t) => RefList r -> RefList r' -> t a -> ListFrom t a
slice r r' = take r' . go r

{- | Insert a (possibly nested) list-like structure into a (possibly
multi-dimensional) sheet. Nesting depth must match the sheet's
dimensionality, but the input need not literally be a 'Nested' value.
-}
insert ::
  ( DimensionalAs x (t a)
  , InsertNested l t
  , AsDimensionalAs x (t a) ~ l a
  ) =>
  x ->
  t a ->
  t a
insert l t = insertNested (l `asDimensionalAs` t) t

-- = Sheets

-- == 1-D Sheets
type Abs1 = 'Absolute :-: Nil
type Rel1 = 'Relative :-: Nil
type Nat1 = S Z

nat1 :: Natural Nat1
nat1 = reifyNatural

type Sheet1 = Nested (NestedNTimes Nat1 Tape)
type ISheet1 = Indexed (NestedNTimes Nat1 Tape)

here1 :: RefList Rel1
here1 = Rel 0 :-: ConicNil

d1 :: (CombineRefLists Rel1 x) => RefList x -> RefList (Rel1 & x)
d1 = (here1 &)

columnAt :: Int -> RefList ('Absolute :-: Nil)
columnAt = dimensional nat1 . Abs

column :: (Z < NestedCount ts) => Indexed ts x -> Int
column = getRef . nth Zero . origin

rightBy, leftBy :: Int -> RefList Rel1
rightBy = dimensional nat1 . Rel
leftBy = rightBy . negate

right, left :: RefList Rel1
right = rightBy 1
left = leftBy 1

-- == 2-D Sheets
type Abs2 = 'Absolute :-: Abs1
type Rel2 = 'Relative :-: Rel1
type Nat2 = S Nat1

nat2 :: Natural Nat2
nat2 = reifyNatural

type Sheet2 = Nested (NestedNTimes Nat2 Tape)
type ISheet2 = Indexed (NestedNTimes Nat2 Tape)

here2 :: RefList Rel2
here2 = Rel 0 :-: here1

d2 :: (CombineRefLists Rel2 x) => RefList x -> RefList (Rel2 & x)
d2 = (here2 &)

rowAt :: Int -> RefList (Tack 'Absolute Rel1)
rowAt = dimensional nat2 . Abs

row :: (Nat1 < NestedCount ts) => Indexed ts x -> Int
row = getRef . nth nat1 . origin

belowBy, aboveBy :: Int -> RefList Rel2
belowBy = dimensional nat2 . Rel
aboveBy = belowBy . negate

below, above :: RefList Rel2
below = belowBy 1
above = aboveBy 1

-- == 3-D Sheets
type Abs3 = 'Absolute :-: Abs2
type Rel3 = 'Relative :-: Rel2
type Nat3 = S Nat2

nat3 :: Natural Nat3
nat3 = reifyNatural

type Sheet3 = Nested (NestedNTimes Nat3 Tape)
type ISheet3 = Indexed (NestedNTimes Nat3 Tape)

here3 :: RefList Rel3
here3 = Rel 0 :-: here2

d3 :: (CombineRefLists Rel3 x) => RefList x -> RefList (Rel3 & x)
d3 = (here3 &)

levelAt :: Int -> RefList (Tack 'Absolute Rel2)
levelAt = dimensional nat3 . Abs

level :: (Nat2 < NestedCount ts) => Indexed ts x -> Int
level = getRef . nth nat2 . origin

outwardBy, inwardBy :: Int -> RefList Rel3
outwardBy = dimensional nat3 . Rel
inwardBy = outwardBy . negate

outward, inward :: RefList Rel3
outward = outwardBy 1
inward = inwardBy 1

-- == 4-D Sheets
type Abs4 = 'Absolute :-: Abs3
type Rel4 = 'Relative :-: Rel3
type Nat4 = S Nat3

nat4 :: Natural Nat4
nat4 = reifyNatural

type Sheet4 = Nested (NestedNTimes Nat4 Tape)
type ISheet4 = Indexed (NestedNTimes Nat4 Tape)

here4 :: RefList Rel4
here4 = Rel 0 :-: here3

d4 :: (CombineRefLists Rel4 x) => RefList x -> RefList (Rel4 & x)
d4 = (here4 &)

spaceAt :: Int -> RefList (Tack 'Absolute Rel3)
spaceAt = dimensional nat4 . Abs

space :: (Nat3 < NestedCount ts) => Indexed ts x -> Int
space = getRef . nth nat3 . origin

anaBy, cataBy :: Int -> RefList Rel4
anaBy = dimensional nat4 . Rel
cataBy = anaBy . negate

ana, cata :: RefList Rel4
ana = anaBy 1
cata = cataBy 1

-- = Magic

fix :: (t -> t) -> t
fix f = let x = f x in x

evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

evaluateF :: (ComonadApply w, Functor f) => w (f (w (f a) -> a)) -> w (f a)
evaluateF fs = fix $ (<@> fs) . fmap (fmap . flip ($)) . duplicate

cell :: (Comonad w, Go r w) => RefList r -> w a -> a
cell = (extract .) . go

cells :: (Traversable t, Comonad w, Go r w) => t (RefList r) -> w a -> t a
cells = traverse cell

{- | Construct a sheet of values from a default value and an insertable
container of values.
-}
sheet ::
  ( InsertNested l t
  , Applicative t
  , DimensionalAs x (t a)
  , AsDimensionalAs x (t a) ~ l a
  ) =>
  a ->
  x ->
  t a
sheet background list = insert list (pure background)

change ::
  ( InsertNested l w
  , ComonadApply w
  , DimensionalAs x (w (w a -> a))
  , AsDimensionalAs x (w (w a -> a)) ~ l (w a -> a)
  ) =>
  x ->
  w a ->
  w a
change new old = evaluate $ insert new (fmap const old)

sheetFromNested ::
  ( InsertNested (Nested fs) (Nested (NestedNTimes (NestedCount fs) Tape))
  , Applicative (Nested (NestedNTimes (NestedCount fs) Tape))
  ) =>
  a ->
  Nested fs a ->
  Nested (NestedNTimes (NestedCount fs) Tape) a
sheetFromNested background list = insertNested list (pure background)

{- | Construct an indexed sheet from an origin index, a default value, and an
insertable container of values.
-}
indexedSheet ::
  ( InsertNested l (Nested ts)
  , Applicative (Nested ts)
  , DimensionalAs x (Nested ts a)
  , AsDimensionalAs x (Nested ts a) ~ l a
  ) =>
  Coordinate (NestedCount ts) ->
  a ->
  x ->
  Indexed ts a
indexedSheet i = (Indexed i .) . sheet

instance
  ( Functor (Nested ts)
  , Comonad (Nested ts)
  , ReifyNatural (NestedCount ts)
  , ts ~ NestedNTimes (NestedCount ts) Tape
  , Cross (NestedCount ts) Tape
  , Go (Replicate (NestedCount ts) 'Relative) (Nested ts)
  ) =>
  Representable (Nested ts)
  where
  type Rep (Nested ts) = Coordinate (NestedCount ts)
  index ts crd = cell crd' ts
   where
    crd' = heterogenize id (diff (fmap eitherFromRef crd) (pure (Abs 0)))
    {-# INLINE crd' #-}
  tabulate describe = describe <$> indices (pure (Abs 0))

instance
  ( Functor (Nested ts)
  , Comonad (Nested ts)
  , ReifyNatural (NestedCount ts)
  , ts ~ NestedNTimes (NestedCount ts) Tape
  , Cross (NestedCount ts) Tape
  , Go (Replicate (NestedCount ts) 'Relative) (Nested ts)
  ) =>
  Distributive (Nested ts)
  where
  distribute = distributeRep
  {-# INLINE distribute #-}

instance
  ( ComonadApply (Nested ts)
  , ReifyNatural (NestedCount ts)
  , Cross (NestedCount ts) Tape
  , ts ~ NestedNTimes (NestedCount ts) Tape
  , Go (Replicate (NestedCount ts) 'Relative) (Nested ts)
  ) =>
  Representable (Indexed ts)
  where
  type Rep (Indexed ts) = Coordinate (NestedCount ts)
  index (Indexed ogn sh) crd = cell crd' sh
   where
    crd' = heterogenize id (diff (fmap eitherFromRef crd) ogn)
    {-# INLINE crd' #-}
  tabulate describe = Indexed ogn $ describe <$> indices ogn
   where
    ogn = pure (Abs 0)

-- | Obligatory 'Distributive' instance.
instance
  ( ComonadApply (Nested ts)
  , ReifyNatural (NestedCount ts)
  , Cross (NestedCount ts) Tape
  , ts ~ NestedNTimes (NestedCount ts) Tape
  , Go (Replicate (NestedCount ts) 'Relative) (Nested ts)
  ) =>
  Distributive (Indexed ts)
  where
  distribute = distributeRep
  {-# INLINE distribute #-}

{- [^1]:
Re-implementation of Kenneth Foner's @ComonadSheet@ library, with bug fixes
and packaging updates. Where possible, functional dependencies have been
replaced with associated type families — mostly because I never fully
grasped functional dependencies, not for any deeper principled reason.
-}
