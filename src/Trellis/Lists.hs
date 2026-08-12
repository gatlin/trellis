{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module : Trellis.Lists
Description : Different types of lists exploiting type arithmetic
Maintainer : gatlin@niltag.net

'Counted' lists encode their length as a 'Peano' number in the type, enabling
compile-time bounds checks. 'Conic' lists let each element have a different
type, tracked in the list's type signature.
-}
module Trellis.Lists (
  -- * Counted Lists
  Counted (..),
  count,
  unCount,

  -- ** Counted utilities
  replicate,
  append,
  nth,
  padTo,
  zip,

  -- * Conic Lists
  Conic (..),
  (:-:),
  Nil,

  -- ** Type-level conic manipulation
  Replicable (type Replicate),
  Tackable (type Tack, tack),
  HasLength (type Length),

  -- * Conversion
  heterogenize,
  homogenize,
)
where

import Prelude hiding (
  LT,
  head,
  iterate,
  map,
  repeat,
  replicate,
  tail,
  take,
  zip,
  zipWith,
  (+),
  (-),
  (<),
  (<=),
 )
import qualified Prelude as P

import Data.List hiding (
  cycle,
  head,
  insert,
  iterate,
  repeat,
  replicate,
  tail,
  take,
  zip,
  zipWith,
 )

import Trellis.Peano (
  Minus (..),
  Natural (..),
  ReifyNatural (..),
  S,
  Z,
  type (+),
  type (-),
  type (<),
  type (<=),
 )

import Data.Kind (Type)

-- = Counted lists

infixr 5 :::

-- | A 'Counted' list encodes its length as a type parameter.
data Counted (n :: Type) (a :: Type)
  = -- | Prepend an item.
    forall t. (n ~ S t) => a ::: (Counted t a)
  | -- | Construct an empty list.
    (n ~ Z) => CountedNil

instance Functor (Counted n) where
  fmap _ CountedNil = CountedNil
  fmap f (a ::: as) = f a ::: fmap f as

instance (ReifyNatural n) => Applicative (Counted n) where
  pure = replicate (reifyNatural :: Natural n)
  fs <*> xs = uncurry id <$> zip fs xs

instance (Show x) => Show (Counted n x) where
  showsPrec p xs =
    showParen (p > 10) $
      showString $
        intercalate
          " ::: "
          ( P.map show $
              unCount xs
          )
          ++ " ::: CountedNil"

instance (Eq a) => Eq (Counted n a) where
  CountedNil == CountedNil = True
  (h1 ::: t1) == (h2 ::: t2)
    | h1 == h2 = t1 == t2
    | otherwise = False

-- | Compute the length of a @Counted@ in terms of a @Natural@.
count :: Counted n a -> Natural n
count CountedNil = Zero
count (_ ::: xs) = Succ (count xs)

unCount :: Counted n a -> [a]
unCount xs = go xs []
 where
  go :: Counted n a -> [a] -> [a]
  go CountedNil final = final
  go (x ::: xs') acc = go xs' (x : acc)

replicate :: Natural n -> x -> Counted n x
replicate Zero _ = CountedNil
replicate (Succ n) x = x ::: replicate n x

append :: Counted n a -> Counted m a -> Counted (m + n) a
append CountedNil ys = ys
append (x ::: xs) ys = x ::: append xs ys

nth :: (n < m) => Natural n -> Counted m a -> a
nth Zero (a ::: _) = a
nth (Succ n) (_ ::: as) = nth n as

{- | Pad a list to a given length; fails to type-check if the target length
is less than the list's current length. [^1]
-}
padTo ::
  (m <= n) =>
  Natural n ->
  x ->
  Counted m x ->
  Counted ((n - m) + m) x
padTo n x list =
  list `append` replicate (n `minus` count list) x

zip :: Counted n a -> Counted n b -> Counted n (a, b)
zip (a ::: as) (b ::: bs) = (a, b) ::: zip as bs
zip CountedNil _ = CountedNil

-- = Conic lists

infixr 5 :-:
data (x :: k1) :-: (y :: k2)
data Nil

{- | A list where each element is @f a@ for some @a@, but @a@ may differ per
element; each element's type is tracked in the list's type signature.
-}
data Conic f ts
  = forall rest a. (ts ~ (a :-: rest)) => (f a) :-: (Conic f rest)
  | (ts ~ Nil) => ConicNil

class (ReifyNatural n) => Replicable n (x :: k) where
  type Replicate n x :: Type

instance Replicable Z x where
  type Replicate Z x = Nil

instance (Replicable n x) => Replicable (S n) x where
  type Replicate (S n) x = x :-: Replicate n x

-- | A @Conic@ length may be computed at the type level.
class HasLength (ts :: k) where
  type Length ts :: Type

instance HasLength Nil where
  type Length Nil = Z

instance (HasLength xs) => HasLength (x :-: xs) where
  type Length (x :-: xs) = S (Length xs)

-- | A @Conic@ supports "tacking" a value onto its head.
class Tackable (x :: k) (xs :: Type) where
  type Tack x xs :: Type
  tack :: f x -> Conic f xs -> Conic f (Tack x xs)

instance Tackable a Nil where
  type Tack a Nil = a :-: Nil
  tack a ConicNil = a :-: ConicNil

instance (Tackable a xs) => Tackable a (x :-: xs) where
  type Tack a (x :-: xs) = x :-: Tack a xs
  tack a (x :-: xs) = x :-: tack a xs

-- = Conversion

{- | Turn a 'Counted' list into a 'Conic' list via a function from @a@ to
@(f t)@.
-}
heterogenize ::
  (a -> f t) ->
  Counted n a ->
  Conic f (Replicate n t)
heterogenize _ CountedNil = ConicNil
heterogenize f (x ::: xs) = f x :-: heterogenize f xs

{- | Collapse a 'Conic' list into a 'Counted' list using a function that turns
any @(f t)@ into an @a@.
-}
homogenize ::
  (forall t. f t -> a) ->
  Conic f ts ->
  Counted (Length ts) a
homogenize _ ConicNil = CountedNil
homogenize f (x :-: xs) = f x ::: homogenize f xs

{- [^1]:

If @padTo@ has the following simpler type ...

@
    padTo
      :: (m <= n)
      => Natural n
      -> x
      -> Counted m x
      -> Counted n x
@

... then GHC reports the following error:

@

    src/Lists.hs:124:3: error:
        • Could not deduce: ((n - m) + m) ~ n
          from the context: m <= n
            bound by the type signature for:
                       padTo :: forall m n x.
                                (m <= n) =>
                                Natural n -> x -> Counted m x -> Counted n x
            at src/Lists.hs:(117,1)-(122,16)
          ‘n’ is a rigid type variable bound by
            the type signature for:
              padTo :: forall m n x.
                       (m <= n) =>
                       Natural n -> x -> Counted m x -> Counted n x
            at src/Lists.hs:(117,1)-(122,16)
          Expected type: Counted n x
            Actual type: Counted ((n - m) + m) x
        • In the expression:
            list `append` replicate (n `minus` count list) x
          In an equation for ‘padTo’:
              padTo n x list = list `append` replicate (n `minus` count list) x
        • Relevant bindings include
            list :: Counted m x (bound at src/Lists.hs:123:11)
            n :: Natural n (bound at src/Lists.hs:123:7)
            padTo :: Natural n -> x -> Counted m x -> Counted n x
              (bound at src/Lists.hs:123:1)
        |
    124 |   list `append` replicate (n `minus` count list) x
        |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

@

I am not entirely certain why the type checker cannot deduce @((n - m) + m) ~ n@,
but it cannot and so the type spells out this relation explicitly:

@
    padto
      :: (m <= n)
      => Natural n
      -> x
      -> Counted m x
      -> Counted ((n - m) + m) x
@
-}
