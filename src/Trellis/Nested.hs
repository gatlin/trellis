{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module : Trellis.Nested
Description : Defines how to nest functors
Maintainer : gatlin@niltag.net

Re-implementation of a library by Kenneth Foner, updated to compile with
functional dependencies replaced by associated type families where possible.
Used by 'Trellis.Sheet' to build and inspect multi-dimensional containers.
-}
module Trellis.Nested (
  -- * Nested functors
  Nested (..),
  F,
  N,

  -- * Deconstructing nested functors
  UnNest,
  unNest,

  -- * Manipulating nested functors
  NestedCountable (type NestedCount),
  nestedCount,
  NestedNTimes,
)
where

import Control.Applicative (Alternative (..))
import Control.Comonad (
  Comonad (..),
  ComonadApply (..),
 )
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Kind (Type)

import Trellis.Peano (Natural (..), S, Z)

data F (x :: Type -> Type)
data N (o :: Type) (i :: Type -> Type)

{- | A @Nested fs a@ composes the layers in @fs@ applied to @a@; @fs@ is a
snoc-list of type constructors, with the outermost layer as @Flat@'s argument
and the innermost as the rightmost argument of the outermost @Nest@.
-}
data Nested fs a
  = forall f. (fs ~ F f) => Flat (f a)
  | forall fs' f. (fs ~ N fs' f) => Nest (Nested fs' (f a))

type family UnNest x where
  UnNest (Nested (F f) a) = f a
  UnNest (Nested (N fs f) a) = Nested fs (f a)

unNest :: Nested fs a -> UnNest (Nested fs a)
unNest (Flat x) = x
unNest (Nest x) = x

instance (Functor f) => Functor (Nested (F f)) where
  fmap f = Flat . fmap f . unNest

instance (Functor f, Functor (Nested fs)) => Functor (Nested (N fs f)) where
  fmap f = Nest . fmap (fmap f) . unNest

instance (Applicative f) => Applicative (Nested (F f)) where
  pure = Flat . pure
  Flat f <*> Flat x = Flat (f <*> x)

instance
  ( Applicative f
  , Applicative (Nested fs)
  ) =>
  Applicative (Nested (N fs f))
  where
  pure = Nest . pure . pure
  Nest f <*> Nest x = Nest ((<*>) <$> f <*> x)

instance (Comonad f) => Comonad (Nested (F f)) where
  extract = extract . unNest
  duplicate = fmap Flat . Flat . duplicate . unNest

instance
  ( Comonad f
  , Comonad (Nested fs)
  , Functor (Nested (N fs f))
  , Distributive f
  ) =>
  Comonad (Nested (N fs f))
  where
  extract = extract . extract . unNest
  duplicate =
    fmap Nest
      . Nest -- wrap it again
      . fmap distribute -- swap middle two layers
      . duplicate -- duplicate outer functor
      . fmap duplicate -- duplicate inner functor
      . unNest -- note: can't pattern match

instance (ComonadApply f) => ComonadApply (Nested (F f)) where
  Flat f <@> Flat x = Flat (f <@> x)

instance
  ( ComonadApply f
  , Distributive f
  , ComonadApply (Nested fs)
  ) =>
  ComonadApply (Nested (N fs f))
  where
  Nest f <@> Nest x = Nest ((<@>) <$> f <@> x)

instance (Distributive f) => Distributive (Nested (F f)) where
  distribute = Flat . distribute . fmap unNest

instance
  ( Distributive f
  , Distributive (Nested fs)
  ) =>
  Distributive (Nested (N fs f))
  where
  distribute = Nest . fmap distribute . distribute . fmap unNest

instance (Foldable f) => Foldable (Nested (F f)) where
  foldMap f = foldMap f . unNest

instance
  ( Foldable f
  , Foldable (Nested fs)
  ) =>
  Foldable (Nested (N fs f))
  where
  foldMap f = foldMap (foldMap f) . unNest

instance (Traversable f) => Traversable (Nested (F f)) where
  traverse f = fmap Flat . traverse f . unNest

instance
  ( Traversable f
  , Traversable (Nested fs)
  ) =>
  Traversable (Nested (N fs f))
  where
  traverse f = fmap Nest . traverse (traverse f) . unNest

instance (Alternative f) => Alternative (Nested (F f)) where
  empty = Flat empty
  Flat x <|> Flat y = Flat (x <|> y)

instance
  ( Applicative f
  , Alternative (Nested fs)
  ) =>
  Alternative (Nested (N fs f))
  where
  empty = Nest empty
  Nest x <|> Nest y = Nest (x <|> y)

class NestedCountable (x :: k) where
  type NestedCount x :: Type

instance NestedCountable (F f) where
  type NestedCount (F f) = S Z

instance NestedCountable (N fs f) where
  type NestedCount (N fs f) = S (NestedCount fs)

nestedCount :: Nested fs a -> Natural (NestedCount fs)
nestedCount (Flat _) = Succ Zero
nestedCount (Nest x) = Succ (nestedCount x)

type family NestedNTimes (n :: Type) (f :: Type -> Type) where
  NestedNTimes (S Z) f = F f
  NestedNTimes (S n) f = N (NestedNTimes n f) f
