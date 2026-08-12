{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Trellis.Day (
  -- Day convolutions
  Day (..),
) where

import Control.Comonad (Comonad (..), ComonadApply (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))

data Day f g a = forall x y. Day (x -> y -> a) (f x) (g y)

instance Functor (Day f g) where
  fmap g (Day f x y) = Day (\a b -> let fab = f a b in g fab) x y
  {-# INLINE fmap #-}

instance (Comonad f, Comonad g) => Comonad (Day f g) where
  extract (Day f x y) = f (extract x) (extract y)
  {-# INLINE extract #-}
  duplicate (Day f x y) =
    let xx = duplicate x
        yy = duplicate y
     in Day (Day f) xx yy
  {-# INLINE duplicate #-}

instance (ComonadApply f, ComonadApply g) => ComonadApply (Day f g) where
  Day u fa fb <@> Day v gc gd =
    Day
      (\(a, c) (b, d) -> u a b (v c d))
      ((,) <$> fa <@> gc)
      ((,) <$> fb <@> gd)

instance (Representable f, Representable g) => Distributive (Day f g) where
  distribute f = Day fn (tabulate id) (tabulate id)
   where
    fn x y = fmap (\(Day o m n) -> o (index m x) (index n y)) f
  {-# INLINE distribute #-}

  collect g f = Day fn (tabulate id) (tabulate id)
   where
    fn x y = fmap (\q -> case g q of Day o m n -> o (index m x) (index n y)) f
  {-# INLINE collect #-}

instance (Representable f, Representable g) => Representable (Day f g) where
  type Rep (Day f g) = (Rep f, Rep g)
  tabulate f = Day (curry f) (tabulate id) (tabulate id)
  {-# INLINE tabulate #-}
  index (Day o m n) (x, y) = o (index m x) (index n y)
  {-# INLINE index #-}
