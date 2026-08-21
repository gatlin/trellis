{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module: Trellis.UI.Core
Description: The comonadic 'Action'\/'Component' machinery every
'Trellis.UI.Screen' implementation and 'Trellis.UI' itself both need -
entirely generic over what a "screen" even is, so it's the shared leaf
both depend on rather than something either would have to duplicate.
-}
module Trellis.UI.Core (
  Handler,
  Interface,
  Component,
  Action (..),
  move,
  hoist,
  modify,
  put,
  get,
  BehaviorOf,
  behavior,
) where

import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree, coiter)
import Control.Comonad.Store (ComonadStore (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

-- | Handles some action parameterized by and resulting in a type of effect.
type Handler effect action = action effect () -> effect ()

-- | With an action 'Handler' we may construct and react to some type of @view@.
type Interface effect action view = Handler effect action -> view

-- | A space of 'Interface's which may be composed in various useful ways.
type Component effect space action view = space (Interface effect action view)

{- | Represents some action performed with or on a given component @space@.
These actions have side effects in a base monad.
-}
newtype Action space effect a = Action
  { perform :: forall r. space (a -> effect r) -> effect r
  }
  deriving (Functor)

instance (Comonad space) => Applicative (Action space effect) where
  pure !a = Action (`extract` a)
  mf <*> ma = mf >>= \f -> fmap f ma

instance (Comonad space) => Monad (Action space effect) where
  Action k >>= f =
    Action $
      k
        . extend
          ( \wa !a ->
              let !(Action fa) = f a
               in fa wa
          )

instance (Comonad space) => MonadTrans (Action space) where
  lift m = Action (extract . fmap (m >>=))

instance (Comonad space, MonadIO effect) => MonadIO (Action space effect) where
  liftIO = lift . liftIO

-- | Carries out an 'Action' in a space yielding a result with side effects.
move ::
  (Functor space) =>
  (a -> b -> effect r) ->
  Action space effect a ->
  space b ->
  effect r
move f (Action a) !s = a $! fmap (flip f) s

-- | Hoist an 'Action' for one space into a different space contravariantly.
hoist ::
  (forall x. w x -> v x) ->
  Action v effect a ->
  Action w effect a
hoist transform (Action action) = Action $ action . transform

{- | 'Action' for components built from a 'ComonadStore': modifies state.
Uses 'pos'\/'seek' rather than 'seeks' to force the new index. [^1]
-}
modify :: (ComonadStore state w) => (state -> state) -> Action w effect ()
modify fn = Action $ \(!st) ->
  let !newIndex = fn (pos st)
      !st' = seek newIndex st
      !v = extract st' ()
   in v

-- | 'Action' for components built from a 'ComonadStore': overwrites state.
put :: (ComonadStore state w) => state -> Action w effect ()
put !x = Action $ \st -> extract (seek x st) ()

-- | 'Action' for components built from a 'ComonadStore': loads state.
get :: (ComonadStore state w) => Action w effect state
get = Action $ \st -> extract st (pos st)

-- | Defines a space with the behavior of a given base functor.
type BehaviorOf = Cofree

-- | Constructs a space with the behavior of a given base functor.
behavior :: (Functor f) => (a -> f a) -> a -> BehaviorOf f a
behavior = coiter

{- [^1]:
'modify' goes via 'pos'/'seek' rather than 'seeks' specifically so the new
index can be forced: 'Control.Comonad.Trans.Store.StoreT' calls itself the
"strict" store transformer, but its index field carries no strictness
annotation and 'seeks' builds the updated index as an unforced thunk. A
long enough chain of 'modify's between renders would otherwise leak.
-}
