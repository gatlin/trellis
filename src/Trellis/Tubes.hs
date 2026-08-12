{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module : Trellis.Tubes
Description : Stream processing with a series of tubes
Maintainer : gatlin@niltag.net
Stability : experimental

Pipes\/Conduit-style stream processing cast in terms of 'CPS', following the
approach of iokasimov's @pipeline@ package, adapted to use this module's own 'CPS'.
-}
module Trellis.Tubes (
  -- * A series of tubes

  -- ** Construction and evaluation
  Series,
  deliver,
  yield,
  await,
  finish,
  embed,
  Generator,
  Reducer,
  Tube,

  -- ** Combination
  (><),

  -- * Utility
  Way (..),
  Source (..),
  Sink (..),
  pause,
  suspend,
)
where

import Data.Void (Void)
import Trellis.CPS (CPS (..))

-- | The head of a stream processing series.
newtype Source i t r = Source {play :: Sink i t r -> t r}

-- | The reservoir at the end of a stream processing pipeline.
newtype Sink o t r = Sink {resume :: o -> Source o t r -> t r}

-- | An intermediate link in the series of tubes.
newtype Way i o r t a = Way {flow :: Source i t r -> Sink o t r -> t r}
  deriving (Functor)

{- | A 'Series' of 'Way's: a delimited continuation embedded in base type
@t :: * -> *@, composable via '(><)', yielding a value of type @r@ when done.
-}
type Series i o t a r = CPS r (Way i o r t) a

type Generator t o = Series Void o t () ()
type Reducer t i = Series i Void t () ()
type Tube t i o = Series i o t () ()

-- | A covariant mapping of a 'Source i t r' into 'Source o t r'.
pause :: (() -> Way i o r t a) -> Source i t r -> Source o t r
pause next ik = Source $ \ok -> (flow $ next ()) ik ok

-- | A contravariant mapping of a 'Sink o t r' into a 'Sink i t r'.
suspend :: (i -> Way i o r t a) -> Sink o t r -> Sink i t r
suspend next ok = Sink $ \v ik -> flow (next v) ik ok

-- | Blocking-wait for an upstream value from the 'Series'.
await :: Series i o t i r
await = CPS $ \k -> Way $ \ik ok -> play ik (suspend k ok)

-- | Yield a value downstream in the 'Series'.
yield :: o -> Series i o t () r
yield v = CPS $ \k -> Way $ \ik ok -> resume ok v (pause k ik)

-- | A 'Series' which does nothing.
finish :: (Monad t) => Series i o t () ()
finish = CPS $ \_ -> Way $ \_ _ -> pure ()

-- | Embeds a value with side effects into an appropriate 'Series'.
embed :: (Monad t) => t a -> Series i o t a ()
embed e = CPS $ \next -> Way $ \ik ok -> e >>= \v -> flow (next v) ik ok

-- | Constructs a 'Series' of 'Way's.
(><) ::
  forall i e a o t.
  (Monad t) =>
  Series i e t () () ->
  Series e o t () () ->
  Series i o t a ()
p >< q = CPS $ \_ -> Way $ \ik ok ->
  flow (q # end) (pause (\() -> p # end) ik) ok
 where
  end :: b -> Way c d () t ()
  end _ = Way $ \_ _ -> pure ()

-- | "...deliver[s] vast amounts of information" from a 'Series' of tubes. :)
deliver ::
  (Monad t) =>
  Series i o t r r ->
  t r
deliver p = flow (p # (\r -> Way $ \_ _ -> pure r)) i o
 where
  i :: Source i t r
  i = Source $ \o' -> play i o'

  o :: Sink o t r
  o = Sink $ \v i' -> resume o v i'
