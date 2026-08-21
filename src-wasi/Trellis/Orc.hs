{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module : Trellis.Orc
Description : The wasm32-wasi backend - torc-backed, not HIO/STM/forkIO-backed
Maintainer : gatlin@niltag.net
Stability : experimental

The native 'Trellis.Orc' (see @src-native/Trellis/Orc.hs@) is a rich
delimited-continuation orchestration language, but trellis's own code
only ever touches a narrow slice of it: a 'Group' handle to cancel a
background subscription, created once at the root and once per
live-cell subscription. Rather than gamble on GHC's own green threads
under a backend as new as wasm32-wasi, this backend hands that same
narrow slice off to **torc** (@~\/code\/torc@, @github.com\/gatlin\/torc@) -
a JS reactive-streams library the user wrote, explicitly modeled on the
same Orc calculus this module's native sibling is - so all the actual
async\/concurrent behavior runs in the browser's own event loop, where
it's already known to work.

Deliberately not reimplemented here, because trellis never calls them:
'Trellis.Orc.Orc' the monad, @spawn@, @\<|\>@, @delay@, @finished@\/
@inGroup@ (see @app-wasi\/Main.hs@ for why teardown doesn't need them
on this backend). 'Live.In'\/'Live.Out' each get their own from-scratch
wasm implementation (see @app-wasi\/Live\/In.hs@\/@Out.hs@) that build
torc 'Observable's directly via JSFFI, rather than writing 'Orc'
computations for a generic @spawn@ to run.
-}
module Trellis.Orc (
  Group,
  Activity (..),
  newRootGroup,
  newChildGroup,
  close,
) where

import Data.IORef
import GHC.Wasm.Prim (JSVal)

{- | An opaque handle to a torc @Activity@ (@{ finish(): void }@) - what
@.subscribe()@ on a torc @Observable@ returns.
-}
newtype Activity = Activity JSVal

foreign import javascript unsafe "$1.finish()"
  js_finish :: JSVal -> IO ()

{- | A flat set of 'Activity' handles that can be cancelled together.
Trellis never nests groups beyond one level (a root, then one child per
live-cell subscription\/out-binding) on either backend, so unlike
native's 'Trellis.HIO.Group' this doesn't need real hierarchy - just
enough bookkeeping for the root to reach every child at teardown.
-}
newtype Group = Group (IORef [Activity])

-- | A fresh, empty root - the one an application keeps around for the
-- lifetime of every subscription\/out-binding it declares.
newRootGroup :: IO Group
newRootGroup = Group <$> newIORef []

{- | Wraps a freshly-@.subscribe()@d torc 'Activity' as its own
single-member child 'Group' (what 'SheetState.LiveBinding.liveGroup'
and similar hold onto), also registering it under @root@ so the root's
own 'close' reaches it too - the same two-level shape
'Update.Subscriptions.cancelSubscription' (closing just one cell's
subscription) and a full teardown (closing everything) both already
expect.
-}
newChildGroup :: Group -> Activity -> IO Group
newChildGroup (Group rootRef) act = do
  modifyIORef' rootRef (act :)
  Group <$> newIORef [act]

{- | Calls @.finish()@ on every 'Activity' this 'Group' tracks - idempotent,
since torc's own @Activity.finish@ contract already is. Synchronous from
JS's own perspective (unlike native's HIO-backed 'close', which only
forks a killer thread and needs a separate blocking 'finished' step
after it) - there's no separate "wait for it to actually stop" step
needed on this backend.
-}
close :: Group -> IO ()
close (Group ref) = do
  acts <- readIORef ref
  mapM_ (\(Activity v) -> js_finish v) acts
  writeIORef ref []
