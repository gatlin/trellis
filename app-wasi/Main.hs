{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module: Main
Description: The wasm32-wasi entry point. A wasm32-wasi build links as a
"reactor" module (see @trellis.cabal@'s @-optl-mexec-model=reactor@), which
has no default entry point at all, not even 'main' - JS must call an
explicit @foreign export@ instead, once, after instantiating. 'main' itself
is a required-but-unused no-op (same pattern as @wasm\/spike\/Tick.hs@);
'appSetup' is the real startup, exported for @wasm\/trellis-sheet.js@ to
call.

'root'\/'mailbox'\/'outsRef' are module-level (not local to 'appSetup')
specifically so 'trellisBindIn'\/'trellisWatchOut' - called later, from
outside the normal event-dispatch loop entirely, by the web component's
own @bindIn@\/@watchOut@ - can still reach them. This is safe despite
looking like global mutable state: each @\<trellis-sheet\>@ gets its own
independent 'WebAssembly.instantiate' call (see @wasm\/trellis-sheet.js@),
which gives it its own independent linear memory and thus its own
independent copies of every top-level ref in this whole program, not a
single set shared across instances.
-}
module Main (main) where

import Control.Concurrent.STM.MonadIO (TVar, newTVar)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Formula (Expr)
import GHC.Wasm.Prim (JSVal)
import Keymap (loadKeyMap)
import Live (OutBinding, declareOutBinding)
import Live.In (declareExternalSubscription)
import Render (render, sheetOutputMode)
import SheetState (initialState)
import System.IO.Unsafe (unsafePerformIO)
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI
import Update (update)

{-# NOINLINE rootRef #-}
rootRef :: IORef (Maybe Orc.Group)
rootRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE mailboxRef #-}
mailboxRef :: IORef (Maybe (TVar [((Int, Int), Expr)]))
mailboxRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE outsRef #-}
outsRef :: IORef [OutBinding]
outsRef = unsafePerformIO (newIORef [])

{- | Every position 'trellisBindIn' has ever bound, so re-binding the
same cell closes the old subscription first instead of leaking it -
'declareExternalSubscription's own cleanup only ever fires when
*something* calls @.finish()@ on what it returns, and nothing else in
this design ever would on its own.
-}
{-# NOINLINE externalBindingsRef #-}
externalBindingsRef :: IORef (Map.Map (Int, Int) Orc.Group)
externalBindingsRef = unsafePerformIO (newIORef Map.empty)

{- | No argv, no CLI flags, no sheet file to load - none of that is
meaningful yet for a browser (see @app-native/Main.hs@'s equivalent CLI
handling, which has no analog here) - just a blank sheet. Teardown isn't
wired up at all: unlike native, a wasm reactor module has no
"the program is exiting" moment to hook - the page just closing\/
navigating away is what ends it, and torc's own 'Activity.finish' being
a no-op-if-called-twice means never calling 'Orc.close' here at all is
harmless, not a leak in any way that outlives the page itself.
-}
foreign export javascript "trellisSetup sync" appSetup :: IO ()
appSetup :: IO ()
appSetup = do
  keymap <- loadKeyMap
  root <- Orc.newRootGroup
  mailbox <- newTVar []
  writeIORef rootRef (Just root)
  writeIORef mailboxRef (Just mailbox)
  UI.mount
    id
    setup
    ( UI.activity
        (update keymap root mailbox outsRef Nothing)
        (render keymap)
        initialState
    )
 where
  setup = do
    UI.setInputMode (UI.inputEsc <> UI.inputMouse)
    UI.setOutputMode sheetOutputMode

main :: IO ()
main = return ()

{- | The web component's @bindIn(row, col, subscribeFn)@ - see
@wasm\/trellis-sheet.js@. Called any time after 'appSetup' (never
before, in practice - the component always awaits setup first), so
'rootRef'\/'mailboxRef' are always populated by the time this can run.
-}
foreign export javascript "trellisBindIn sync" trellisBindIn :: Int -> Int -> JSVal -> IO ()
trellisBindIn :: Int -> Int -> JSVal -> IO ()
trellisBindIn row col subscribeFn = do
  Just root <- readIORef rootRef
  Just mailbox <- readIORef mailboxRef
  -- \| The public JS API takes (row, col) - the more intuitive order
  -- for anyone thinking "row 3, column 2" - but SheetState/Formula's
  -- own internal (Int,Int) convention is (col, row) throughout (see
  -- Cli.parseCoord's own doc: "0,0" -> (0,0), same order @x,y@ formula
  -- syntax uses) - swapped here, once, right at the boundary, so nothing
  -- downstream needs to care which convention the JS caller used.
  let pos = (col, row)
  existing <- readIORef externalBindingsRef
  case Map.lookup pos existing of
    Just oldGroup -> Orc.close oldGroup
    Nothing -> return ()
  grp <- declareExternalSubscription root mailbox pos subscribeFn
  modifyIORef' externalBindingsRef (Map.insert pos grp)

{- | The web component's @watchOut(row, col)@ - see
@wasm\/trellis-sheet.js@. Appends a fresh 'OutBinding' to 'outsRef',
which 'Update.Core.update' now reads fresh every 'UI.Tick' rather than
closing over a fixed list the way native's own (never-changing) set
still does - this is the one thing that actually makes a *dynamically*
added watch show up at all. Deliberately doesn't also update
'SheetState.outBindings' (the display\/bookkeeping map
'publishOutUpdates' itself never actually reads) - doing so would need
a way to run 'UI.modify' from outside the normal dispatch loop, which
nothing here has, and nothing actually needs.
-}
foreign export javascript "trellisWatchOut sync" trellisWatchOut :: Int -> Int -> IO ()
trellisWatchOut :: Int -> Int -> IO ()
trellisWatchOut row col = do
  Just root <- readIORef rootRef
  -- \| (row, col) -> (col, row) - see 'trellisBindIn's own note.
  binding <- declareOutBinding root (col, row) ""
  modifyIORef' outsRef (binding :)

{- | Drops whatever 'trellisBindIn' subscription was bound to this cell,
if any - the web component's own @unbindIn@, mirroring 'trellisBindIn'.
A no-op if nothing was ever bound there.
-}
foreign export javascript "trellisUnbindIn sync" trellisUnbindIn :: Int -> Int -> IO ()
trellisUnbindIn :: Int -> Int -> IO ()
trellisUnbindIn row col = do
  let pos = (col, row) -- must match trellisBindIn's own key construction
  existing <- readIORef externalBindingsRef
  case Map.lookup pos existing of
    Just oldGroup -> do
      Orc.close oldGroup
      modifyIORef' externalBindingsRef (Map.delete pos)
    Nothing -> return ()
