module Main (main) where

import Cli (CliOptions (..), parseArgs)
import Control.Concurrent.STM.MonadIO (newTVar)
import Control.Exception (finally)
import Control.Monad (forM, void)
import qualified Data.Map.Strict as Map
import Keymap (loadKeyMap)
import Live (LiveSpec (TailFile), declareOutBinding, declareSubscription)
import Render (render, sheetOutputMode)
import SheetState (LiveBinding (..), SheetState (..), initialState)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Termbox2 as Tb2
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI
import Update (update)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right opts -> run opts

{- | Kept separate from argument parsing so a malformed "--in"\/"--out"
flag is reported and exits before 'Tb2.init' ever runs - never leaves
the terminal stuck in raw mode over a typo.
-}
run :: CliOptions -> IO ()
run opts = do
  keymap <- loadKeyMap
  root <- Orc.newRootGroup
  mailbox <- newTVar []
  ins <- forM (cliIns opts) $ \(pos, path) -> do
    grp <- declareSubscription root mailbox pos (TailFile path)
    return (pos, LiveBinding grp ("!tail " ++ path))
  outs <- forM (cliOuts opts) $ uncurry (declareOutBinding root)
  let st0 =
        initialState
          { subscriptions = Map.fromList ins
          , outBindings = Map.fromList (cliOuts opts)
          }
  UI.mount id setup (UI.activity (update keymap root mailbox outs) render st0)
    `finally` teardown root
 where
  setup = do
    void (Tb2.setInputMode (Tb2.inputEsc <> Tb2.inputMouse))
    void (Tb2.setOutputMode sheetOutputMode)
  -- \| 'Orc.close' only forks a killer thread and returns; blocking on
  -- 'Orc.finished' after it is what makes teardown finish before the
  -- process exits, so nothing under 'root' can outlive Trellis.
  teardown root = do
    Orc.close root
    Orc.finished root `Orc.inGroup` root
