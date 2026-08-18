module Main (main) where

import Cli (CliOptions (..), parseArgs)
import Control.Concurrent.STM.MonadIO (newTVar)
import Control.Exception (IOException, finally, try)
import Control.Monad (forM, void)
import qualified Data.Map.Strict as Map
import Keymap (loadKeyMap)
import Live (
  LiveSpec (TailFile),
  declareOutBinding,
  declareSubscription,
  parseLiveSpec,
 )
import Parser (parseExpr)
import Render (render, sheetOutputMode)
import SheetFile (FileEntry (..), parseSheetFile)
import SheetState (LiveBinding (..), SheetState (..), initialState)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
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
flag, or an existing sheet file that can't be read, is reported and
exits before 'Tb2.init' ever runs - never leaves the terminal stuck in
raw mode over a typo.
-}
run :: CliOptions -> IO ()
run opts = do
  keymap <- loadKeyMap
  root <- Orc.newRootGroup
  mailbox <- newTVar []
  fileEntries <- loadFileEntries (cliFile opts)
  let fileCellLines = [(pos, text) | CellEntry pos text <- fileEntries]
      fileOutLines = [(pos, path) | OutEntry pos path <- fileEntries]
      fileFormulaCells =
        Map.fromList
          [ (pos, expr)
          | (pos, text) <- fileCellLines
          , Nothing <- [parseLiveSpec text]
          , Right expr <- [parseExpr text]
          ]
  fileSubs <-
    forM
      [ (pos, spec, text)
      | (pos, text) <- fileCellLines
      , Just spec <- [parseLiveSpec text]
      ]
      $ \(pos, spec, text) -> do
        grp <- declareSubscription root mailbox pos spec
        return (pos, LiveBinding grp text)
  ins <- forM (cliIns opts) $ \(pos, path) -> do
    grp <- declareSubscription root mailbox pos (TailFile path)
    return (pos, LiveBinding grp ("!tail " ++ path))
  fileOutHandles <- forM fileOutLines (uncurry (declareOutBinding root))
  cliOutHandles <- forM (cliOuts opts) (uncurry (declareOutBinding root))
  let st0 =
        initialState
          { cells = fileFormulaCells
          , subscriptions = Map.fromList (fileSubs ++ ins)
          , outBindings = Map.fromList (fileOutLines ++ cliOuts opts)
          }
  UI.mount
    id
    setup
    ( UI.activity
        ( update
            keymap
            root
            mailbox
            ( fileOutHandles
                ++ cliOutHandles
            )
            (cliFile opts)
        )
        (render keymap)
        st0
    )
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

{- | Reads and parses the sheet file given at startup, if any. A path
that doesn't exist yet isn't an error - it's how a brand-new sheet gets
named, the same way opening a text editor on a path that doesn't exist
yet just starts you on a blank buffer; 'saveKey' creates it on first
save. A path that exists but can't be read as a plain file (wrong
permissions, a directory, etc.) still exits cleanly before 'Tb2.init'
ever runs, matching how a malformed "--in"\/"--out" flag already
behaves. A malformed line inside an otherwise-readable file is just a
warning, printed to stderr, and skipped.
-}
loadFileEntries :: Maybe FilePath -> IO [FileEntry]
loadFileEntries Nothing = return []
loadFileEntries (Just path) = do
  result <- try (readFile path) :: IO (Either IOException String)
  case result of
    Left e
      | isDoesNotExistError e -> return []
      | otherwise ->
          hPutStrLn
            stderr
            ("trellis: " ++ path ++ ": " ++ show e)
            >> exitFailure
    Right txt -> do
      let (warnings, entries) = parseSheetFile txt
      mapM_ (\w -> hPutStrLn stderr ("trellis: " ++ path ++ ": " ++ w)) warnings
      return entries
