{- |
Module: Live
Description: A tiny "!"-prefixed mini-language for cells fed by a
'Trellis.Orc' subscription instead of an ordinary formula, plus the
mirror-image direction: cells that publish their value out to a pipe.
-}
module Live (
  LiveSpec (..),
  parseLiveSpec,
  toOrc,
  literal,
  declareSubscription,
  OutBinding (..),
  declareOutBinding,
  enqueueOut,
) where

import Control.Concurrent.MonadIO (
  MVar,
  MonadIO (liftIO),
  newEmptyMVar,
  putMVar,
  takeMVar,
  tryTakeMVar,
 )
import Control.Concurrent.STM.MonadIO (TVar, modifyTVar_, newTVar)
import Control.Exception (SomeException, displayException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Time.Clock (NominalDiffTime)
import Formula (Expr (..), ExprF (..))
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  IOMode (ReadMode, WriteMode),
  hClose,
  hGetLine,
  hIsEOF,
  hPutStrLn,
  hSetBuffering,
  openFile,
 )
import System.Process (readCreateProcess, shell)
import Trellis.Orc ((<|>))
import qualified Trellis.Orc as Orc

-- | A cell fed by something other than a pure formula.
data LiveSpec
  = -- | @!Ns cmd@ - re-runs @cmd@ every @N@ seconds; the cell becomes its stdout.
    ShellInterval NominalDiffTime String
  | -- | @!tail path@ - the cell becomes each line read from a file or FIFO as it arrives.
    TailFile FilePath
  deriving (Eq, Show)

{- | Parses the "!"-prefixed mini-language. "!" is unused by
'Parser.parseExpr's grammar (cell refs use "@"), so there's no collision
with an ordinary formula - a spec and a formula can never be ambiguous.
-}
parseLiveSpec :: String -> Maybe LiveSpec
parseLiveSpec ('!' : rest) = case words rest of
  ["tail", path] -> Just (TailFile path)
  (intervalTok : cmdWords@(_ : _)) -> do
    n <- parseSeconds intervalTok
    Just (ShellInterval n (unwords cmdWords))
  _ -> Nothing
parseLiveSpec _ = Nothing

-- | Parses a trailing-@s@ seconds token, e.g. @"5s"@ or @"0.5s"@.
parseSeconds :: String -> Maybe NominalDiffTime
parseSeconds tok = case reverse tok of
  ('s' : rdigits) -> case reads (reverse rdigits) :: [(Double, String)] of
    [(n, "")] -> Just (realToFrac n)
    _ -> Nothing
  _ -> Nothing

{- | The 'Trellis.Orc.Orc' computation a 'LiveSpec' actually runs. Every IO
step is exception-guarded: a bad command or a missing file becomes a
visible \"ERR: ...\" value in the cell, not a crash - a subscription is
untrusted, user-typed input the same way a formula is. [^2]
-}
toOrc :: LiveSpec -> Orc.Orc String
toOrc (ShellInterval interval cmd) = go
 where
  -- \| Yields as soon as the command finishes, *then* waits - not the
  -- other way around, so the first result shows up immediately instead
  -- of only after the first full interval has elapsed.
  go = do
    out <- liftIO (safely (readCreateProcess (shell cmd) ""))
    return out <|> (Orc.delay interval >> go)
toOrc (TailFile path) = openLoop
 where
  openLoop = do
    opened <-
      liftIO (try (openFile path ReadMode) :: IO (Either SomeException Handle))
    either retryAfterError readLoop opened

  -- \| Reads sequentially from one open handle for as long as there's
  -- data; only reopens on genuine EOF, so a fast writer doesn't pay for
  -- a close\/reopen between every line. [^1]
  readLoop h = do
    stepped <- liftIO (try (step h) :: IO (Either SomeException (Either () String)))
    case stepped of
      Left e -> liftIO (hClose h) >> retryAfterError e
      Right (Left ()) -> liftIO (hClose h) >> Orc.delay (0.5 :: Double) >> openLoop
      Right (Right line) -> return line <|> readLoop h

  step h = do
    eof <- hIsEOF h
    if eof then return (Left ()) else Right <$> hGetLine h

  retryAfterError e = do
    Orc.delay (1 :: Double)
    return ("ERR: " ++ displayException e) <|> openLoop

{- | Runs an IO action that might throw, turning any exception into a
visible error string instead of letting it escape.
-}
safely :: IO String -> IO String
safely act =
  either (("ERR: " ++) . displayException) id
    <$> (try act :: IO (Either SomeException String))

{- | Bypasses 'Parser.parseExpr' entirely - live output is a value, not
formula syntax, same as any typed-in @NumLitF@\/@StrLitF@ literal.
-}
literal :: String -> Expr
literal raw = case reads trimmed of
  [(n, "")] -> Expr (NumLitF n)
  _ -> Expr (StrLitF trimmed)
 where
  trimmed = dropWhileEnd isSpace (dropWhile isSpace raw)

{- | Forks @spec@ under @root@, writing every result it produces into
@mailbox@ tagged with @pos@; returns the child 'Orc.Group' the caller
keeps (in 'SheetState.subscriptions') to cancel it later.
-}
declareSubscription ::
  Orc.Group -> TVar [((Int, Int), Expr)] -> (Int, Int) -> LiveSpec -> IO Orc.Group
declareSubscription root mailbox pos spec =
  Orc.spawn root $ do
    raw <- toOrc spec
    liftIO (modifyTVar_ mailbox ((pos, literal raw) :))

{- | A cell watched for changes and published to a pipe\/file whenever its
value differs from what was last written - via its own background
writer thread ('declareOutBinding'), so a stuck pipe can never stall the UI.
-}
data OutBinding = OutBinding
  { outPos :: (Int, Int)
  , outBox :: MVar String
  , outLast :: TVar (Maybe String)
  }

{- | Spawns a background writer under @root@ for @path@ and returns the
handle the main loop publishes new values into (via 'enqueueOut')
whenever the watched cell's value changes. [^3]
-}
declareOutBinding :: Orc.Group -> (Int, Int) -> FilePath -> IO OutBinding
declareOutBinding root pos path = do
  box <- newEmptyMVar
  lastVal <- newTVar Nothing
  _ <- Orc.spawn root (openLoop box)
  return (OutBinding pos box lastVal)
 where
  -- \| Retries on failure - notably, opening a FIFO for writing before a
  -- reader exists fails immediately rather than blocking. [^4]
  openLoop box = do
    opened <-
      liftIO (try (openFile path WriteMode) :: IO (Either SomeException Handle))
    case opened of
      Left _ -> Orc.delay (1 :: Double) >> openLoop box
      Right h -> liftIO (hSetBuffering h LineBuffering) >> writerLoop box h

  -- \| A failed write (e.g. the reader disconnected) reopens rather
  -- than giving up for good, so a reader that goes away and comes back
  -- later still gets picked up.
  writerLoop box h = do
    val <- liftIO (takeMVar box)
    wrote <- liftIO (try (hPutStrLn h val) :: IO (Either SomeException ()))
    case wrote of
      Left _ -> liftIO (hClose h) >> openLoop box
      Right () -> writerLoop box h

{- | Non-blockingly replaces whatever's pending with the newest value - a
stale, not-yet-written value is dropped, not queued, so the caller (the
main update loop) can never block here either.
-}
enqueueOut :: OutBinding -> String -> IO ()
enqueueOut b val = do
  _ <- tryTakeMVar (outBox b)
  putMVar (outBox b) val

{- [^1]:
Correct FIFO semantics: a FIFO reports EOF once every writer has closed
it, not "no data yet" - reopening after that is what lets a *new* writer
be picked up. Re-reads a regular (non-FIFO) file from byte 0 on every
reopen, so this is scoped to pipe\/FIFO use for now.
-}

{- [^2]:
Discovered live, not hypothetically: a subscribed cell with a shell
command that exits non-zero took the whole app down before this guard
existed, since 'System.Process.readCreateProcess' throws on a non-zero
exit and nothing upstream was catching it - 'Trellis.HIO's own forked
threads don't catch on a subscriber's behalf, by design.
-}

{- [^3]:
No error surfaces anywhere if @path@ can never be opened (e.g. a bad
directory, not just "no reader yet") - 'openLoop' just retries forever,
once a second, whichever reason it's failing for. Unlike an --in
subscription there's no cell to show "ERR: ..." in, and distinguishing
"will never work" from "no reader yet" isn't possible from the
exception alone in every case, so retrying uniformly is the simplest
correct choice.
-}

{- [^4]:
Confirmed live: 'declareOutBinding' originally assumed 'openFile' would
block the way a plain C @open()@ does, same as C-level FIFO documentation
describes - it doesn't. GHC's runtime opens files non-blocking internally
for its own I\/O-manager reasons, and a non-blocking @open()@ of a FIFO
for writing with no reader fails immediately with @ENXIO@ instead of
waiting. Without the retry loop, a subscription declared before any
reader existed (the common case at startup) would fail once and never
try again, no matter how long a reader waited afterward.
-}
