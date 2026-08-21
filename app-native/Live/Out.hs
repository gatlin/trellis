{- |
Module: Live.Out
Description: Cells that publish their value out to a pipe, the
mirror-image direction of "Live.In"'s subscriptions.
-}
module Live.Out (
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
import Control.Concurrent.STM.MonadIO (TVar, newTVar)
import Control.Exception (SomeException, try)
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  IOMode (WriteMode),
  hClose,
  hPutStrLn,
  hSetBuffering,
  openFile,
 )
import qualified Trellis.Orc as Orc

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
whenever the watched cell's value changes. [^1]
-}
declareOutBinding :: Orc.Group -> (Int, Int) -> FilePath -> IO OutBinding
declareOutBinding root pos path = do
  box <- newEmptyMVar
  lastVal <- newTVar Nothing
  _ <- Orc.spawn root (openLoop box)
  return (OutBinding pos box lastVal)
 where
  -- \| Retries on failure - notably, opening a FIFO for writing before a
  -- reader exists fails immediately rather than blocking. [^2]
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
No error surfaces anywhere if @path@ can never be opened (e.g. a bad
directory, not just "no reader yet") - 'openLoop' just retries forever,
once a second, whichever reason it's failing for. Unlike an --in
subscription there's no cell to show "ERR: ..." in, and distinguishing
"will never work" from "no reader yet" isn't possible from the
exception alone in every case, so retrying uniformly is the simplest
correct choice.
-}

{- [^2]:
Confirmed live: 'declareOutBinding' originally assumed 'openFile' would
block the way a plain C @open()@ does, same as C-level FIFO documentation
describes - it doesn't. GHC's runtime opens files non-blocking internally
for its own I\/O-manager reasons, and a non-blocking @open()@ of a FIFO
for writing with no reader fails immediately with @ENXIO@ instead of
waiting. Without the retry loop, a subscription declared before any
reader existed (the common case at startup) would fail once and never
try again, no matter how long a reader waited afterward.
-}
