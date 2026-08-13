{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module : Trellis.HIO
Description : Hierarchical IO - hierarchical thread-group tracking
Maintainer : gatlin@niltag.net
Stability : experimental

Vendored from garden. 'IO' augmented with a tracked tree of forked threads,
so killing a 'Group' recursively kills everything forked under it - what
makes 'Trellis.Orc's cancellation safe rather than merely convenient. [^1]
-}
module Trellis.HIO (
  -- * Hierarchical IO
  HIO (..),
  runHIO,
  unHIO,

  -- * Thread groups
  Group,
  newGroup,
  newPrimGroup,
  local,
  close,
  finished,
  register,

  -- * Auxiliary types
  Entry (..),
  Inhabitants (..),

  -- * Profiling HIO
  countingThreads,
  threadCount,
  incrementThreadCount,
  printThreadReport,
)
where

import Control.Concurrent.MonadIO (
  HasFork (..),
  MonadIO (..),
  ThreadId,
  killThread,
  myThreadId,
 )
import Control.Concurrent.STM.MonadIO (
  TVar,
  atomically,
  check,
  modifyTVar,
  modifyTVar_,
  newTVar,
  readTVar,
  readTVarSTM,
  writeTVar,
  writeTVarSTM,
 )
import Control.Exception (finally, mask)
import Control.Monad (
  ap,
  join,
  void,
  when,
 )
import System.IO.Unsafe (unsafePerformIO)

-- | Accounts for its inhabitants, which may be threads or other 'Group's.
type Group = (TVar Int, TVar Inhabitants)

-- | Empty and closed to new members, or open to any number of them.
data Inhabitants = Closed | Open [Entry]

data Entry = Thread ThreadId | Group Group

{- | 'IO' plus an ambient current 'Group', so every forked thread is tracked
and can be killed en masse via an ancestor. 'MonadIO' lets arbitrary 'IO'
be embedded, but such actions can't themselves be killed early. [^2]
-}
newtype HIO a = HIO {inGroup :: Group -> IO a}

instance Functor HIO where
  fmap f (HIO hio) = HIO (fmap (fmap f) hio)

instance Applicative HIO where
  pure x = HIO $ \_ -> pure x
  (<*>) = ap

instance Monad HIO where
  m >>= k = _join (fmap k m)
   where
    _join :: HIO (HIO a) -> HIO a
    _join hhio = HIO $ \w -> do
      x <- hhio `inGroup` w
      x `inGroup` w

instance MonadIO HIO where
  liftIO io = HIO $ const io

{- ORMOLU_DISABLE -}
instance HasFork HIO where
#ifdef __GHC_BLOCK_DEPRECATED__
    fork hio = HIO $ \w -> mask $ \restore -> do
        when countingThreads incrementThreadCount
        increment w
        fork
          ( ( do
                tid <- myThreadId
                register (Thread tid) w
                restore (hio `inGroup` w)
            )
              `finally` decrement w
          )
#else
    fork hio = HIO $ \w -> block $ do
        fork
          ( block
              ( do
                  tid <- myThreadId
                  register (Thread tid) w
                  unblock (hio `inGroup` w)
              )
              `finally` decrement w
          )
#endif
{- ORMOLU_ENABLE -}

-- | Creates a fresh child 'Group', registered under the current one and
-- ready to 'close' independently of it.
newGroup :: HIO Group
newGroup = HIO $ \w -> do
  w' <- newPrimGroup
  register (Group w') w
  return w'

-- | Runs an 'HIO' computation under an explicitly given 'Group'.
local :: Group -> HIO a -> HIO a
local w p = liftIO (p `inGroup` w)

{- | Kills every thread descended from a 'Group' and closes it, so nothing
new can register - a no-op if already closed. [^3]
-}
close :: Group -> IO ()
close (c, t) = liftIO $ void (fork (kill (Group (c, t)) >> writeTVar c 0))

-- | Blocks until every thread in a 'Group' has finished.
finished :: Group -> HIO ()
finished w = liftIO $ isZero w

{- | Runs an 'HIO' computation in a fresh, parentless root 'Group', blocking
until every thread it forks has finished.
-}
runHIO :: HIO b -> IO ()
runHIO hio = do
  w <- newPrimGroup
  _r <- hio `inGroup` w
  isZero w
  when countingThreads printThreadReport

-- | Unsafely extracts the underlying result value from the 'HIO' monad.
unHIO :: HIO a -> a
unHIO hio = unsafePerformIO $ do
  w <- newPrimGroup
  _r <- hio `inGroup` w
  isZero w
  when countingThreads printThreadReport
  return _r

-- | Creates a fresh, parentless 'Group' - the entry point for a long-lived
-- root group an application keeps around, as opposed to 'newGroup' (which
-- requires already running inside one).
newPrimGroup :: IO Group
newPrimGroup = do
  count <- newTVar 0
  threads <- newTVar (Open [])
  return (count, threads)

{- | Adds an entry to a 'Group', or - if the group is already closed - kills
the registering thread outright rather than let it join a dead group. [^4]
-}
register :: Entry -> Group -> IO ()
register tid (_, t) = join $ atomically $ do
  ts <- readTVarSTM t
  case ts of
    Closed -> return (myThreadId >>= killThread)
    Open tids ->
      writeTVarSTM t (Open (tid : tids))
        >> return (return ())

-- | Recursively kills a thread\/group entry; a no-op on an already-closed group.
kill :: Entry -> IO ()
kill (Thread tid) = killThread tid
kill (Group (_, t)) = do
  (ts, _) <- modifyTVar t (const Closed)
  case ts of
    Closed -> return ()
    Open tids -> mapM_ kill tids

increment, decrement, isZero :: Group -> IO ()
increment (c, _) = modifyTVar_ c (+ 1)
decrement (c, _) = modifyTVar_ c (\x -> x - 1)
isZero (c, _) = atomically (readTVarSTM c >>= (check . (== 0)))

-- * Profiling HIO

-- | Off by default - its report would corrupt termbox2's display if it
-- ever fired while the terminal is in raw\/alt-screen mode.
countingThreads :: Bool
countingThreads = False

threadCount :: TVar Integer
{-# NOINLINE threadCount #-}
threadCount = unsafePerformIO $ newTVar 0

incrementThreadCount :: IO ()
incrementThreadCount = modifyTVar_ threadCount (+ 1)

printThreadReport :: IO ()
printThreadReport = do
  n <- readTVar threadCount
  putStrLn "----------"
  putStrLn (show n ++ " HIO threads were forked.")

{- [^1]:
Vendored from garden, itself a from-scratch reimplementation of ideas
from Galois' @orc@ package (not a copy of it) - see 'Trellis.Orc' for
the rest of the API this module underpins.
-}

{- [^2]:
'MonadIO'-lifted 'IO' actions run outside the tracked-thread bookkeeping
that makes 'close' work; they finish on their own once started, same as
any ordinary 'IO' action would.
-}

{- [^3]:
Non-blocking: forks a killer thread and returns immediately. Callers that
need to know teardown has actually finished (e.g. before letting the
process exit) should follow with 'finished'.
-}

{- [^4]:
Otherwise a thread forked concurrently with someone else's 'close' could
slip into a group that's supposed to be empty and gone - suicide here
keeps "closed means truly empty" true even under that race.
-}
