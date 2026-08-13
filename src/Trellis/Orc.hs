{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module : Trellis.Orc
Description : Delimited-continuation orchestration for concurrent, cancellable, many-valued computations
Maintainer : gatlin@niltag.net
Stability : experimental

Vendored from garden (itself a re-implementation of Galois' @orc@ package,
based on the Orc language from UT Austin). 'Orc' is 'Trellis.CPS.CPS'
specialised to 'Trellis.HIO.HIO': its continuation is an ordinary,
shareable function - unlike 'Trellis.UI.Action', whose @forall r.@
encoding is deliberately one-shot - so '<|>' can genuinely fork two
branches that both call it. [^1]
-}
module Trellis.Orc (
  -- * A language for distributed Orchestration
  Orc,
  runOrc,
  spawn,
  collect,

  -- * Combinators
  par,
  (<|>),
  stop,
  signal,
  (<+>),
  (<?>),
  cut,
  val,
  eagerly,
  onlyUntil,
  butAfter,
  notBefore,
  delay,
  publish,
  repeating,
  sync,

  -- * List-like utilities
  takeOrc,
  dropOrc,
  zipOrc,
  liftList,
  syncList,
  runChan,
  scan,

  -- * Hierarchical thread groups
  HIO.Group,
  newRootGroup,
  HIO.close,
  HIO.finished,
  HIO.inGroup,

  -- * Re-exports & convenience
  (#),
  shift,
  reset,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent.MonadIO (
  Chan,
  MVar,
  MonadIO (..),
  fork,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
  threadDelay,
  tryPutMVar,
  tryTakeMVar,
  writeChan,
 )
import Control.Concurrent.STM.MonadIO (
  atomically,
  modifyTVar,
  newTVar,
  readTVar,
  readTVarSTM,
  writeTVarSTM,
 )
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (MonadPlus (..), join)
import Trellis.CPS (CPS (..), reset, shift)
import qualified Trellis.CPS as CPS
import qualified Trellis.HIO as HIO
import Trellis.HIO (HIO)

import System.IO.Unsafe (unsafePerformIO)

{- | Orphan, but scoped to the one concrete pairing 'Orc' actually is -
'Trellis.CPS' deliberately has no 'MonadIO' instance of its own (nothing
else vendored needs one), so this is the narrowest place to add it. [^2]
-}
instance MonadIO (CPS () HIO) where
  liftIO = CPS.lift . liftIO

-- | A monad for orchestrating concurrent computations via 'Trellis.HIO.HIO'.
type Orc = CPS () HIO

{- | Runs an 'Orc' computation, discarding the (many) results. Blocks until
every thread it forks has finished - see 'spawn' for a non-blocking
alternative suited to a subscription meant to run indefinitely.
-}
runOrc :: Orc a -> IO ()
runOrc p = HIO.runHIO (p # \_ -> return ())

{- | Forks @p@ as a child of @parent@ in a fresh 'HIO.Group', returning
that child immediately so the caller can later 'HIO.close' it to cancel
@p@ and everything it forked. 'HIO' threads the ambient group through
'>>=', so nested '<|>'\/'repeating'\/'eagerly' threads all inherit it -
one 'HIO.close' really does reach everything, at any depth.
-}
spawn :: HIO.Group -> Orc a -> IO HIO.Group
spawn parent p = do
  child <- HIO.newGroup `HIO.inGroup` parent
  _ <- fork (p # \_ -> return ()) `HIO.inGroup` child
  return child

-- | A fresh, parentless 'HIO.Group' - the root an application keeps
-- around for the lifetime of every 'spawn'ed background job.
newRootGroup :: IO HIO.Group
newRootGroup = HIO.newPrimGroup

-- | Terminates an 'Orc' computation.
stop :: Orc a
stop = CPS $ \_ -> return ()

-- | @return ()@, placed at the end of an 'Orc' computation to signal it
-- has no more values to produce.
signal :: Orc ()
signal = return ()

{- | Parallel choice: performs the actions of @p@ and @q@, returning their
results as they become available, in unspecified order. Also '<|>'.
-}
par :: Orc a -> Orc a -> Orc a
par = (<|>)

{- | Immediately forks @p@ and returns a handle to its first result. The
handle itself blocks when invoked; 'eagerly' does not.
-}
eagerly :: Orc a -> Orc (Orc a)
eagerly p = CPS $ \k -> do
  res <- newEmptyMVar
  w <- HIO.newGroup
  threadId <- fork $ p `saveOnce` (res, w)
  _ <- HIO.local w $ return threadId
  k (liftIO $ readMVar res)

-- | Waits for @p@'s first result, then kills @p@ and any remaining work.
cut :: Orc a -> Orc a
cut = join . eagerly

{- | ("and-then") Performs and returns all of @p@'s results first, then
@q@'s.
-}
(<+>) :: Orc a -> Orc a -> Orc a
p <+> q = CPS $ \k -> do
  w <- HIO.newGroup
  threadId <- fork (p # k)
  _ <- HIO.local w $ return threadId
  HIO.finished w
  q # k

-- | ("or-else") @p@'s results, or @q@'s if @p@ produced none.
(<?>) :: Orc a -> Orc a -> Orc a
p <?> q = do
  tripwire <- newEmptyMVar
  do
    x <- p
    _ <- tryPutMVar tripwire ()
    return x
    <+> do
      triggered <- tryTakeMVar tripwire
      case triggered of
        Nothing -> q
        Just _ -> stop

saveOnce :: Orc a -> (MVar a, HIO.Group) -> HIO ()
p `saveOnce` (r, w) = do
  ticket <- newMVar ()
  p # \x -> liftIO (takeMVar ticket >> putMVar r x >> HIO.close w)

{- | Reads from @vals@ until @j@ values have been read or it's exhausted
(a 'Nothing'); fills @end@ once there are no more values.
-}
echo :: Int -> MVar (Maybe a) -> MVar () -> Orc a
echo 0 _ end = silent (putMVar end ())
echo j vals end = do
  mx <- takeMVar vals
  case mx of
    Nothing -> silent (putMVar end ())
    Just x -> return x <|> echo (j - 1) vals end

-- | Executes @p@ but suppresses its results.
silent :: Orc a -> Orc b
silent p = p >> stop

-- | Runs @p@ and @done@; once @done@ produces a result, kills both and
-- returns it, discarding @p@'s results.
onlyUntil :: Orc a -> Orc b -> Orc b
p `onlyUntil` done = cut (silent p <|> done)

{- | Runs @p@; if it hasn't produced a result within @t@ seconds, also
runs @q@ and returns whichever finishes first (killing the other).
-}
butAfter :: (RealFrac n) => Orc a -> (n, Orc a) -> Orc a
p `butAfter` (t, def) = cut (p <|> (delay t >> def))

-- | Runs @p@ and returns its first result, but not before @w@ seconds.
notBefore :: Orc a -> Float -> Orc a
p `notBefore` w = sync const p (delay w)

-- | Wait for @w@ seconds before continuing. [^3]
delay :: (RealFrac n) => n -> Orc ()
delay w = liftIO (threadDelay (round (w * 1000000)))

-- | Runs @p@ and @q@ in parallel, applying @f@ to their first results.
sync :: (a -> b -> c) -> Orc a -> Orc b -> Orc c
sync f p q = do
  po <- eagerly p
  qo <- eagerly q
  f <$> po <*> qo

{- | Forks @p@ and returns a lazy thunk of its single (trimmed) result. Use
with 'publish' when the value needs to be forced before proceeding.
-}
val :: Orc a -> Orc a
val p = CPS $ \k -> do
  res <- newEmptyMVar
  w <- HIO.newGroup
  threadId <- fork $ p `saveOnce` (res, w)
  _ <- HIO.local w $ return threadId
  k (unsafePerformIO $ readMVar res)

-- | A hyperstrict 'return', for synchronizing on multiple 'val' results.
publish :: (NFData a) => a -> Orc a
publish x = deepseq x $ return x

{- | Repeats @p@ and returns its results. Best with a single-valued @p@ -
a multi-valued @p@ spawns a repeating thread per result. [^4]
-}
repeating :: Orc a -> Orc a
repeating p = do
  x <- p
  return x <|> repeating p

-- | Puts @p@'s results (tagged 'Just') into @vals@ until @end@ is filled.
sandbox :: Orc a -> MVar (Maybe a) -> MVar () -> Orc ()
sandbox p vals end =
  ((p >>= (putMVar vals . Just)) <+> putMVar vals Nothing)
    `onlyUntil` takeMVar end

-- | Runs @p@ and returns its first @n@ results.
takeOrc :: Int -> Orc a -> Orc a
takeOrc n p = do
  vals <- newEmptyMVar
  end <- newEmptyMVar
  echo n vals end <|> silent (sandbox p vals end)

-- | Drops @p@'s first @n@ results, returning the rest.
dropOrc :: Int -> Orc a -> Orc a
dropOrc n p = do
  countdown <- newTVar n
  x <- p
  join $ atomically $ do
    w <- readTVarSTM countdown
    if w == 0
      then return $ return x
      else do
        writeTVarSTM countdown (w - 1)
        return stop

-- | Zips @p@ and @q@'s results; when one finishes, kills the other.
zipOrc :: Orc a -> Orc b -> Orc (a, b)
zipOrc p q = do
  pvals <- newEmptyMVar
  qvals <- newEmptyMVar
  end <- newEmptyMVar
  zipp pvals qvals end
    <|> silent (sandbox p pvals end)
    <|> silent (sandbox q qvals end)

zipp :: MVar (Maybe a) -> MVar (Maybe b) -> MVar () -> Orc (a, b)
zipp pvals qvals end = do
  mx <- takeMVar pvals
  my <- takeMVar qvals
  case mx of
    Nothing -> silent (putMVar end () >> putMVar end ())
    Just x -> case my of
      Nothing -> silent (putMVar end () >> putMVar end ())
      Just y -> return (x, y) <|> zipp pvals qvals end

-- | Collects all of @p@'s values into a list once @p@ completes.
collect :: Orc a -> Orc [a]
collect p = do
  accum <- newTVar []
  silent (p >>= \v -> modifyTVar accum (v :)) <+> readTVar accum

-- | Runs @p@ and writes its results to the channel @ch@.
runChan :: Chan a -> Orc a -> IO ()
runChan ch p = runOrc (p >>= writeChan ch)

-- | Runs @ps@ in parallel until each produces its first result.
syncList :: [Orc a] -> Orc [a]
syncList ps = mapM eagerly ps >>= sequence

-- | Combines the results of @p@ with a running state, left-to-right in
-- whatever nondeterministic order 'Orc' produces them.
scan :: (a -> s -> s) -> s -> Orc a -> Orc s
scan f s p = do
  accum <- newTVar s
  x <- p
  (_w, w') <- modifyTVar accum (f x)
  return w'

liftList :: (MonadPlus list) => [a] -> list a
liftList = foldr (mplus . return) mzero

{- [^1]:
Established earlier in this session: 'Trellis.UI.Action's continuation is
rank-2 quantified (@forall r.@) and threaded through a 'Comonad' space, so
it can only ever be invoked once, in place. 'Orc's '<|>' literally forks
the same continuation down two branches (see its definition below) -
'Action' structurally cannot do that, and was never meant to.
-}

{- [^2]:
Without this, none of 'Orc's 'liftIO'-based combinators ('delay',
'eagerly', ...) type-check - garden's own @CPS.hs@ has this instance,
'Trellis.CPS' deliberately doesn't (it @hiding (liftIO)@s it on import),
since nothing else vendored so far needed it.
-}

{- [^3]:
Garden's original also printed a "did you mean this many seconds?"
sanity check to stdout for waits over 100 seconds. Dropped along with
'putStrLine'\/'printOrc'\/'prompt' - anything writing straight to stdout
would corrupt termbox2's raw\/alt-screen display.
-}

{- [^4]:
"This behavior may not be intentional" per the original - kept as an
honest warning, not a bug we introduced.
-}
