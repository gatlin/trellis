{- |
Module: Update.Subscriptions
Description: Draining live-cell input and publishing "--out" cells,
plus the shared editor-modal bookkeeping ('existingText',
'cancelSubscription') both editing and clearing a cell rely on.
-}
module Update.Subscriptions (
  existingText,
  cancelSubscription,
  drainLiveUpdates,
  publishOutUpdates,
) where

import Control.Concurrent.STM.MonadIO (
  TVar,
  atomically,
  readTVarSTM,
  writeTVarSTM,
 )
import Control.Monad (forM_, unless, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Formula (Expr, blank, evaluated, renderExpr, showValue, window)
import Live (OutBinding (..), enqueueOut)
import SheetState (LiveBinding (..), SheetState (..))
import qualified Trellis.Orc as Orc
import qualified Trellis.UI as UI

{- | What to pre-fill an edit with: a live spec's own typed text if the
cell is subscribed, otherwise its formula rendered back to text.
-}
existingText :: SheetState -> (Int, Int) -> String
existingText st pos = case Map.lookup pos (subscriptions st) of
  Just b -> liveSpecText b
  Nothing -> renderExpr (Map.findWithDefault blank pos (cells st))

-- | Kills a cell's subscription, if it has one, and forgets it.
cancelSubscription :: (Int, Int) -> UI.Action (UI.Store SheetState) IO ()
cancelSubscription pos = do
  st <- UI.get
  case Map.lookup pos (subscriptions st) of
    Nothing -> return ()
    Just b -> do
      UI.liftIO (Orc.close (liveGroup b))
      UI.modify
        ( \st' ->
            st'{subscriptions = Map.delete pos (subscriptions st')}
        )

-- | Applies whatever live/async results have arrived since the last tick.
drainLiveUpdates ::
  TVar [((Int, Int), Expr)] -> UI.Action (UI.Store SheetState) IO ()
drainLiveUpdates mailbox = do
  updates <- UI.liftIO $ atomically $ do
    xs <- readTVarSTM mailbox
    writeTVarSTM mailbox []
    return xs
  unless (null updates) $
    UI.modify
      ( \st ->
          st{cells = foldr (uncurry Map.insert) (cells st) updates}
      )

{- | Publishes each "--out" cell's current value to its pipe, but only when
it's actually changed since the last publish - otherwise every tick would
write the same value again, whether or not anything happened.
-}
publishOutUpdates :: [OutBinding] -> UI.Action (UI.Store SheetState) IO ()
publishOutUpdates outs = do
  st <- UI.get
  let vals = evaluated (cells st)
  forM_ outs $ \b -> do
    let str =
          maybe "" showValue (listToMaybe (concat (window (outPos b) 1 1 vals)))
    UI.liftIO $ do
      changed <- atomically $ do
        prev <- readTVarSTM (outLast b)
        if prev == Just str
          then return False
          else writeTVarSTM (outLast b) (Just str) >> return True
      when changed (enqueueOut b str)
