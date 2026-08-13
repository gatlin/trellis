{- |
Module: Live
Description: Cells fed by a live subscription, or publishing their value
out to a pipe - see "Live.In" and "Live.Out".
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

import Live.In (
  LiveSpec (..),
  declareSubscription,
  literal,
  parseLiveSpec,
  toOrc,
 )
import Live.Out (OutBinding (..), declareOutBinding, enqueueOut)
