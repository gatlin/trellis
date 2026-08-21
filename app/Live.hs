{- |
Module: Live
Description: Cells fed by a live subscription, or publishing their value
out to a pipe - see "Live.In" and "Live.Out".
-}
module Live (
  LiveSpec (..),
  parseLiveSpec,
  literal,
  declareSubscription,
  OutBinding (..),
  declareOutBinding,
  enqueueOut,
) where

-- \| 'Live.In.toOrc' isn't re-exported here: it's a native-only
-- implementation detail (the 'Trellis.Orc.Orc' computation a 'LiveSpec'
-- runs, native's own orchestration monad) with no wasi equivalent - the
-- wasi 'Live.In' builds torc 'Observable's directly instead, and nothing
-- outside "Live.In" itself ever called 'toOrc' anyway (confirmed by grep).
import Live.In (
  LiveSpec (..),
  declareSubscription,
  literal,
  parseLiveSpec,
 )
import Live.Out (OutBinding (..), declareOutBinding, enqueueOut)
