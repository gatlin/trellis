{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module: Live.Out
Description: wasm32-wasi backend - no real filesystem exists in a browser
sandbox to write an out-binding's value to (same underlying limitation
"Live.In"'s @!tail@ has), but the value itself is still real and useful
here: dispatched directly, synchronously, as a JS callback (the wasm
component's @watchOut@\/@cell-out@ - see @wasm\/trellis-sheet.js@) rather
than queued for a nonexistent background writer thread. There's no
'Trellis.Orc.spawn' on this backend to build a writer thread out of even
if one were wanted (see @src-wasi\/Trellis\/Orc.hs@'s own haddock) - and
none is needed, since a JS callback dispatch can never block the way a
file\/pipe write could, so the whole point of native's background-thread
design doesn't apply here.
-}
module Live.Out (
  OutBinding (..),
  declareOutBinding,
  enqueueOut,
) where

import Control.Concurrent.MonadIO (
  MVar,
  newEmptyMVar,
 )
import Control.Concurrent.STM.MonadIO (TVar, newTVar)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Word (Word8)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, ptrToIntPtr)
import qualified Trellis.Orc as Orc

{- | A cell watched for changes, dispatched to JS whenever its value
differs from what was last dispatched - 'outBox' is kept only for type
compatibility with the shared 'Update.Subscriptions.publishOutUpdates'
(which constructs/reads 'OutBinding' generically across both backends);
nothing on this backend ever reads or writes it, since 'enqueueOut'
dispatches directly and synchronously rather than queuing for a
background reader that doesn't exist here.
-}
data OutBinding = OutBinding
  { outPos :: (Int, Int)
  , outBox :: MVar String
  , outLast :: TVar (Maybe String)
  }

declareOutBinding :: Orc.Group -> (Int, Int) -> FilePath -> IO OutBinding
declareOutBinding _root pos _path = do
  box <- newEmptyMVar
  lastVal <- newTVar Nothing
  return (OutBinding pos box lastVal)

{- | Hand-rolled UTF-8 - see "Live.In"'s module haddock for why this
doesn't reach for 'GHC.IO.Encoding' on this backend. Duplicated rather
than shared: both copies are small, stable, and self-contained, and
factoring them out isn't worth a new shared module yet.
-}
utf8Encode :: String -> [Word8]
utf8Encode = concatMap encodeChar
 where
  encodeChar c
    | n < 0x80 = [fromIntegral n]
    | n < 0x800 =
        [ 0xC0 .|. fromIntegral (n `shiftR` 6)
        , contByte n
        ]
    | n < 0x10000 =
        [ 0xE0 .|. fromIntegral (n `shiftR` 12)
        , contByte (n `shiftR` 6)
        , contByte n
        ]
    | otherwise =
        [ 0xF0 .|. fromIntegral (n `shiftR` 18)
        , contByte (n `shiftR` 12)
        , contByte (n `shiftR` 6)
        , contByte n
        ]
   where
    n = fromEnum c
  contByte n = 0x80 .|. (fromIntegral n .&. 0x3F)

ptrArg :: Ptr a -> Int
ptrArg = fromIntegral . ptrToIntPtr

{- | Calls @window.__trellisOutCallback@ - set up per-instance by
@wasm\/trellis-sheet.js@'s "active instance" swap, the same mechanism
@window.trellisHost@\/@window.__trellisTick@ already use (see
@src-wasi\/Trellis\/UI\/Screen.hs@) - so a page with more than one
@\<trellis-sheet\>@ still routes each dispatch to the right instance.
A no-op if nothing's listening (native's own no-op-when-nothing-cares
default, unaffected).
-}
foreign import javascript unsafe
  "const row = $1, col = $2, ptr = $3, len = $4; \
  \const value = new TextDecoder().decode(new Uint8Array(__exports.memory.buffer, ptr, len)); \
  \if (window.__trellisOutCallback) window.__trellisOutCallback(row, col, value);"
  js_dispatchOut :: Int -> Int -> Int -> Int -> IO ()

enqueueOut :: OutBinding -> String -> IO ()
enqueueOut b val = do
  -- \| outPos is (col, row) - SheetState/Formula's own internal
  -- convention throughout (see app-wasi/Main.hs's trellisWatchOut for
  -- the fuller note) - the public JS API's cell-out event reports
  -- {row, col, ...} instead, the more intuitive order, so this swaps
  -- back at the boundary rather than leaking the internal convention.
  let (col, row) = outPos b
      bytes = utf8Encode val
  withArrayLen bytes $ \len ptr ->
    js_dispatchOut row col (ptrArg ptr) len
