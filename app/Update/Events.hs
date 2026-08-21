{- |
Module: Update.Events
Description: Pure predicates over input events.
-}
module Update.Events (
  isKey,
  isShiftKey,
  isMouse,
  dragging,
  printableChar,
) where

import Data.Bits ((.&.))
import qualified Trellis.UI as UI

-- | Does an incoming event carry a given key, of the given event type?
isKey :: UI.InputEvent -> UI.Key -> Bool
isKey evt k = UI.evtType evt == UI.eventKey && UI.evtKey evt == k

{- | Does an incoming key event carry a given key with Shift held -
termbox2 only ever sets this modifier on the arrow keys (a bare
character key like 'k' held with Shift just arrives as 'K', a different
character, not a modifier on 'k'), so this is only meaningful for those.
-}
isShiftKey :: UI.InputEvent -> UI.Key -> Bool
isShiftKey evt k = isKey evt k && UI.evtMod evt .&. UI.modShift /= 0

isMouse :: UI.InputEvent -> UI.Key -> Bool
isMouse evt k = UI.evtType evt == UI.eventMouse && UI.evtKey evt == k

-- | Is this mouse event part of an in-progress drag?
dragging :: UI.InputEvent -> Bool
dragging evt = UI.evtMod evt .&. UI.modMotion /= 0

{- | A key event carrying an ordinary printable character rather than a
named key: termbox2 reports these with no key code, just a Unicode
codepoint. Not a binding - a typed character always means itself.
-}
printableChar :: UI.InputEvent -> Maybe Char
printableChar evt
  | UI.evtType evt == UI.eventKey
  , UI.evtKey evt == UI.keyNone
  , UI.evtCh evt /= 0 =
      Just (toEnum (fromIntegral (UI.evtCh evt)) :: Char)
  | otherwise = Nothing
