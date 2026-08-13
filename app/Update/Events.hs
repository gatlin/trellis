{- |
Module: Update.Events
Description: Pure predicates over termbox2 input events.
-}
module Update.Events (
  isKey,
  isMouse,
  dragging,
  printableChar,
) where

import Data.Bits ((.&.))
import qualified Termbox2 as Tb2

-- | Does an incoming event carry a given key, of the given event type?
isKey :: Tb2.Tb2Event -> Tb2.Tb2Key -> Bool
isKey evt k = Tb2._type evt == Tb2.eventKey && Tb2._key evt == k

isMouse :: Tb2.Tb2Event -> Tb2.Tb2Key -> Bool
isMouse evt k = Tb2._type evt == Tb2.eventMouse && Tb2._key evt == k

-- | Is this mouse event part of an in-progress drag?
dragging :: Tb2.Tb2Event -> Bool
dragging evt = Tb2._mod evt .&. Tb2.modMotion /= 0

{- | A key event carrying an ordinary printable character rather than a
named key: termbox2 reports these with no key code, just a Unicode
codepoint. Not a binding - a typed character always means itself.
-}
printableChar :: Tb2.Tb2Event -> Maybe Char
printableChar evt
  | Tb2._type evt == Tb2.eventKey
  , Tb2._key evt == 0
  , Tb2._ch evt /= 0 =
      Just (toEnum (fromIntegral (Tb2._ch evt)) :: Char)
  | otherwise = Nothing
