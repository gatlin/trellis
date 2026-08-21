{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module: Trellis.UI.Backend
Description: The backend-neutral event/key/color vocabulary every
'Trellis.UI' caller (and every 'Trellis.UI.Screen' implementation) shares.

Deliberately depends on nothing backend-specific - no termbox2, no future
canvas/JS binding - so it compiles identically under every target.
Each concrete 'Trellis.UI.Screen' is responsible for translating its own
real events/colors into this vocabulary (and back, for colors), not the
other way around. Newtypes and named constants mirror the shape the
codebase already had via termbox2-hs closely (same derived typeclasses, so
@.&.@-style modifier checks, @\<\>@-combined colors, and raw-integer-literal
color usage all keep working unchanged at every call site) - this is a
relocation, not a redesign.
-}
module Trellis.UI.Backend (
  -- Keys
  Key,
  keyNone,
  keyArrowUp,
  keyArrowDown,
  keyArrowLeft,
  keyArrowRight,
  keyCtrlA,
  keyCtrlB,
  keyCtrlC,
  keyCtrlD,
  keyCtrlE,
  keyCtrlF,
  keyCtrlG,
  keyCtrlH,
  keyCtrlI,
  keyCtrlJ,
  keyCtrlK,
  keyCtrlL,
  keyCtrlM,
  keyCtrlN,
  keyCtrlO,
  keyCtrlP,
  keyCtrlQ,
  keyCtrlR,
  keyCtrlS,
  keyCtrlT,
  keyCtrlU,
  keyCtrlV,
  keyCtrlW,
  keyCtrlX,
  keyCtrlY,
  keyCtrlZ,
  keyCtrlEnter,
  keyCtrlEsc,
  keyCtrlTab,
  keyBackTab,
  keyDelete,
  keyHome,
  keyEnd,
  keyPgUp,
  keyPgDn,
  keyF2,
  keySpace,
  keyBackspace,
  keyBackspace2,
  keyMouseLeft,
  keyMouseRight,
  keyMouseMiddle,
  keyMouseRelease,
  keyMouseWheelUp,
  keyMouseWheelDown,

  -- Modifiers
  Modifiers,
  modAlt,
  modCtrl,
  modShift,
  modMotion,

  -- Event type
  EventType,
  eventKey,
  eventMouse,

  -- Input event
  InputEvent (..),

  -- Colors
  Color,
  colorBlack,
  colorGreen,
  colorMagenta,
  colorWhite,
  colorDefault,
  attrBold,
  attrUnderline,

  -- Output/input modes
  OutputMode,
  output256,
  InputMode,
  inputEsc,
  inputMouse,
) where

import Data.Bits (Bits, (.|.))
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Types (CInt)

-- | A named key: an arrow, a Ctrl+letter combo, Delete, and so on. Zero
-- ('keyNone') is reserved to mean "this event carries an ordinary typed
-- character instead" - see 'InputEvent'.
newtype Key = Key Word16
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

keyNone :: Key
keyNone = 0

keyArrowUp, keyArrowDown, keyArrowLeft, keyArrowRight :: Key
keyArrowUp = 1
keyArrowDown = 2
keyArrowLeft = 3
keyArrowRight = 4

-- | Ctrl+letter, A through Z - 'Keymap.namedCtrlKey' maps a typed 'Char'
-- onto these.
keyCtrlA, keyCtrlB, keyCtrlC, keyCtrlD, keyCtrlE, keyCtrlF, keyCtrlG :: Key
keyCtrlH, keyCtrlI, keyCtrlJ, keyCtrlK, keyCtrlL, keyCtrlM, keyCtrlN :: Key
keyCtrlO, keyCtrlP, keyCtrlQ, keyCtrlR, keyCtrlS, keyCtrlT, keyCtrlU :: Key
keyCtrlV, keyCtrlW, keyCtrlX, keyCtrlY, keyCtrlZ :: Key
keyCtrlA = 10
keyCtrlB = 11
keyCtrlC = 12
keyCtrlD = 13
keyCtrlE = 14
keyCtrlF = 15
keyCtrlG = 16
keyCtrlH = 17
keyCtrlI = 18
keyCtrlJ = 19
keyCtrlK = 20
keyCtrlL = 21
keyCtrlM = 22
keyCtrlN = 23
keyCtrlO = 24
keyCtrlP = 25
keyCtrlQ = 26
keyCtrlR = 27
keyCtrlS = 28
keyCtrlT = 29
keyCtrlU = 30
keyCtrlV = 31
keyCtrlW = 32
keyCtrlX = 33
keyCtrlY = 34
keyCtrlZ = 35

keyCtrlEnter, keyCtrlEsc, keyCtrlTab, keyBackTab :: Key
keyCtrlEnter = 40
keyCtrlEsc = 41
keyCtrlTab = 42
keyBackTab = 43

keyDelete, keyHome, keyEnd, keyPgUp, keyPgDn, keyF2, keySpace :: Key
keyDelete = 50
keyHome = 51
keyEnd = 52
keyPgUp = 53
keyPgDn = 54
keyF2 = 55
keySpace = 56

keyBackspace, keyBackspace2 :: Key
keyBackspace = 60
keyBackspace2 = 61

keyMouseLeft, keyMouseRight, keyMouseMiddle, keyMouseRelease :: Key
keyMouseWheelUp, keyMouseWheelDown :: Key
keyMouseLeft = 70
keyMouseRight = 71
keyMouseMiddle = 72
keyMouseRelease = 73
keyMouseWheelUp = 74
keyMouseWheelDown = 75

-- | A 'InputEvent's modifier bitset - Alt\/Ctrl\/Shift\/mid-drag ("motion").
-- Numerically identical to termbox2's own @TB_MOD_*@ bit assignments,
-- since these are a genuinely portable, standard flag layout worth
-- reusing verbatim rather than an implementation detail to hide.
newtype Modifiers = Modifiers Word8
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits)

modAlt, modCtrl, modShift, modMotion :: Modifiers
modAlt = 1
modCtrl = 2
modShift = 4
modMotion = 8

-- | Which of 'InputEvent's fields are meaningful - a key\/char event, or a
-- mouse event.
newtype EventType = EventType Word8
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

eventKey, eventMouse :: EventType
eventKey = 1
eventMouse = 3

{- | One backend-neutral input event. @evtKey@ is 'keyNone' when @evtCh@
carries an ordinary typed character instead (and vice versa) - the same
XOR convention every caller ('Keymap.matches', 'Update.Events.printableChar',
...) already relies on.
-}
data InputEvent = InputEvent
  { evtType :: EventType
  , evtMod :: Modifiers
  , evtKey :: Key
  , evtCh :: Word32
  , evtW, evtH, evtX, evtY :: Int32
  }
  deriving (Show, Eq)

{- | A color, or a bold\/underline attribute flag, OR-combined via '<>' -
low bits are an ANSI-256 palette index (raw integer literals like
@textFg = 15@ mean exactly what they'd mean in any ANSI-256-aware
terminal), high bits are attribute flags. Numerically identical to
termbox2's own @TB_*@ values - like 'Modifiers', this is a genuinely
portable standard (ANSI-256), not termbox2-specific data.
-}
newtype Color = Color CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits)

instance Semigroup Color where
  (<>) = (.|.)

colorBlack, colorGreen, colorMagenta, colorWhite, colorDefault :: Color
colorBlack = 1
colorGreen = 3
colorMagenta = 6
colorWhite = 8
colorDefault = 0x2000

attrBold, attrUnderline :: Color
attrBold = 0x0100
attrUnderline = 0x0200

-- | Which color-numbering scheme 'Color' values are drawn from - only
-- 'output256' is used anywhere in this codebase.
newtype OutputMode = OutputMode CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

output256 :: OutputMode
output256 = 2

-- | Input features to enable, OR-combined via '<>'.
newtype InputMode = InputMode CInt
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Bits)

instance Semigroup InputMode where
  (<>) = (.|.)

inputEsc, inputMouse :: InputMode
inputEsc = 1
inputMouse = 4
