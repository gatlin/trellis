{- |
Module: Render.Theme
Description: Shared colors and small drawing helpers every other
Render module draws from.
-}
module Render.Theme (
  sheetOutputMode,
  textFg,
  textBg,
  focusFg,
  focusBg,
  gridFg,
  gridBg,
  liveFg,
  liveBg,
  outFg,
  outBg,
  fillFg,
  fillBg,
  selectionFg,
  selectionBg,
  numFg,
  numBg,
  strFg,
  strBg,
  boolFg,
  boolBg,
  dateFg,
  dateBg,
  errFg,
  errBg,
  valueColors,
  chartFg,
  chartBg,
  chartPalette,
  heatmapRamp,
  modalBorderFg,
  modalBorderBg,
  modalHPad,
  modalVPad,
  padTo,
) where

import Formula (Value (..))
import qualified Trellis.UI as UI

{- | One global setting for the whole app, not per-call - every color used
anywhere must agree on the same mode's numbering. 256 over grayscale:
grayscale has no white for text or black for the focus highlight.
-}
sheetOutputMode :: UI.OutputMode
sheetOutputMode = UI.output256

-- | Ordinary text: header numbers, row numbers, unfocused cell content.
textFg, textBg :: UI.Color
textFg = 15 -- bright white; plain "7" reads as a dull grey on many terminals
textBg = UI.colorDefault

-- | The focused cell: inverted, same as before, just re-picked for 256 mode.
focusFg, focusBg :: UI.Color
focusFg = 0 -- black
focusBg = 15 -- bright white

{- | The grid's ruling wants to recede behind the cell text, not compete
with it. 240 is a dark-mid grey: visible without shouting.
-}
gridFg, gridBg :: UI.Color
gridFg = 240
gridBg = UI.colorDefault

-- | A cell fed by a live\/async 'Live.LiveSpec' subscription, when unfocused.
liveFg, liveBg :: UI.Color
liveFg = 51 -- bright cyan
liveBg = UI.colorDefault

{- | A cell published out to a pipe\/file ("--out"), when unfocused and
not itself also a live subscription (which takes visual precedence).
-}
outFg, outBg :: UI.Color
outFg = 201 -- bright magenta
outBg = UI.colorDefault

{- | The rectangle an in-progress fill drag is about to replicate over -
needs its own hue, distinct from 'liveFg'\/'outFg', since a destination
cell can already be live\/published and the preview should still read
clearly while the drag is happening.
-}
fillFg, fillBg :: UI.Color
fillFg = 226 -- bright yellow
fillBg = UI.colorDefault

{- | A row\/column selection made with 'selectButton', once released -
its own hue, distinct from every other tier (cyan live, magenta out,
yellow fill-preview), so a completed selection still reads clearly on
its own.
-}
selectionFg, selectionBg :: UI.Color
selectionFg = 0 -- black
selectionBg = 33 -- bright blue

-- | Per-type cell colors: each 'Value' constructor gets its own fg/bg
-- pair so the sheet is scannable at a glance.

{- | Numbers: a warm, distinct hue that reads as "data" without
competing with the special-purpose tiers above.
-}
numFg, numBg :: UI.Color
numFg = 222 -- bright orange
numBg = UI.colorDefault

{- | Strings: high-contrast "header" treatment - bright text on a dark
background, visually distinct from every other type.
-}
strFg, strBg :: UI.Color
strFg = 15 -- bright white
strBg = 235 -- dark grey

{- | Booleans: a clear green, distinct from the chart green (46) and
the live-cell cyan (51).
-}
boolFg, boolBg :: UI.Color
boolFg = 114 -- bright green
boolBg = UI.colorDefault

{- | Dates: a cool blue, distinct from the live-cell cyan (51) and
the selection blue (33).
-}
dateFg, dateBg :: UI.Color
dateFg = 117 -- bright blue
dateBg = UI.colorDefault

{- | Errors: unmistakable red, the one color that should always demand
attention.
-}
errFg, errBg :: UI.Color
errFg = 196 -- bright red
errBg = UI.colorDefault

-- | Map an evaluated 'Value' to its display color pair.
valueColors :: Value -> (UI.Color, UI.Color)
valueColors VBlank = (textFg, textBg)
valueColors (VNum _) = (numFg, numBg)
valueColors (VStr _) = (strFg, strBg)
valueColors (VBool _) = (boolFg, boolBg)
valueColors (VDate _) = (dateFg, dateBg)
valueColors (VErr _) = (errFg, errBg)

{- | The bar\/line chart panel's own content color - a classic terminal-plot
green, distinct from every other tier above.
-}
chartFg, chartBg :: UI.Color
chartFg = 46
chartBg = UI.colorDefault

{- | A small categorical palette a bar chart's bars cycle through by
position, so adjacent bars read as distinct at a glance instead of one
same-colored shape repeated - unlike 'chartFg', which the line chart
keeps using uniformly, since a multi-colored line reads as noise rather
than signal.
-}
chartPalette :: [UI.Color]
chartPalette = [46, 51, 226, 208, 213, 39]

{- | A heatmap's cold-to-hot gradient, each step's colors chosen together
so the text stays legible against its own background - the same reasoning
'focusFg'\/'focusBg' already follows. Indexed by 'SheetState.heatmapStep'.
-}
heatmapRamp :: [(UI.Color, UI.Color)]
heatmapRamp =
  [ (15, 27) -- white on blue: coolest
  , (0, 51) -- black on cyan
  , (0, 46) -- black on green
  , (0, 226) -- black on yellow
  , (15, 196) -- white on red: hottest
  ]

{- | The opposite of the grid's ruling: a dialog demanding attention, so
drawn bright ('textFg's white) with a heavy line weight rather than a new
hue - grabs the eye without adding a color to the palette.
-}
modalBorderFg, modalBorderBg :: UI.Color
modalBorderFg = 15
modalBorderBg = UI.colorDefault

{- | Breathing room between the editor's border and its text, so it reads
as a considered dialog rather than text jammed against a frame.
-}
modalHPad, modalVPad :: Int
modalHPad = 2
modalVPad = 1

-- | Pad or truncate a string to an exact width.
padTo :: Int -> String -> String
padTo width str = take width (str ++ repeat ' ')
