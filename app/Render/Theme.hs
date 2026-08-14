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

import qualified Termbox2 as Tb2

{- | One global setting for the whole app, not per-call - every color used
anywhere must agree on the same mode's numbering. 256 over grayscale:
grayscale has no white for text or black for the focus highlight.
-}
sheetOutputMode :: Tb2.Tb2Output
sheetOutputMode = Tb2.output256

-- | Ordinary text: header numbers, row numbers, unfocused cell content.
textFg, textBg :: Tb2.Tb2ColorAttr
textFg = 15 -- bright white; plain "7" reads as a dull grey on many terminals
textBg = Tb2.colorDefault

-- | The focused cell: inverted, same as before, just re-picked for 256 mode.
focusFg, focusBg :: Tb2.Tb2ColorAttr
focusFg = 0 -- black
focusBg = 15 -- bright white

{- | The grid's ruling wants to recede behind the cell text, not compete
with it. 240 is a dark-mid grey: visible without shouting.
-}
gridFg, gridBg :: Tb2.Tb2ColorAttr
gridFg = 240
gridBg = Tb2.colorDefault

-- | A cell fed by a live\/async 'Live.LiveSpec' subscription, when unfocused.
liveFg, liveBg :: Tb2.Tb2ColorAttr
liveFg = 51 -- bright cyan
liveBg = Tb2.colorDefault

{- | A cell published out to a pipe\/file ("--out"), when unfocused and
not itself also a live subscription (which takes visual precedence).
-}
outFg, outBg :: Tb2.Tb2ColorAttr
outFg = 201 -- bright magenta
outBg = Tb2.colorDefault

{- | The rectangle an in-progress fill drag is about to replicate over -
needs its own hue, distinct from 'liveFg'\/'outFg', since a destination
cell can already be live\/published and the preview should still read
clearly while the drag is happening.
-}
fillFg, fillBg :: Tb2.Tb2ColorAttr
fillFg = 226 -- bright yellow
fillBg = Tb2.colorDefault

{- | A row\/column selection made with 'selectButton', once released -
its own hue, distinct from every other tier (cyan live, magenta out,
yellow fill-preview), so a completed selection still reads clearly on
its own.
-}
selectionFg, selectionBg :: Tb2.Tb2ColorAttr
selectionFg = 0 -- black
selectionBg = 33 -- bright blue

{- | The bar\/line chart panel's own content color - a classic terminal-plot
green, distinct from every other tier above.
-}
chartFg, chartBg :: Tb2.Tb2ColorAttr
chartFg = 46
chartBg = Tb2.colorDefault

{- | A small categorical palette a bar chart's bars cycle through by
position, so adjacent bars read as distinct at a glance instead of one
same-colored shape repeated - unlike 'chartFg', which the line chart
keeps using uniformly, since a multi-colored line reads as noise rather
than signal.
-}
chartPalette :: [Tb2.Tb2ColorAttr]
chartPalette = [46, 51, 226, 208, 213, 39]

{- | A heatmap's cold-to-hot gradient, each step's colors chosen together
so the text stays legible against its own background - the same reasoning
'focusFg'\/'focusBg' already follows. Indexed by 'SheetState.heatmapStep'.
-}
heatmapRamp :: [(Tb2.Tb2ColorAttr, Tb2.Tb2ColorAttr)]
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
modalBorderFg, modalBorderBg :: Tb2.Tb2ColorAttr
modalBorderFg = 15
modalBorderBg = Tb2.colorDefault

{- | Breathing room between the editor's border and its text, so it reads
as a considered dialog rather than text jammed against a frame.
-}
modalHPad, modalVPad :: Int
modalHPad = 2
modalVPad = 1

-- | Pad or truncate a string to an exact width.
padTo :: Int -> String -> String
padTo width str = take width (str ++ repeat ' ')
