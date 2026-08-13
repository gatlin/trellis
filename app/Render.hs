{- |
Module: Render
Description: Drawing the sheet, its grid, and the formula-editing modal.
-}
module Render (
  render,
  sheetOutputMode,
) where

import Render.Grid (render)
import Render.Theme (sheetOutputMode)
