{- |
Module: Update
Description: Event handling: navigation, zoom, pan, the CPS-based
formula-editing modal, and draining/publishing live cell subscriptions.
-}
module Update (
  update,
  isKey,
  isMouse,
  dragging,
  printableChar,
  isValid,
  insertAt,
  deleteBefore,
  deleteAt,
  clampCursor,
) where

import Update.Buffer (clampCursor, deleteAt, deleteBefore, insertAt, isValid)
import Update.Core (update)
import Update.Events (dragging, isKey, isMouse, printableChar)
