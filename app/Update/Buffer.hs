{- |
Module: Update.Buffer
Description: Pure formula-editor buffer editing: validation and
cursor-relative insert/delete.
-}
module Update.Buffer (
  isValid,
  insertAt,
  deleteBefore,
  deleteAt,
  clampCursor,
) where

import Parser (parseExpr)

-- | Does a formula editor's buffer currently parse?
isValid :: String -> Bool
isValid buf = case parseExpr buf of
  Right _ -> True
  Left _ -> False

-- | Keep a cursor position within the bounds of a buffer.
clampCursor :: Int -> String -> Int
clampCursor c s = max 0 (min (length s) c)

{- | Insert a character at a cursor position, returning the new buffer and
the cursor position just after the inserted character.
-}
insertAt :: Char -> Int -> String -> (String, Int)
insertAt c pos s =
  let (before, after) = splitAt pos s
   in (before ++ [c] ++ after, pos + 1)

{- | Delete the character just before a cursor position (backspace),
returning the new buffer and cursor. A no-op at position 0.
-}
deleteBefore :: Int -> String -> (String, Int)
deleteBefore pos s
  | pos <= 0 = (s, pos)
  | otherwise =
      let (before, after) = splitAt pos s
       in (init before ++ after, pos - 1)

{- | Delete the character just after a cursor position (forward-delete).
The cursor itself doesn't move; a no-op at the end of the buffer.
-}
deleteAt :: Int -> String -> (String, Int)
deleteAt pos s
  | pos >= length s = (s, pos)
  | otherwise =
      let (before, after) = splitAt pos s
       in (before ++ drop 1 after, pos)
