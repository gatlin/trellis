# Plan

Ordered by priority. Each item lists scope, risk, and why it's in this position.

## 1. Select-and-delete should delete the selection

- **Scope:** The delete key handler currently clears only the active cell. It needs to iterate over the full selection rectangle (or row/column span) and clear each cell.
- **Risk:** Low. The selection geometry is already computed for fill and chart; we just need to reuse it in the delete path.
- **Why first:** Core interaction bug. Every multi-cell delete feels broken until this is fixed.

## 2. Ctrl+A / Ctrl+E in the formula editor

- **Scope:** Add two key bindings in the editor's key-dispatch to move the cursor to column 0 and to end-of-line.
- **Risk:** Very low. Two new cases in an existing `case` or `choice`.
- **Why second:** Trivial, unblocks a common editing workflow, no interaction with other features.

## 3. `RAND()` builtin

- **Scope:**
  - New `Call0F RandOp` (or similar) in the expression AST.
  - Parser entry: `randP = mk0 "RAND" (Expr (Call0F RandOp))`.
  - Evaluator clause: return `VNum` from `randomRIO (0.0, 1.0)`.
  - Add to the feature list in README.
- **Risk:** Low. One new constructor, one parser rule, one eval clause.
- **Why third:** Small, self-contained, gives users a tool for testing and prototyping.

## 4. Per-type cell colors

- **Scope:**
  - Define fg/bg pairs in `Render/Theme.hs` for each `Value` constructor: `VNum`, `VStr`, `VBool`, `VErr`, `VBlank`.
  - In the cell-rendering code, pattern-match on the evaluated `Value` and pick the appropriate color pair.
  - Strings should read as "headers" (the note in todo.txt suggests a bold or high-contrast treatment).
- **Risk:** Medium. Touches rendering and theme; need to verify contrast is readable across all 256-color terminals.
- **Why fourth:** Makes the sheet scannable at a glance. Doing it before #5 and #6 means new types (unquoted strings, dates) get their color treatment from day one.

## 5. Unquoted strings

- **Scope:**
  - Parser change: where an expression is expected, a bare word that is *not* a known function name, keyword, or cell reference should parse as `VStr`.
  - Must not break: function calls (`SUM(...)`, `IF(...)`), cell refs (`@1,2`), range syntax, `TRUE`/`FALSE`, numbers, operators.
  - Likely approach: in the `factorP` / atom level, after failing to match a known function or keyword, fall back to reading a bare word as a string literal.
- **Risk:** Medium-high. Context-sensitive disambiguation in a hand-written parser. Easy to introduce regressions where a bare word was previously a parse error but now silently becomes a string.
- **Why fifth:** Most likely to cause subtle parse regressions. Better to do it after the simpler items are stable and the test surface is well-exercised.

## 6. Date type

- **Scope:**
  - New `VDate` constructor in `Value`.
  - Parser: date literal syntax (e.g. `DATE("2025-01-15")` or a dedicated `#2025-01-15` token).
  - Operations: `YEAR`, `MONTH`, `DAY`, `DATEADD`, `DATEDIF`, `TODAY`, `NOW`.
  - `showValue` / `text` / `showValue` clauses for `VDate`.
  - Color treatment (covered by #4's infrastructure).
  - Interaction with existing type system: explicit conversion only, no implicit coercion to/from `VNum` or `VStr`.
- **Risk:** High. Largest surface area: new type, multiple functions, formatting, edge cases (leap years, timezone, locale).
- **Why last:** Benefits from #4 (color) and #5 (string handling) being in place. Largest scope, most likely to need iteration.
