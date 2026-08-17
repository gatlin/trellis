# trellis

A console-based spreadsheet. Haskell, termbox2, 256 colors, no GUI.

(c) 1988 &mdash; Gatlin Cheyenne Johnson <gatlin@niltag.net>

---

## Quick start

```sh
git clone --recurse-submodules https://github.com/gatlin/trellis.git
cd trellis

# Install GHC + Cabal (skip if you already have them)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.12.2
# close and reopen your terminal

cabal build
cabal run trellis
```

Run it in a real terminal (not an IDE's embedded one) with mouse support
and 256-color output.

> **Already cloned without `--recurse-submodules`?** Run
> `git submodule update --init` inside the repo, then build.

---

## Using Trellis

### Opening and saving

```
trellis mysheet.trellis
```

Opens that file, or starts a blank sheet that will become it.
`Ctrl+S` writes back. No autosave, no save-as &mdash; one file per session.

The file format is one declaration per line:

```
0,0=10
1,0=@0,0+1
2,0=!5s date
OUT 3,0=/tmp/status.fifo
```

Plain enough to read, diff, or hand-edit.

### Command-line bindings

Repeatable flags for pre-wiring cells before the sheet opens:

```
trellis --in 0,0=/tmp/sensor.fifo --out 2,3=/tmp/status.fifo
```

| Flag | Effect |
|------|--------|
| `--in COL,ROW=PATH` | Pre-fills the cell with `!tail PATH` (same as typing it). |
| `--out COL,ROW=PATH` | Spawns a background thread that writes the cell's value to `PATH` on every change. |

Both compose with a sheet file on the same command line. Coordinates
use the same `(column, row)` order as `@x,y` in a formula.

### Keybindings

Read from `~/.config/trellis/keybindings` (a plain-text file,
regenerated with defaults if missing).

---

## Features

### Formulas

A small typed language with explicit precedence:

1. Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`
2. Additive: `+`, `-`, `&` (string concatenation)
3. Multiplicative: `*`, `/`
4. Unary minus
5. Atoms: numbers, quoted strings, booleans, cell refs, function calls

**Range aggregates** over `@x0,y0:x1,y1` (a bare `@x,y` is a 1&times;1 range):
`SUM`, `AVERAGE`, `COUNT`, `COUNTA`, `MIN`, `MAX`, `PRODUCT`, `MEDIAN`,
`VAR`, `STDEV`.

**Scalar functions:**
`IF`, `AND`, `OR`, `NOT`, `STR`, `NUM`, `ABS`, `SQRT`, `LOG`, `LN`,
`EXP`, `SIGN`, `INT`, `TRUNC`, `CEILING`, `FLOOR`, `MOD`, `POWER`,
`ROUND`, `ROUNDUP`, `ROUNDDOWN`, `LEN`, `UPPER`, `LOWER`, `TRIM`,
`LEFT`, `RIGHT`, `FIND`, `REPT`, `MID`, `SUBSTITUTE`, `RAND`.

There is **no implicit coercion** between types. `1 + "hello"` is an
error. Use `STR` and `NUM` to convert explicitly.

### Interaction

- **Mouse:** click to select, drag to extend, double-click to edit,
  scroll to zoom, middle-drag to pan, right-drag to fill (references
  adjust to match the target).
- **Keyboard:** arrow keys to move, Shift+Arrow to extend a selection,
  Enter to edit, Esc to cancel.
- **Fill:** drag-select a whole row or column first, then right-drag
  to replicate across it.

### Charts

Select a row, column, or block, then:

| Key | Chart |
|-----|-------|
| `b` | Bar |
| `l` | Line |
| `h` | Heatmap |

Press the same key again to close; a different key to switch.

### Live cells

Not part of the formula language. A cell whose text starts with `!`
is interpreted as a live spec instead of a formula:

| Syntax | Behavior |
|--------|----------|
| `!Ns cmd` | Re-runs `cmd` every *N* seconds; cell becomes its stdout. |
| `!tail path` | Streams lines from a file or FIFO as they arrive. |

---

## Design notes

### Evaluation

Cells are evaluated by tying a lazy knot over the grid: each cell's
value is a thunk that references its neighbors' thunks. No dependency
graph, no topological sort, no invalidation pass. Circular references
work because Haskell's laziness defers evaluation until a value is
actually demanded.

### Type system

The `Value` type is closed and explicit:

```haskell
data Value
  = VBlank
  | VNum Double
  | VStr String
  | VBool Bool
  | VDate Day
  | VErr String
```

No implicit coercion. `VErr` is the only path a type mismatch takes.
This keeps the error surface small and behavior predictable.

### Coordinates

All coordinates are **(column, row)** &mdash; x first, y second. This
applies everywhere: cell references (`@3,2`), ranges (`@1,0:4,3`),
CLI flags (`--in 0,0=&hellip;`), the sheet file format, and internal
`(Int, Int)` pairs. It matches the mathematical `(x, y)` convention
and the terminal's column-then-row addressing.

### Rendering

termbox2 (C library, Haskell bindings vendored as a submodule).
256-color output. The UI is drawn from scratch each frame &mdash; no
retained-mode widget system. All colors live in
`app/Render/Theme.hs`; cell geometry in `app/SheetState/Geometry.hs`.

### Concurrency

Background work (live cells, out bindings) runs in the `Orc` monad
(`src/Trellis/Orc.hs`): a small process layer providing `spawn`,
`sync`, race (`<?>`), and `scan`. `HIO` underneath tracks thread
groups so cancelling a cell's live spec tears down its thread cleanly.

---

## Architecture &amp; contributing

### Module layout

```
app/
  Main.hs              -- entry point, wires everything together
  Cli.hs               -- command-line argument parsing
  Parser.hs            -- recursive-descent formula parser (Attoparsec)
  Formula.hs           -- expression AST (Expr, Op, etc.)
  Formula/Builtins.hs  -- Value type, showValue, aggregation, function eval
  SheetFile.hs         -- plain-text sheet file parse/serialize
  SheetState/
    Chart.hs           -- chart classification and rendering data
    Fill.hs            -- fill-source classification and preview geometry
    Geometry.hs        -- viewport math: cell widths, visible rows/cols, clamping
  Live/
    In.hs              -- live input cells (!Ns cmd, !tail path)
    Out.hs             -- out bindings (cell → file, background thread)
  Render/
    Theme.hs           -- color palette, selection colors, chart colors
  Update/
    Buffer.hs          -- formula editor buffer (insert, delete, clamp)
    Events.hs          -- termbox2 event helpers (key matching, printable chars)

src/Trellis/
  Orc.hs               -- lightweight process monad (spawn, sync, race, scan)
  HIO.hs               -- IO-based monad with group/thread tracking
  CPS.hs               -- continuation-passing style foundation for Orc
  UI.hs                -- termbox2 screen primitives (drawText, drawGlyph)
  Sheet.hs             -- typed list/stream/tape combinators
  Tubes.hs             -- source/sink streaming
  Peano.hs             -- compile-time natural numbers (bounded concurrency)
```

`app/` is the spreadsheet. `src/Trellis/` is a small supporting
library: concurrency primitives, UI drawing helpers, and typed
collection combinators. The split keeps the concurrency and UI layers
reusable and testable in isolation.

### Parser structure

Recursive-descent on `Data.Attoparsec.Text`. Each precedence level is
a named function that calls the next-lower level. The `choice`
combinator is used at the atom level to try alternatives in order.

### Adding a builtin function

Touch three files:

1. **`Formula.hs`** &mdash; add a constructor to the AST (e.g. `Call0F RandOp`).
2. **`Parser.hs`** &mdash; add a parser rule (e.g. `randP = mk0 "RAND" &hellip;`)
   and wire it into the atom-level `choice`.
3. **`Formula/Builtins.hs`** &mdash; add the evaluation clause.

Then add it to the feature list above.
