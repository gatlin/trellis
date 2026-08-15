# trellis

(c) 1988 &mdash; Gatlin Cheyenne Johnson <gatlin@niltag.net>

# What?

A console-based spreadsheet application written in Haskell.

# How to build.

New to Haskell? Do these three steps in order, copying each block as
you go.

**1. Clone the repo — with this exact flag.** Trellis depends on
another repo (`termbox2-hs`) that plain `git clone` silently skips.
`--recurse-submodules` tells it to fetch that too:

```
git clone --recurse-submodules https://github.com/gatlin/trellis.git
cd trellis
```

(Already cloned without the flag and things seem broken/missing? Run
`git submodule update --init` from inside the repo and move on to
step 2.)

**2. Install Haskell.** [GHCup](https://www.haskell.org/ghcup/) is
the standard installer for the Haskell compiler (GHC) and build tool
(Cabal), both of which you need:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.12.2
```

Accept the defaults it offers. Then close and reopen your terminal —
this puts the tools it just installed on your `PATH`.

**3. Build and run**, from inside the repo:

```
cabal build
cabal run trellis
```

Run it in a real terminal (not an IDE's built-in one) with mouse
support and 256 colors.

# Current features.

* Comonadic sheet evaluation. `evaluate` ties a lazy self-referential
  knot over the grid rather than building a dependency graph.
* A small typed formula language: numbers, strings, booleans, explicit
  conversion only (`STR`, `NUM`), no implicit coercion between them.
* Range formulas over `@x0,y0:x1,y1` — `SUM`, `AVERAGE`, `COUNT`, `MIN`,
  `MAX`, `PRODUCT`, `MEDIAN`, `VAR`, `STDEV`, `COUNTA`. A bare `@x,y` is
  its own 1x1 range.
* Everything else: `IF`, `AND`, `OR`, `NOT`, `STR`, `NUM`, `ABS`,
  `SQRT`, `LOG`, `LN`, `EXP`, `SIGN`, `INT`, `TRUNC`, `CEILING`,
  `FLOOR`, `MOD`, `POWER`, `ROUND`, `ROUNDUP`, `ROUNDDOWN`, `LEN`,
  `UPPER`, `LOWER`, `TRIM`, `LEFT`, `RIGHT`, `FIND`, `REPT`, `MID`,
  `SUBSTITUTE`, `RAND`.
* Mouse support: click to select or drag, double-click to edit, scroll
  to zoom, middle-click-drag to pan, right-click-drag to fill a range
  with the source cell's formula (references adjusted to match).
  Drag-select a whole row or column first and a right-click-drag
  replicates across it instead.
* Shift+Arrow extends a selection from the keyboard, the same rectangle
  a mouse drag makes.
* Charts: select a row, column, or block, then `b`/`l`/`h` toggles a
  bar, line, or heatmap chart over it. Press the same key again to
  close it, a different one to switch.
* Keyboard navigation and a modal formula editor with interior cursor
  movement.
* Configurable keybindings, read from a plain text config file at
  `~/.config/trellis/keybindings` (regenerated with defaults if missing).
* Live cells: type `!5s some-command` into a cell and it re-runs that
  command every 5 seconds, becoming its output. `!tail /path/to/fifo`
  streams lines from a file or pipe as they arrive. Either replaces the
  cell's formula, not part of the formula language itself.
* Sheets save and load to a plain-text file.

# Architecture.

## Module layout

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

`app/` is the spreadsheet application. `src/Trellis/` is a small
supporting library: concurrency primitives, UI drawing helpers, and
typed collection combinators. The split exists so the concurrency and
UI layers are reusable and testable in isolation from the spreadsheet
logic.

## Evaluation model

Cells are evaluated by tying a lazy knot over the grid: each cell's
value is a thunk that references its neighbors' thunks. No dependency
graph, no topological sort, no invalidation pass. Circular references
work naturally because Haskell's laziness defers evaluation until a
value is actually demanded.

Background work (live cells, out bindings) runs in the `Orc` monad
(`src/Trellis/Orc.hs`), a small purpose-built process layer that
provides `spawn`, `sync`, race (`<?>`), and `scan` without pulling in
a full actor framework. `HIO` underneath tracks thread groups so that
cancelling a cell's live spec tears down its thread cleanly.

## Type system

The `Value` type in `Formula/Builtins.hs` is closed and explicit:

```haskell
data Value
  = VBlank
  | VNum Double
  | VStr String
  | VBool Bool
  | VErr String
```

There is **no implicit coercion** between types. `1 + "hello"` is an
error, not a silent string concatenation. Use `STR` and `NUM` to
convert explicitly. This is a deliberate design choice: predictable
behavior, small error surface, and it makes the `VErr` path the only
way a type mismatch surfaces.

## Parser

The formula parser in `app/Parser.hs` is a recursive-descent parser
built on `Data.Attoparsec.Text`. The grammar is structured in explicit
precedence levels, each a named function that calls the next:

1. Comparison (`=`, `<>`, `<`, `>`, `<=`, `>=`)
2. Additive (`+`, `-`, `&` for string concatenation)
3. Multiplicative (`*`, `/`)
4. Unary minus
5. Atoms (numbers, quoted strings, booleans, cell refs, function calls, `IF`)

The `choice` combinator is used at the atom level to try each
alternative in order. The `Parser` type is Attoparsec's, and the
`<|>` operator comes from `Control.Applicative`.

To add a new builtin function, touch three files:

1. `Formula.hs` — add a constructor to the AST (e.g. `Call0F RandOp`).
2. `Parser.hs` — add a parser rule (e.g. `randP = mk0 "RAND" ...`) and
   wire it into the atom-level `choice`.
3. `Formula/Builtins.hs` — add the evaluation clause.

Then add it to the feature list in this README.

## Coordinates

All coordinates are **(column, row)** — x first, y second. This
applies everywhere:

- Cell references in formulas: `@3,2` means column 3, row 2.
- Ranges: `@1,0:4,3`.
- CLI flags: `--in 0,0=/path`.
- Sheet file format: `0,0=10`.
- Internal data structures: `(Int, Int)` pairs throughout.

This is the opposite of the `(row, column)` convention used by most
spreadsheet software. It matches the mathematical `(x, y)` convention
and the terminal's column-then-row addressing.

## Rendering

Uses `termbox2` (a C library with Haskell bindings, vendored as a git
submodule). 256-color output. All colors are defined in
`app/Render/Theme.hs`. The UI is drawn from scratch each frame — there
is no retained-mode widget system. Cell geometry (widths, visible
rows/cols, clamping) lives in `app/SheetState/Geometry.hs`.

## Live cells and out bindings

These are **not** part of the formula language. A cell's text is first
checked for a `!` prefix; if it matches a live spec (`!Ns cmd` or
`!tail path`), the cell runs as a background process instead of being
parsed as a formula. Out bindings (`OUT x,y=path` in the sheet file, or
`--out` on the CLI) spawn a background thread that watches a cell's
value and writes changes to a file. Both use the `Orc` monad for
lifecycle management.

# Saving and loading sheets.

```
trellis mysheet.trellis
```

opens that file, or — if it doesn't exist yet — starts a blank sheet
that will become it. `Ctrl+S` writes the sheet back; there's no
autosave and no save-as, just the one file a session was pointed at.
The format is one declaration per line, formulas and live specs as
typed:

```
0,0=10
1,0=@0,0+1
2,0=!5s date
OUT 3,0=/tmp/status.fifo
```

plain enough to read, diff, or hand-edit.

# Feeding cells from the command line.

A cell can be bound to a live source before the sheet even opens, or
made to publish its own value out, via repeatable `--in`/`--out` flags:

```
trellis --in 0,0=/tmp/sensor.fifo --out 2,3=/tmp/status.fifo
```

`--in COL,ROW=PATH` pre-fills a cell with `!tail PATH`, same as typing
it in by hand. `--out COL,ROW=PATH` watches a cell and writes its value
to `PATH` every time it changes, in its own background thread, so a
pipe nobody's reading yet can't hang the sheet. Coordinates use the
same `(column,row)` order as `@x,y` in a formula, and either flag can
be repeated for as many cells as you like. Both compose with a sheet
file given on the same command line.
