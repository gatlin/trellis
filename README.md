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
* Six builtins: `IF`, `AND`, `OR`, `NOT`, `STR`, `NUM`.
* Mouse support: click to select or drag, double-click to edit, scroll
  to zoom, middle-click-drag to pan.
* Keyboard navigation and a modal formula editor with interior cursor
  movement.
* Configurable keybindings, read from a plain text config file at
  `~/.config/trellis/keybindings` (regenerated with defaults if missing).
* Live cells: type `!5s some-command` into a cell and it re-runs that
  command every 5 seconds, becoming its output. `!tail /path/to/fifo`
  streams lines from a file or pipe as they arrive. Either replaces the
  cell's formula, not part of the formula language itself.

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
be repeated for as many cells as you like.


