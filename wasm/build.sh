#!/bin/sh
# Builds the wasm/canvas trellis backend end to end: `wasm32-wasi-cabal
# build`, then GHC's post-linker (turns the raw JSFFI-using .wasm into a
# real ESM module trellis-host.mjs/main.mjs can import), landing both
# outputs in wasm/dist/ - the one place both local dev and CI should look.
#
# Requires the ghc-wasm-meta toolchain on PATH (wasm32-wasi-cabal,
# wasm32-wasi-ghc, node) - either `source ~/.ghc-wasm/env` yourself first
# (local dev), or run this after ~/.ghc-wasm/add_to_github_path.sh has
# already populated $GITHUB_PATH/$GITHUB_ENV for the rest of the job (CI -
# see .github/workflows/wasm-build.yml).
set -eu

cd "$(dirname "$0")/.."

if ! command -v wasm32-wasi-cabal >/dev/null 2>&1; then
    echo "wasm/build.sh: wasm32-wasi-cabal not on PATH." >&2
    echo "Source ~/.ghc-wasm/env first (see https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)." >&2
    exit 1
fi

echo "wasm/build.sh: building exe:trellis for wasm32-wasi..."
wasm32-wasi-cabal build exe:trellis

wasm_out=$(wasm32-wasi-cabal list-bin exe:trellis 2>/dev/null)

post_link="$HOME/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs"
if [ ! -f "$post_link" ]; then
    echo "wasm/build.sh: post-link.mjs not found at $post_link" >&2
    echo "(expected alongside wasm32-wasi-ghc from the ghc-wasm-meta bootstrap)" >&2
    exit 1
fi

mkdir -p wasm/dist
cp "$wasm_out" wasm/dist/trellis.wasm

echo "wasm/build.sh: running GHC's JSFFI post-linker..."
node "$post_link" --input wasm/dist/trellis.wasm --output wasm/dist/trellis.js

echo "wasm/build.sh: done -> wasm/dist/trellis.wasm, wasm/dist/trellis.js"
