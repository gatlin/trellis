# Vendored: torc

`torc.js` (+ `.js.map`/`.d.ts`) is the built ESM output of
[torc](https://github.com/gatlin/torc) v4.1.0, the user's own JS
reactive-streams library - used as the live-cell (`Trellis.Orc`) substrate
on the wasm target (see `src-wasi/Trellis/Orc.hs`, `app-wasi/Live/In.hs`).

Not published to a package registry, so this is a vendored copy rather
than an `npm install`-able dependency. To rebuild after a torc source
change:

```sh
cd ~/code/torc && npm ci && npm run build
cp dist/esm/index.js{,.map} dist/esm/index.d.ts /path/to/trellis/wasm/vendor/
mv /path/to/trellis/wasm/vendor/index.js /path/to/trellis/wasm/vendor/torc.js
mv /path/to/trellis/wasm/vendor/index.js.map /path/to/trellis/wasm/vendor/torc.js.map
mv /path/to/trellis/wasm/vendor/index.d.ts /path/to/trellis/wasm/vendor/torc.d.ts
```

A git-submodule-style dependency (matching how `termbox2-hs` is linked
into this repo) would be the more principled long-term answer if torc
gets more use here - this vendored copy is the pragmatic minimum to get
real live-cell data working now.
