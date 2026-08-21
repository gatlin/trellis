// Playwright verification of <trellis-sheet> (wasm/trellis-sheet.js),
// the Web Component wrapper - separate from wasm/e2e/check.mjs (the
// bare-page demo those tests already cover) since this exercises a
// genuinely different public surface: bindIn/watchOut/cell-out, and
// multi-instance isolation. Serve wasm/ first, e.g.:
//   python3 -m http.server 8935 &   (from inside wasm/)
//   node e2e/component_check.mjs
import { chromium } from "playwright";

const PAGE = `<!doctype html>
<html><body>
<trellis-sheet id="a" width="800" height="400"></trellis-sheet>
<trellis-sheet id="b" width="800" height="400"></trellis-sheet>
<script type="module">
  import "./trellis-sheet.js";
  window.__ready = Promise.all([
    new Promise((r) => document.getElementById("a").addEventListener("ready", r)),
    new Promise((r) => document.getElementById("b").addEventListener("ready", r)),
  ]);
</script>
</body></html>`;

// A torc-shift-shaped source: (next) => cleanupFn, emitting an
// incrementing count every intervalMs, starting immediately - the
// simplest possible thing that isn't torc.pure's single fixed value,
// deliberately NOT importing torc at all here (the whole point of
// duck-typed bindIn is that callers never need to).
function countSourceScript(intervalMs, step) {
  return `(next) => {
    let n = 0;
    next(n);
    const id = setInterval(() => { n += ${step}; next(n); }, ${intervalMs});
    return () => clearInterval(id);
  }`;
}

async function run(runIndex) {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.route("**/component-test", (route) =>
    route.fulfill({ contentType: "text/html", body: PAGE })
  );
  await page.goto("http://127.0.0.1:8935/component-test");
  await page.evaluate(() => window.__ready);
  await page.waitForTimeout(200);

  // --- bindIn + watchOut together on one sheet: bind a counting
  // source to a cell, watch that same cell, and assert the cell-out
  // events actually carry the source's own emitted sequence (not just
  // "some event fired") - proves values genuinely flow from an
  // external JS source through bindIn into the sheet and back out
  // through watchOut, not just that neither call threw.
  const bindInResult = await page.evaluate(async (srcCode) => {
    const sheet = document.getElementById("a");
    const seen = [];
    sheet.addEventListener("cell-out", (e) => {
      if (e.detail.row === 0 && e.detail.col === 0) seen.push(e.detail.value);
    });
    await sheet.watchOut(0, 0);
    const subscribeFn = new Function(`return ${srcCode}`)();
    await sheet.bindIn(0, 0, subscribeFn);
    await new Promise((r) => setTimeout(r, 900)); // several 200ms emissions
    return seen;
  }, countSourceScript(200, 1));

  // --- watchOut driven by ordinary formula editing, not bindIn - the
  // other real path a cell's value changes through, matching
  // wasm/e2e/check.mjs's own real-keyboard-input style.
  await page.evaluate(async () => {
    const sheet = document.getElementById("b");
    window.__bOut = [];
    sheet.addEventListener("cell-out", (e) => {
      if (e.detail.row === 3 && e.detail.col === 2) window.__bOut.push(e.detail.value);
    });
    await sheet.watchOut(3, 2);
  });
  const bCanvas = await page.evaluateHandle(() =>
    document.getElementById("b").shadowRoot.querySelector("canvas")
  );
  await bCanvas.asElement().click(); // focuses it - <trellis-sheet> deliberately doesn't auto-focus
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("Enter");
  await page.waitForTimeout(100);
  await page.keyboard.type("40+2");
  await page.waitForTimeout(100);
  await page.keyboard.press("Enter");
  // publishOutUpdates only runs on a synthetic Tick, never the same
  // tick that processes the Input event committing the edit itself
  // (Update.Core.update pattern-matches Tick vs Input separately, one
  // event per tick) - so this needs to wait out at least one
  // subsequent ~250ms heartbeat, not just "a moment".
  await page.waitForTimeout(800);
  const editOut = await page.evaluate(() => window.__bOut);

  // --- multi-instance isolation: two independently-bound counting
  // sources (different step sizes) on the two sheets' own (1,1) cells,
  // watched independently - confirms neither sheet's Haskell state or
  // JS-side globals (window.trellisHost etc.) leak into the other.
  const isolation = await page.evaluate(async ({ srcA, srcB }) => {
    const a = document.getElementById("a");
    const b = document.getElementById("b");
    const seenA = [];
    const seenB = [];
    a.addEventListener("cell-out", (e) => {
      if (e.detail.row === 1 && e.detail.col === 1) seenA.push(e.detail.value);
    });
    b.addEventListener("cell-out", (e) => {
      if (e.detail.row === 1 && e.detail.col === 1) seenB.push(e.detail.value);
    });
    await a.watchOut(1, 1);
    await b.watchOut(1, 1);
    await a.bindIn(1, 1, new Function(`return ${srcA}`)());
    await b.bindIn(1, 1, new Function(`return ${srcB}`)());
    // Generous window/sample-count margin deliberately: this is a
    // wall-clock-timing-based check (both sheets' own 150ms source
    // intervals, each riding their own independent 250ms heartbeat),
    // and it genuinely got flaky at a tighter 700ms/>=3-samples budget
    // under real CPU contention (another Playwright/Chromium instance
    // running concurrently) - loosened here rather than assumed fine
    // in isolation only.
    await new Promise((r) => setTimeout(r, 1500));
    return { seenA, seenB };
  }, { srcA: countSourceScript(150, 1), srcB: countSourceScript(150, 100) });

  await browser.close();

  const bindInOk =
    bindInResult.length >= 3 &&
    bindInResult[0] === "0" &&
    bindInResult.every((v, i) => i === 0 || Number(v) === Number(bindInResult[i - 1]) + 1);

  const editOutOk = editOut.length >= 1 && editOut[editOut.length - 1] === "42";

  const isolationOk =
    isolation.seenA.length >= 2 &&
    isolation.seenB.length >= 2 &&
    isolation.seenA.every((v) => Number(v) % 100 !== 0 || Number(v) === 0) &&
    isolation.seenB.every((v) => Number(v) % 100 === 0) &&
    // the two sequences must actually differ - real proof of no cross-talk,
    // not just "both happened to look plausible independently"
    JSON.stringify(isolation.seenA) !== JSON.stringify(isolation.seenB);

  const ok = pageErrors.length === 0 && bindInOk && editOutOk && isolationOk;

  console.log(
    `[run ${runIndex}] bindIn=${JSON.stringify(bindInResult)} editOut=${JSON.stringify(editOut)} ` +
    `isoA=${JSON.stringify(isolation.seenA)} isoB=${JSON.stringify(isolation.seenB)} ` +
    `bindInOk=${bindInOk} editOutOk=${editOutOk} isolationOk=${isolationOk} pageErrors=${JSON.stringify(pageErrors)}`
  );
  return ok;
}

let allOk = true;
for (let i = 1; i <= 3; i++) {
  const ok = await run(i);
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allOk = allOk && ok;
}

if (!allOk) {
  console.error("OVERALL: FAIL");
  process.exit(1);
}
console.log("OVERALL: PASS (3/3 runs green)");
