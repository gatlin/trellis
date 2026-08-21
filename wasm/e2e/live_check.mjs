// Playwright verification of real ShellInterval live-cell data (fetch()
// polling via torc's `shift`, Live.In.declareSubscription's wasm path) -
// distinct from wasm/e2e/check.mjs, which only exercises ordinary
// formula editing. Proves, against a real local HTTP endpoint this test
// controls (not the public internet - unreliable and not the point):
//   1. genuine repeated re-fetching (the endpoint's request count climbs
//      by more than one over a real, rAF-ticked wait, not just once), and
//   2. the rendered cell actually reflects a new value each time (canvas
//      pixels at that specific cell change between two sampled points),
//   3. cancelling the subscription (Delete, the real clearCell binding ->
//      cancelSubscription -> Orc.close -> torc Activity.finish()) really
//      stops polling - the endpoint's request count plateaus afterward
//      instead of continuing to climb forever in the background.
//
// Serve wasm/ first, then run this from inside wasm/:
//   python3 -m http.server 8935 &
//   node e2e/live_check.mjs
import { chromium } from "playwright";
import http from "node:http";

// A tiny local endpoint returning an incrementing counter with CORS
// headers (the page is served on 8935, this on 8936 - different origin).
//
// Note on timing: Chromium throttles/coalesces setInterval firings for
// backgrounded or headless pages (a well-documented power-saving policy,
// unrelated to this feature) - confirmed directly by isolating the same
// subscription in a script that keeps the page "active" via many short
// waits instead of one long one, which showed a dead-accurate 1
// request/second. Under Playwright here, requests can arrive in bursts
// with irregular gaps between them rather than evenly spaced - genuine
// browser behavior, not a bug in Live.In's polling logic. The assertions
// below only check that *more than one* fetch happened over the wait
// window, not that they arrived on a precise schedule, so this doesn't
// affect correctness of the check.
function startCounterServer() {
  let n = 0;
  let requestCount = 0;
  const server = http.createServer((req, res) => {
    requestCount++;
    n++;
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.setHeader("Content-Type", "text/plain");
    res.end(String(n));
  });
  return new Promise((resolve) => {
    server.listen(8936, "127.0.0.1", () =>
      resolve({ server, getRequestCount: () => requestCount })
    );
  });
}

// Whole-canvas diff rather than computing one cell's exact pixel rect
// (gutter/header offsets, ruled-line stride, zoom - replicating
// Render.Grid's real layout math here would be fragile). With the cursor
// stationary and nothing else being edited, the *only* thing that should
// legitimately differ between two samples is the live cell's changing
// digit, so a full-canvas diff is still a clean, specific signal.
async function samplePixels(page) {
  return page.evaluate(() => {
    const c = document.getElementById("trellis-canvas");
    const ctx = c.getContext("2d");
    return Array.from(ctx.getImageData(0, 0, c.width, c.height).data);
  });
}

async function run() {
  const { server, getRequestCount } = await startCounterServer();
  const browser = await chromium.launch();
  const page = await browser.newPage();
  const pageErrors = [];
  page.on("pageerror", (err) => pageErrors.push(String(err)));

  await page.goto("http://127.0.0.1:8935/index.html");
  await page.waitForFunction(() => document.getElementById("status").textContent === "running", { timeout: 10000 });
  await page.waitForTimeout(200);

  // Navigate to a fresh cell (2,2) and subscribe it to the counter
  // endpoint, polling every 1s - real interval, real fetch, real torc
  // Observable, exactly as a user would type it.
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("Enter");
  await page.waitForTimeout(100);
  await page.keyboard.type("!1s http://127.0.0.1:8936/counter");
  await page.waitForTimeout(100);
  await page.keyboard.press("Enter");
  await page.waitForTimeout(1200); // let the immediate first fetch land + redraw

  const reqAfterFirst = getRequestCount();
  const pixelsFirst = await samplePixels(page);

  // Let several 1s intervals elapse via real requestAnimationFrame ticks,
  // not a shortened/fake clock.
  await page.waitForTimeout(3500);

  const reqAfterWait = getRequestCount();
  const pixelsSecond = await samplePixels(page);

  const genuinelyRepolled = reqAfterWait >= reqAfterFirst + 2;
  const pixelsChanged = pixelsFirst.some((v, i) => v !== pixelsSecond[i]);

  // Cancel: Delete on the focused (live) cell -> cancelSubscription ->
  // Orc.close -> the JS cleanup closure -> clearInterval + onDone.
  await page.keyboard.press("Delete");
  await page.waitForTimeout(200);
  const reqAtCancel = getRequestCount();
  await page.waitForTimeout(3000); // several more would-be intervals
  const reqAfterCancel = getRequestCount();
  const pollingStopped = reqAfterCancel <= reqAtCancel + 1; // allow one in-flight

  await browser.close();
  server.close();

  const ok =
    pageErrors.length === 0 &&
    genuinelyRepolled &&
    pixelsChanged &&
    pollingStopped;

  console.log(
    `reqAfterFirst=${reqAfterFirst} reqAfterWait=${reqAfterWait} genuinelyRepolled=${genuinelyRepolled} ` +
    `pixelsChanged=${pixelsChanged} reqAtCancel=${reqAtCancel} reqAfterCancel=${reqAfterCancel} ` +
    `pollingStopped=${pollingStopped} pageErrors=${JSON.stringify(pageErrors)}`
  );
  return ok;
}

let allOk = true;
for (let i = 1; i <= 3; i++) {
  const ok = await run();
  console.log(`[run ${i}] ${ok ? "PASS" : "FAIL"}`);
  allOk = allOk && ok;
}

if (!allOk) {
  console.error("OVERALL: FAIL");
  process.exit(1);
}
console.log("OVERALL: PASS (3/3 runs green)");
