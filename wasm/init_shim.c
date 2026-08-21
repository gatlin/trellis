#include <Rts.h>

/* Per RtsAPI.h's own documented reactor-module pattern (search
 * "wasm32-wasi reactor module" in that header): the user calls
 * hs_init_ghc() once, after which exported Haskell functions can be
 * invoked repeatedly. Mirrors GHC's own generated command-module
 * wrapper (defaultRtsConfig, rts_opts_enabled = RtsOptsSafeOnly) but
 * deliberately leaves rts_hs_main false and never calls hs_exit - a
 * reactor module's RTS must stay alive across calls, not tear down
 * after one.
 *
 * A preopened directory must be provided by the WASI host (see
 * browser_main.mjs/node_check.mjs) even though this module never
 * touches the filesystem - without one, GHC's own auto-generated
 * __ghc_wasm_jsffi_init constructor (which runs as part of the same
 * _initialize/__wasm_call_ctors sequence as this one) calls _Exit(71).
 * Confirmed empirically by intercepting wasi_snapshot_preview1's
 * proc_exit and reading the resulting stack trace - undocumented in
 * the GHC user's guide.
 */
__attribute__((constructor)) static void trellis_hs_init(void) {
    int argc = 1;
    char *argv0 = "trellis.wasm";
    char *argv[] = {argv0, NULL};
    char **argvp = argv;
    RtsConfig conf = defaultRtsConfig;
    conf.rts_opts_enabled = RtsOptsSafeOnly;
    conf.rts_opts_suggestions = true;
    conf.keep_cafs = false;
    hs_init_ghc(&argc, &argvp, conf);
}
