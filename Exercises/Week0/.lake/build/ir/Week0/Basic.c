// Lean compiler output
// Module: Week0.Basic
// Imports: public import Init
#include <lean/lean.h>
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wunused-label"
#elif defined(__GNUC__) && !defined(__CLANG__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#ifdef __cplusplus
extern "C" {
#endif
LEAN_EXPORT lean_object* lp_Week0_hello;
static lean_object* lp_Week0_hello___closed__0;
static lean_object* _init_lp_Week0_hello___closed__0() {
_start:
{
lean_object* x_1; 
x_1 = lean_mk_string_unchecked("world", 5, 5);
return x_1;
}
}
static lean_object* _init_lp_Week0_hello() {
_start:
{
lean_object* x_1; 
x_1 = lp_Week0_hello___closed__0;
return x_1;
}
}
lean_object* initialize_Init(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_Week0_Week0_Basic(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
lp_Week0_hello___closed__0 = _init_lp_Week0_hello___closed__0();
lean_mark_persistent(lp_Week0_hello___closed__0);
lp_Week0_hello = _init_lp_Week0_hello();
lean_mark_persistent(lp_Week0_hello);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
