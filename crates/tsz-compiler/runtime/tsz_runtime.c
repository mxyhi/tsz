#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

// TSZ minimal runtime: provides stdout support for `console.log(...)`.
//
// Design goals:
// - Non-variadic API, easy for AOT codegen to call directly
// - Prefer libc (stdio) to keep the implementation simple

void tsz_log_i64(int64_t v) {
  fprintf(stdout, "%" PRId64, v);
}

void tsz_log_f64(double v) {
  // Use %.17g to preserve f64 precision as much as possible (while keeping it readable).
  fprintf(stdout, "%.17g", v);
}

void tsz_log_str(const uint8_t* ptr, int64_t len) {
  if (ptr == NULL || len <= 0) {
    return;
  }
  fwrite(ptr, 1, (size_t)len, stdout);
}

void tsz_log_space(void) {
  fputc(' ', stdout);
}

void tsz_log_newline(void) {
  fputc('\n', stdout);
}
