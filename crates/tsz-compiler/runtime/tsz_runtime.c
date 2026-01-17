#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

// TSZ 最小运行时：为 `console.log(...)` 提供标准输出能力。
//
// 设计目标：
// - API 非 variadic，便于 AOT 代码生成直接调用
// - 尽量依赖 libc（stdio）以保持实现简洁

void tsz_log_i64(int64_t v) {
  fprintf(stdout, "%" PRId64, v);
}

void tsz_log_f64(double v) {
  // 使用 %.17g 以尽量保留 f64 精度（兼顾可读性）
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

