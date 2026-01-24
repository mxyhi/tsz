# TSZ

[English](#english) | [中文](#zh)

## English

TSZ is a compiled (AOT) language and compiler that is primarily TypeScript syntax, allows dropping bad features, and targets high performance.

### Quick Start

```bash
# Run an example (process exit code: 42)
cargo run -p tsz-cli -- run examples/exit42.ts

# Run a multi-file package example (directory entry, import + call)
cargo run -p tsz-cli -- run examples/pkg-import

# Stdout example (console.log)
cargo run -p tsz-cli -- run examples/console-log.ts

# Assignment example (`=` and `+=` etc.)
cargo run -p tsz-cli -- run examples/assign.ts

# Control flow example (block / if / else / while / break / continue)
cargo run -p tsz-cli -- run examples/control-flow.ts

# `const` example (compile-time folding / inlining)
cargo run -p tsz-cli -- run examples/const.ts

# Build artifacts (exe/obj/ir)
cargo run -p tsz-cli -- build examples/exit42.ts --emit exe
cargo run -p tsz-cli -- build examples/exit42.ts --emit obj -o out.o
cargo run -p tsz-cli -- build examples/exit42.ts --emit ir -o out.clif

# OptLevel: size
cargo run -p tsz-cli -- build examples/exit42.ts --opt size

# Benchmark (compile/run)
cargo run -p tsz-cli -- bench examples/control-flow.ts --compile-iters 10 --run-iters 30
```

### Spec (implemented minimal subset)

See [docs/tsz-spec.md](docs/tsz-spec.md).

### Roadmap

See [TODO.md](TODO.md).

<a id="zh"></a>
## 中文

一个“TypeScript 语法为主、可删减坏特性、追求高性能”的编译型语言与编译器（AOT）。

### 快速开始

```bash
# 运行示例（返回进程退出码 42）
cargo run -p tsz-cli -- run examples/exit42.ts

# 运行多文件包示例（目录入口，import + 调用）
cargo run -p tsz-cli -- run examples/pkg-import

# 标准输出示例（console.log）
cargo run -p tsz-cli -- run examples/console-log.ts

# 赋值示例（= 与 += 等语法糖）
cargo run -p tsz-cli -- run examples/assign.ts

# 控制流示例（block / if / else / while / break / continue）
cargo run -p tsz-cli -- run examples/control-flow.ts

# const 示例（编译期常量折叠/内联）
cargo run -p tsz-cli -- run examples/const.ts

# 构建产物（exe/obj/ir）
cargo run -p tsz-cli -- build examples/exit42.ts --emit exe
cargo run -p tsz-cli -- build examples/exit42.ts --emit obj -o out.o
cargo run -p tsz-cli -- build examples/exit42.ts --emit ir -o out.clif

# OptLevel: size
cargo run -p tsz-cli -- build examples/exit42.ts --opt size

# 基准测试（compile/run）
cargo run -p tsz-cli -- bench examples/control-flow.ts --compile-iters 10 --run-iters 30
```

### 规范（当前实现的最小子集）

见 `docs/tsz-spec.md`。

### 路线图

见 [TODO.md](TODO.md)。
