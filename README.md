# TSZ

一个“TypeScript 语法为主、可删减坏特性、追求高性能”的编译型语言与编译器（AOT）。

## 快速开始

```bash
# 运行示例（返回进程退出码 42）
cargo run -p tsz-cli -- run examples/exit42.ts

# 运行多文件包示例（目录入口，import + 调用）
cargo run -p tsz-cli -- run examples/pkg-import

# 标准输出示例（console.log）
cargo run -p tsz-cli -- run examples/console-log.ts

# const 示例（编译期常量折叠/内联）
cargo run -p tsz-cli -- run examples/const.ts
```

## 规范（当前实现的最小子集）

见 `docs/tsz-spec.md`。

## 路线图（语法）

- [x] let（函数体内；块级作用域）
- [x] const
- [ ] function v1（参数列表 / 多语句 block / 表达式扩展）

更完整路线图见 [TODO.md](TODO.md)。
