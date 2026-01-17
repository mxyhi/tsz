# TODO

## 支持语法（路线图）

> 约定：每完成一项语法能力，同时更新 `docs/tsz-spec.md`，并补齐对应的解析/类型检查/端到端测试。

- [x] 模块系统（最小子集）
  - [x] 语法：`export function ...`
  - [x] 语法：`import { a, b } from "<specifier>";`
  - [x] 解析：相对路径（支持省略扩展名，补全 `.ts` / `.tsz`）
  - [x] 解析：目录入口（读取 `package.json` 的 `tsz.entry`）
  - [x] 解析：`node_modules` 包导入（仅包根；暂不支持 subpath import）
  - [ ] 解析：package subpath import（例如 `import { x } from "dep/subpath"`）

- [x] console.log
  - [x] 语法：`console.log();` / `console.log(<expr>, <expr>, ...);`
  - [x] 输出：参数用空格分隔，末尾自动追加换行
  - [x] 类型：支持 `number/bigint` + `string` 字面量（当前 string 仅用于输出）

- [x] 基础类型（v0）
  - [x] `number` / `bigint` / `void`（函数参数/返回值、入口 ABI）
  - [ ] `boolean`（语法 + 类型检查 + codegen）
  - [ ] `string`（局部变量/返回值/运行时；当前仅支持 string 字面量用于 console.log）

- [x] let
  - [x] 语法：`let <name>: <type>? = <expr>;`（先支持函数体内；块级作用域）
  - [x] 编译链路：lexer → parser(AST) → typecheck(HIR/作用域) → codegen(局部变量) → build/run 测试用例

- [x] const
  - [x] 语法：`const <name>: <type>? = <expr>;`（只读）
  - [x] 语义：编译期常量折叠/内联（字面量与一元负号；允许引用其他 `const`）
  - [x] 测试：编译期报错（非 const expr）+ build/run（读 const）
  - [ ] 常量折叠扩展：支持 `+ - * /` 的编译期折叠（仅字面量/const 引用）

- [x] function
  - [x] v0：0 参数 + 单 `return`（当前已实现）
  - [x] v1：参数列表（`function f(a: T, b: U): R { ... }`）
  - [x] v1：多语句 block（通常需要 `let/const` 先落地）
  - [x] v1：表达式扩展（标识符、二元运算、括号、调用带参数）

- [ ] 赋值与可变性（v2+）
  - [ ] 语法：`<name> = <expr>;`（仅 `let` 可写；`const` 禁止）
  - [ ] 可选：`+=` / `-=` / `*=` / `/=`（语法糖）

- [ ] 控制流（v2+）
  - [ ] 块语句：`{ ... }`（真正块级作用域，允许嵌套）
  - [ ] `if / else`
  - [ ] `while`（以及 `break/continue`）

- [ ] 运算与比较（v2+）
  - [ ] 比较：`== != < <= > >=`（需要 boolean）
  - [ ] 逻辑：`&& || !`（需要 boolean）

## 编译器与工具链

- [x] CLI：`tsz build` / `tsz run`（支持 `--opt none|speed`）
- [ ] CLI：`tsz check`（只做解析 + 类型检查，不生成/链接）

- [ ] 诊断（Diagnostics）
  - [ ] Span → 行/列 + 源码片段高亮（类似 rustc）
  - [ ] 多错误收集（尽量一次报告多个错误点）

- [ ] 构建产物
  - [ ] 产物管理：临时 `.o/.obj` 清理策略（避免污染输出目录）
  - [ ] 可选：`--emit obj` / `--emit ir`（调试/排障）

## 性能

- [ ] 基准测试（compile time + runtime）与性能预算
- [ ] 并行模块加载/解析（在保证确定性的前提下）
- [ ] 更细粒度 OptLevel（例如 size；若确有收益）

## 运行时与生态（方向已定）

- [ ] 内存模型：ARC/RC + `Weak`
- [ ] 对象布局 + 基础容器（string/array 等）
- [ ] FFI：C ABI（声明/调用/链接）
