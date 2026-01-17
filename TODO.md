# TODO

## 支持语法（路线图）

> 约定：每完成一项语法能力，同时更新 `docs/tsz-spec.md` 与 `README.md` 的路线图勾选状态，并补齐对应的解析/类型检查/端到端测试。

- [x] let
  - [x] 语法：`let <name>: <type>? = <expr>;`（先支持函数体内；块级作用域）
  - [x] 编译链路：lexer → parser(AST) → typecheck(HIR/作用域) → codegen(局部变量) → build/run 测试用例

- [x] const
  - [x] 语法：`const <name>: <type>? = <expr>;`（只读）
  - [x] 语义：编译期常量折叠/内联（字面量与一元负号；允许引用其他 `const`）
  - [x] 测试：编译期报错（非 const expr）+ build/run（读 const）

- [x] function
  - [x] v0：0 参数 + 单 `return`（当前已实现）
  - [x] v1：参数列表（`function f(a: T, b: U): R { ... }`）
  - [x] v1：多语句 block（通常需要 `let/const` 先落地）
  - [x] v1：表达式扩展（标识符、二元运算、括号、调用带参数）
