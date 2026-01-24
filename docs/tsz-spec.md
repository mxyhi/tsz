[English](#en) | [中文](#zh)

<a id="en"></a>
# TSZ Spec (v0)

TSZ is a language subset based primarily on TypeScript syntax, aiming for predictable static semantics and high-performance AOT.

> Goal: allow “removing bad features” and do not pursue TypeScript/JavaScript compatibility.

## 1. Files and Package Entry

### 1.1 Source File Extensions

- Source file extension: `.ts` (recommended)
- Compatible extension: `.tsz` (optional)

### 1.2 Entry Arguments for `tsz build/run`

`tsz build <entry>` / `tsz run <entry>` supports two forms of `<entry>`:

1. **File path**: directly specify a `.ts` / `.tsz` file (extension may be omitted and will be auto-completed).
2. **Directory path (npm package root)**: the directory must contain `package.json`, and the entry file is specified via `tsz.entry`.

### 1.3 TSZ Fields in `package.json` (Minimum Constraints)

TSZ only cares about one field:

```json
{
  "name": "your-package",
  "version": "0.0.0",
  "tsz": { "entry": "src/main.ts" }
}
```

Rules:

- `tsz.entry` must be a path relative to the package root
- It must point to a `.ts` / `.tsz` source file
- TSZ **does not** read Node/TS fields like `main` / `module` / `exports` / `types`

## 2. Module System (Minimal Subset)

### 2.1 Syntax

Only named imports are supported:

```ts
import { foo, bar } from "./lib.ts";
```

Constraints:

- Only `import { a, b } from "<specifier>";` is supported
- Default import, `import * as ns`, `as` renaming, and dynamic import are not supported
- `<specifier>` can only be:
  - **Relative paths**: starting with `./` or `../`
  - **Package name/subpath** (node_modules), for example:
    - Package root: `"left-pad"` / `"@scope/pkg"`
    - Package subpath: `"left-pad/subpath"` / `"@scope/pkg/subpath"`

### 2.2 Resolution Rules (Simplified Node Style)

#### Relative Paths

Let the current module file be `current_file`, and its directory be `current_dir`:

- If `<specifier>` omits the extension, auto-complete to `.ts` (then try `.tsz`)
- If the resolved path is a directory: treat it as a “package root” and use `tsz.entry` from its `package.json`

#### Package Names/Subpaths (node_modules)

Starting from `current_dir`, search upwards for `node_modules/<pkg>`:

- Once found, treat that directory as the “package root” and read `tsz.entry` from its `package.json` (for TSZ package validation)

Then:

- If `<specifier>` is the **package root**: resolve to `tsz.entry`
- If `<specifier>` has a **subpath**: treat the subpath as a path under the package root and continue resolving by the “relative path” rules
  - If extension is omitted: complete to `.ts` (then try `.tsz`)
  - If it hits a directory: treat it as a “package root” and read `tsz.entry` from that directory’s `package.json`

Subpath constraints:

- No empty segments, `.`, or `..`
- `\\` is not allowed
- If not found, raise an error

> TSZ’s npm support boundary: only “dependency resolution + source compilation”; it does not implement Node’s complex `exports/conditions` rules, nor execute JS.

## 3. Syntax Subset (v1)

### 3.1 Top-Level Declarations

Only function declarations are supported:

```ts
export function main(): bigint {
  return 42n;
}
```

Constraints:

- Only `function <name>(a: T, b: U, ...): <type> { ... }` is supported
  - Parameters must have type annotations; currently supported types are `number/bigint/boolean/string`
- `export` is only used to expose functions for import by other modules

### 3.2 Statements

- Block statements (real block scope, nesting allowed):
  - `{ <stmt>* }`
  - Rule: `let/const` declared inside a block are only visible within that block and its child blocks
  - Empty blocks are allowed

- `if / else if / else`:
  - `if (<cond>) <stmt>`
  - `if (<cond>) <stmt> else <stmt>`
  - `if (<cond>) <stmt> else if (<cond>) <stmt> ... else <stmt>`
  - Rule: `<cond>` must be of type `boolean`
  - `<stmt>` can be a single statement or a block `{ ... }`

- `while` (and `break/continue`):
  - `while (<cond>) <stmt>`
  - Rule: `<cond>` must be of type `boolean`
  - `break;` / `continue;` are only allowed inside loop bodies (`while/for`), taking effect on the innermost loop

- `for` (and `break/continue`):
  - `for (<init?>; <cond?>; <update?>) <stmt>`
  - `<init?>` (initialization clause) supports:
    - `let <name>: <type>? = <expr>;`
    - `const <name>: <type>? = <expr>;`
    - `<name> = <expr>;` (and `+=` / `-=` / `*=` / `/=`)
    - empty (omitted)
  - `<cond?>` (condition clause) supports:
    - `<expr>` (type must be `boolean`)
    - empty (omitted, semantically equivalent to `true`)
  - `<update?>` (update clause) supports:
    - `<name> = <expr>` (and `+=` / `-=` / `*=` / `/=`)
    - empty (omitted)
  - `break;` / `continue;` are only allowed inside loop bodies (`while/for`), taking effect on the innermost loop
  - Scope: `let/const` bindings declared in `<init?>` are only visible inside the `for` statement (including condition/update/body)

- `let` (function-local variables; block scope):
  - `let <name>: <type>? = <expr>;`
  - Rule: `<type>` may be omitted and inferred from `<expr>`
  - Constraints: only `number/bigint/boolean/string` are supported; must be declared before use; cannot collide with module-level symbols (functions/imports)

- `const` (function-local constants; compile-time constant bindings):
  - `const <name>: <type>? = <expr>;`
  - Rule: `<type>` may be omitted and inferred from `<expr>`
  - Constraints: only `number/bigint/boolean/string` are supported; `<expr>` must be compile-time foldable (literals / unary ops (`-` / `!`) / binary ops (`+ - * / == != < <= > >= && ||`) / references to other `const`; `boolean/string` do not support unary minus); must be declared before use; cannot collide with module-level symbols (functions/imports)

- Assignment (only `let` is writable):
  - `<name> = <expr>;`
  - Constraints:
    - The assignment target must be a declared local `let`; assigning to `const` and function parameters is forbidden
    - The type of `<expr>` must match the variable type (`number/bigint/boolean/string`; `void` is not allowed)
  - Optional sugar: `+=` / `-=` / `*=` / `/=`
    - Semantics: `x <op>= y` ≡ `x = x <op> y`
    - Constraints: `<op>` only works for `number/bigint`, and both sides must have the same type (reuse binary op rules)

- `console.log` (standard output):
  - `console.log();`
  - `console.log(<expr>, <expr>, ...);`
  - Rule: arguments are printed separated by spaces, with a trailing newline
  - Parameter types: `number` / `bigint` / `boolean` / `string`

- `return`:
  - `return <expr>;`
  - `return;` (only when the function return type is `void`)
  - Constraints: non-`void` functions require “all control paths must return a value”; `void` functions may omit the final `return;` (implicit return)

### 3.3 Expressions

- `number` literal (internally `f64`)
- `bigint` literal (internally **fixed-width** `i64`, e.g. `42n`)
- `boolean` literal: `true` / `false`
- `string` literal (UTF-8)
- Identifiers: references to locals/constants/parameters (from `let/const/params`; `const` is currently inlined at compile time)
- Unary ops: `-<expr>` / `!<expr>` (`!` only supports `boolean`)
- Binary ops (arithmetic): `+ - * /` (only for `number/bigint`, and both sides must have the same type)
- Binary ops (comparison): `== != < <= > >=` (`< <= > >=` only for `number/bigint`; `== !=` supports `number/bigint/boolean`; both sides must have the same type; result is `boolean`)
- Binary ops (logical): `&& ||` (only `boolean`; result is `boolean`; short-circuit evaluation)
- Precedence (high → low): `!` > `* /` > `+ -` > `< <= > >=` > `== !=` > `&&` > `||` (all left-associative)
- Parentheses: `(<expr>)`
- Function call: `foo(<expr>, <expr>, ...)`

## 4. Types and Entry ABI

### 4.1 Types

- `number`: `f64`
- `bigint`: `i64` (not arbitrary precision)
- `void`
- `boolean`: `i8` (0/1)
- `string`: pointer (currently implemented as a length-prefixed UTF-8 byte sequence; usable for locals/params/return values)

The entry return type of `main` still only supports `number/bigint/void`.

### 4.2 Entry Function and Exit Code

The entry function must be:

```ts
export function main(): number | bigint | void { ... }
```

Constraints:

- `main` must have **0 parameters** (called by a C ABI `int main()` wrapper without arguments)

Process exit code mapping:

- `void` → 0
- `bigint(i64)` → truncated to `i32`
- `number(f64)` → converted to `i32` (truncated toward zero)

## 5. Toolchain and Platform

- Code generation: Cranelift AOT (native ISA)
- Linker: uses environment variable `CC`, otherwise:
  - macOS/Linux: `cc`
  - Windows: `clang`

## 6. Not Implemented but Decided Direction (Placeholder)

- Memory: ARC/RC + `Weak` (runtime and object layout not implemented yet)
- FFI: C ABI (not implemented yet)

<a id="zh"></a>
# TSZ 规范（v0）

TSZ 是一个以 TypeScript 语法为主、追求可预测静态语义与高性能 AOT 的语言子集。

> 目标：允许“删减坏特性”，不追求 TypeScript/JavaScript 兼容。

## 1. 文件与包入口

### 1.1 源文件扩展名

- 源文件扩展名：`.ts`（推荐）
- 兼容扩展名：`.tsz`（可选）

### 1.2 `tsz build/run` 的入口参数

`tsz build <entry>` / `tsz run <entry>` 的 `<entry>` 支持两种形式：

1. **文件路径**：直接指定某个 `.ts` / `.tsz` 文件（也允许省略扩展名，自动补全）。
2. **目录路径（npm 包根目录）**：目录内必须存在 `package.json`，并通过 `tsz.entry` 指定入口文件。

### 1.3 `package.json` 的 TSZ 字段（最小约束）

TSZ 只关心一个字段：

```json
{
  "name": "your-package",
  "version": "0.0.0",
  "tsz": { "entry": "src/main.ts" }
}
```

规则：

- `tsz.entry` 必须是相对包根目录的路径
- 必须指向 `.ts` / `.tsz` 源码文件
- TSZ **不**读取 `main` / `module` / `exports` / `types` 等 Node/TS 字段

## 2. 模块系统（最小子集）

### 2.1 语法

仅支持命名导入：

```ts
import { foo, bar } from "./lib.ts";
```

约束：

- 仅支持 `import { a, b } from "<specifier>";`
- 不支持 default import、`import * as ns`、`as` 重命名、动态 import
- `<specifier>` 只能是：
  - **相对路径**：以 `./` 或 `../` 开头
  - **包名/包子路径**（node_modules）：例如：
    - 包根：`"left-pad"` / `"@scope/pkg"`
    - 包子路径：`"left-pad/subpath"` / `"@scope/pkg/subpath"`

### 2.2 解析规则（简化版 Node 风格）

#### 相对路径

设当前模块文件为 `current_file`，其目录为 `current_dir`：

- 若 `<specifier>` 省略扩展名，则自动补全为 `.ts`（再尝试 `.tsz`）
- 解析得到的路径为目录时：视为“包根目录”，走 `package.json` 的 `tsz.entry`

#### 包名/包子路径（node_modules）

从 `current_dir` 开始，向上逐级寻找 `node_modules/<pkg>` 目录：

- 找到后，将该目录视为“包根目录”，读取其 `package.json` 的 `tsz.entry`（用于校验 TSZ 包配置）

然后：

- 若 `<specifier>` 是 **包根**：解析为 `tsz.entry`
- 若 `<specifier>` 带 **子路径**：将子路径当作“包根目录下的路径”，按“相对路径”规则继续解析
  - 省略扩展名：补全 `.ts`（再尝试 `.tsz`）
  - 命中目录：视为“包根目录”，读取该目录 `package.json` 的 `tsz.entry`

子路径约束：

- 不允许空段、`.`、`..`
- 不允许使用 `\\`
- 未找到则报错

> TSZ 的 npm 支持边界：仅“依赖解析 + 源码编译”；不实现 Node 的 `exports/conditions` 复杂规则，也不执行 JS。

## 3. 语法子集（v1）

### 3.1 顶层声明

仅支持函数声明：

```ts
export function main(): bigint {
  return 42n;
}
```

约束：

- 仅支持 `function <name>(a: T, b: U, ...): <type> { ... }`
  - 形参必须带类型标注；当前支持 `number/bigint/boolean/string`
- `export` 仅用于把函数暴露给其他模块导入

### 3.2 语句

- 支持块语句（真正块级作用域，允许嵌套）：
  - `{ <stmt>* }`
  - 规则：块内声明的 `let/const` 只在该块及其子块内可见
  - 允许空块

- 支持 `if / else if / else`：
  - `if (<cond>) <stmt>`
  - `if (<cond>) <stmt> else <stmt>`
  - `if (<cond>) <stmt> else if (<cond>) <stmt> ... else <stmt>`
  - 规则：`<cond>` 的类型必须为 `boolean`
  - `<stmt>` 可以是单语句或块 `{ ... }`

- 支持 `while`（以及 `break/continue`）：
  - `while (<cond>) <stmt>`
  - 规则：`<cond>` 的类型必须为 `boolean`
  - `break;` / `continue;` 仅允许出现在循环体内（`while/for`；按最近一层循环生效）

- 支持 `for`（以及 `break/continue`）：
  - `for (<init?>; <cond?>; <update?>) <stmt>`
  - `<init?>`（初始化子句）支持：
    - `let <name>: <type>? = <expr>;`
    - `const <name>: <type>? = <expr>;`
    - `<name> = <expr>;`（以及 `+=` / `-=` / `*=` / `/=`）
    - 空（省略）
  - `<cond?>`（条件子句）支持：
    - `<expr>`（类型必须为 `boolean`）
    - 空（省略，语义等价于 `true`）
  - `<update?>`（更新子句）支持：
    - `<name> = <expr>`（以及 `+=` / `-=` / `*=` / `/=`）
    - 空（省略）
  - `break;` / `continue;` 仅允许出现在循环体内（`while/for`；按最近一层循环生效）
  - 作用域：`for` 的 `<init?>` 声明的 `let/const` 绑定只在该 `for` 语句内部可见（包含条件/更新/循环体）

- 支持 `let`（函数体内局部变量；块级作用域）：
  - `let <name>: <type>? = <expr>;`
  - 规则：`<type>` 可省略，省略时从 `<expr>` 推断类型
  - 约束：当前支持 `number/bigint/boolean/string`；必须先声明后使用；不可与模块级符号（函数/导入）重名

- 支持 `const`（函数体内局部常量；编译期常量绑定）：
  - `const <name>: <type>? = <expr>;`
  - 规则：`<type>` 可省略，省略时从 `<expr>` 推断类型
  - 约束：当前支持 `number/bigint/boolean/string`；`<expr>` 必须可在编译期折叠（字面量/一元运算（`-` / `!`）/二元运算（`+ - * / == != < <= > >= && ||`）/引用其他 `const`；其中 `boolean/string` 不支持一元负号）；必须先声明后使用；不可与模块级符号（函数/导入）重名

- 支持赋值（仅 `let` 可写）：
  - `<name> = <expr>;`
  - 约束：
    - 赋值目标必须是已声明的 `let` 局部变量；禁止给 `const` 与函数参数赋值
    - `<expr>` 的类型必须与变量类型一致（`number/bigint/boolean/string`；不允许 `void`）
  - 可选语法糖：`+=` / `-=` / `*=` / `/=`
    - 语义等价：`x <op>= y` ≡ `x = x <op> y`
    - 约束：`<op>` 仅对 `number/bigint` 生效，且左右类型必须一致（复用二元运算规则）

- 支持 `console.log`（标准输出）：
  - `console.log();`
  - `console.log(<expr>, <expr>, ...);`
  - 规则：参数之间用空格分隔输出，末尾自动追加换行
  - 参数类型：`number` / `bigint` / `boolean` / `string`

- 支持 `return`：
  - `return <expr>;`
  - `return;`（仅当函数返回类型为 `void`）
  - 约束：非 `void` 函数要求“所有控制路径都必须返回一个值”；`void` 函数允许省略末尾 `return;`（隐式返回）

### 3.3 表达式

- `number` 字面量（内部为 `f64`）
- `bigint` 字面量（内部为 **定长** `i64`，例如 `42n`）
- `boolean` 字面量：`true` / `false`
- `string` 字面量（UTF-8）
- 标识符：局部变量/常量/参数引用（来自 `let/const/params`；`const` 当前会被编译期内联）
- 一元运算：`-<expr>` / `!<expr>`（`!` 仅支持 `boolean`）
- 二元运算（算术）：`+ - * /`（仅支持 `number/bigint`，且左右类型必须一致）
- 二元运算（比较）：`== != < <= > >=`（`< <= > >=` 仅支持 `number/bigint`；`== !=` 支持 `number/bigint/boolean`；左右类型必须一致；结果为 `boolean`）
- 二元运算（逻辑）：`&& ||`（仅支持 `boolean`；结果为 `boolean`；短路求值）
- 优先级（高 → 低）：`!` > `* /` > `+ -` > `< <= > >=` > `== !=` > `&&` > `||`（均为左结合）
- 括号：`(<expr>)`
- 函数调用：`foo(<expr>, <expr>, ...)`

## 4. 类型与入口 ABI

### 4.1 类型

- `number`：`f64`
- `bigint`：`i64`（不是任意精度）
- `void`
- `boolean`：`i8`（0/1）
- `string`：指针（当前实现为长度前缀的 UTF-8 字节序列；可用于局部变量/参数/返回值）

`main` 的入口返回类型仍仅支持 `number/bigint/void`。

### 4.2 入口函数与退出码

入口函数必须是：

```ts
export function main(): number | bigint | void { ... }
```

约束：

- `main` 必须是 **0 参数**（由 C ABI `int main()` 包装调用，不传入参数）

进程退出码映射：

- `void` → 0
- `bigint(i64)` → 截断为 `i32`
- `number(f64)` → 转换为 `i32`（向零截断）

## 5. 工具链与平台

- 代码生成：Cranelift AOT（本机 ISA）
- 链接器：使用环境变量 `CC`，否则：
  - macOS/Linux：`cc`
  - Windows：`clang`

## 6. 未实现但已决策的方向（占位）

- 内存：ARC/RC + `Weak`（运行时与对象布局尚未实现）
- FFI：C ABI（尚未实现）
