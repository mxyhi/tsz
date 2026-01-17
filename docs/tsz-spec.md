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
