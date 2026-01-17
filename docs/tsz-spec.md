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
  - **包名**：例如 `"left-pad"` / `"@scope/pkg"`

### 2.2 解析规则（简化版 Node 风格）

#### 相对路径

设当前模块文件为 `current_file`，其目录为 `current_dir`：

- 若 `<specifier>` 省略扩展名，则自动补全为 `.ts`（再尝试 `.tsz`）
- 解析得到的路径为目录时：视为“包根目录”，走 `package.json` 的 `tsz.entry`

#### 包名（node_modules）

从 `current_dir` 开始，向上逐级寻找 `node_modules/<pkg>` 目录：

- 找到后，将该目录视为“包根目录”，读取其 `package.json` 的 `tsz.entry`
- 未找到则报错

> TSZ 的 npm 支持边界：仅“依赖解析 + 源码编译”；不实现 Node 的 `exports/conditions` 复杂规则，也不执行 JS。

## 3. 语法子集（v0）

### 3.1 顶层声明

仅支持函数声明：

```ts
export function main(): bigint {
  return 42n;
}
```

约束：

- 仅支持 `function <name>(): <type> { ... }`
- 仅支持 0 参数
- `export` 仅用于把函数暴露给其他模块导入

### 3.2 语句

- 支持 `console.log`（标准输出）：
  - `console.log();`
  - `console.log(<expr>, <expr>, ...);`
  - 规则：参数之间用空格分隔输出，末尾自动追加换行
  - 约束：函数体允许写多条 `console.log(...)`，但最后一条语句必须是 `return`

- 支持 `return`：
  - `return <expr>;`
  - `return;`（仅当函数返回类型为 `void`）

### 3.3 表达式

- `number` 字面量（内部为 `f64`）
- `bigint` 字面量（内部为 **定长** `i64`，例如 `42n`）
- `string` 字面量（仅用于 `console.log`；不提供通用 string 运行时）
- 一元负号：`-<expr>`
- 0 参函数调用：`foo()`

## 4. 类型与入口 ABI

### 4.1 类型

- `number`：`f64`
- `bigint`：`i64`（不是任意精度）
- `void`

`boolean/string` 仅保留占位（当前实现不支持作为入口返回类型，也不提供运行时）。

### 4.2 入口函数与退出码

入口函数必须是：

```ts
export function main(): number | bigint | void { ... }
```

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
