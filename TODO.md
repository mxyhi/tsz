# TODO

## Syntax / Language

- [x] module system (min)
  - [x] `export function ...`
  - [x] `import { a, b } from "<specifier>";`
  - [x] resolver: `./rel` → `.ts|.tsz`
  - [x] resolver: `package.json#tsz.entry`
  - [x] resolver: `node_modules/<pkg>`
  - [x] resolver: `<pkg>/<subpath>`

- [x] `console.log`
  - [x] `console.log();` / `console.log(<expr>, <expr>, ...);`
  - [x] stdout: `arg1 arg2 ...\n`
  - [x] types: `number | bigint | boolean | string`

- [x] primitives (v0)
  - [x] `number` / `bigint` / `void`
  - [x] `boolean`
  - [x] `string`
  - [x] literals: `123` / `3.14`
  - [x] literals: `123n`
  - [x] literals: `true` / `false`
  - [x] literals: `"..."` / `'...'`

- [x] `let`
  - [x] `let <name>: <type>? = <expr>;`
  - [x] pipeline: `lexer -> parser -> typecheck -> codegen -> build/run`

- [x] `const`
  - [x] `const <name>: <type>? = <expr>;`
  - [x] fold: literals / unary `-`
  - [x] fold: `const` refs
  - [x] fold: `+ - * /` / `== != < <= > >=` / `&& || !`

- [x] `function`
  - [x] `function f(): R { return <expr>; }`
  - [x] `function f(a: T, b: U): R { ... }`
  - [x] block: `{ <stmt>* }`
  - [x] expr: `ident` / `binary` / `()` / `call(args)`

- [x] `return`
  - [x] `return <expr>;`
  - [x] `return;`
  - [x] type: `void` vs non-`void`

- [x] assignment / mutability (v2+)
  - [x] `<name> = <expr>;`
  - [x] `+=` / `-=` / `*=` / `/=`

- [x] control flow (v2+)
  - [x] `{ ... }`
  - [x] `while` / `break` / `continue`
  - [x] `if / else if / else`
  - [x] `for` / `break` / `continue`

- [x] ops / compare (v2+)
  - [x] `== != < <= > >=`
  - [x] `&& || !`

- [ ] syntax polish
  - [ ] numeric: `1e3` / `1E-3`
  - [ ] trailing comma: params / call args / import names
  - [ ] string escapes: `\n` `\t` `\\` `\"` `\'` `\uXXXX`

## Compiler / Tooling

- [x] CLI: `tsz build` / `tsz run` / `--opt none|speed`
- [x] CLI: `tsz check`
- [x] diagnostics: `span -> line:col + snippet`
- [x] diagnostics: multi-error
- [x] emit: `--emit obj` / `--emit ir`
- [x] artifacts: temp `.o/.obj` cleanup

## Performance

- [x] benchmark: compile + runtime
- [x] parallel module load/parse (deterministic)
- [x] optlevel: `size` (if useful)

## Runtime / Ecosystem

- [ ] memory: `ARC/RC + Weak`
- [ ] layout + core containers: `string` / `array`
- [ ] FFI: `C ABI` (declare / call / link)
