# Task Plan: TSZ（TS 语法子集）高性能 AOT 编译器

## Goal
在 macOS/Linux/Windows 上提供一个可用的 `tsz build/run` 最小闭环：读取 TSZ 源码 → 词法/语法 → 类型检查（最小）→ AOT 生成原生可执行文件 → 运行返回码验证。

## Phases
- [ ] Phase 1: 初始化 Rust 工作区与 CLI（当前）
- [ ] Phase 2: 定义 TSZ 子集与约束文档
- [ ] Phase 3: 实现 lexer/parser 最小子集
- [ ] Phase 4: 实现类型检查与 HIR
- [ ] Phase 5: 实现 Cranelift AOT 代码生成
- [ ] Phase 6: 实现链接与运行（`tsz run`）
- [ ] Phase 7: 加入最小模块解析与 npm 入口
- [ ] Phase 8: 添加示例与测试基准

## Key Questions
1. TSZ 源文件扩展名与包入口规范：用 `.tsz` 还是直接 `.ts`（不兼容 TS 语义也没问题）？
2. Windows 工具链基线：要求 `clang`/`lld` 还是 MSVC 链接器？如何自动探测？
3. ARC/RC + `Weak` 的运行时形态：对象布局、引用计数策略（非原子/原子）、跨线程语义。
4. npm 支持边界：仅“依赖解析 + 源码编译”还是也要兼容 `exports/conditions` 等 Node 复杂规则？

## Decisions Made
- 方案：A（原生 AOT）
- 平台：全平台导向（macOS/Linux/Windows），先 Host 可用，再补齐差异
- 内存：A1（ARC/RC + `Weak`）
- FFI：需要（C ABI）
- 不新增语法：不引入新关键字/操作符/字面量
- `bigint`：重定义为定长 `i64`（放弃 TS 兼容）
- npm：用于分发/解析；依赖包内源码必须遵守 TSZ 子集（不运行 JS）

## Errors Encountered
- 暂无（后续遇到会记录：错误现象 → 根因 → 解决方式）

## Status
**Currently in Phase 1** - 初始化 workspace/CLI 结构，并打通最小 build 管线的骨架。

