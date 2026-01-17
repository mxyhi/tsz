// 多文件示例：包入口（通过 package.json 的 tsz.entry 指定）
import { fortyTwo } from "./lib.ts";

export function main(): bigint {
  return fortyTwo();
}
