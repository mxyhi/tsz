// Multi-file example: package entry (via package.json tsz.entry)
// - Import names support trailing comma
// - Function params and call args support trailing comma
import { fortyTwo, } from "./lib.ts";

function id(x: bigint,): bigint {
  return x;
}

export function main(): bigint {
  return id(fortyTwo(),);
}
