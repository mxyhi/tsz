// Syntax polish example:
// - numeric scientific notation: 1e3 / 1E-3
// - trailing comma: import names / function params / call args / console.log args
// - string escapes: \n \t \\ \" \' \uXXXX
import { fortyTwo, } from "./syntax-polish-lib.ts";

function id(x: bigint,): bigint {
  return x;
}

export function main(): bigint {
  console.log("number", 1e3, 1E-3,);
  console.log("escapes", "a\nb\tc\\d\"e\'f\u0041",);
  return id(fortyTwo(),);
}

