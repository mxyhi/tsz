// Stdout example: console.log(...)
//
// Demonstrates "syntax polish" additions:
// - trailing comma in call args
// - numeric scientific notation: 1e3 / 1E-3
// - string escapes: \n \t \\ \" \' \uXXXX
export function main(): bigint {
  console.log("hello", 1, 2n);
  console.log("neg", -1, -2n);
  console.log("scientific", 1e3, 1E-3,);
  console.log("escapes", "a\nb\tc\\d\"e\'f\u0041",);
  console.log();
  return 0n;
}
