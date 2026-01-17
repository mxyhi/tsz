// Local variable example: let
export function main(): bigint {
  let x: bigint = 7n;
  console.log("x", x);

  let y = -x;
  console.log("y", y);

  return x;
}
