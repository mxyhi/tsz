// Assignment example: = and compound assignments (+=, -=, *=, /=)
export function main(): bigint {
  let x: bigint = 40n;
  x += 2n;
  console.log("x", x);

  x = x - 1n;
  console.log("x", x);

  return x;
}

