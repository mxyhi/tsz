// Local constant example: const (compile-time folding + inlining)
export function main(): bigint {
  const x: bigint = 7n;
  const y = -x;
  const z = -y;

  console.log("x", x);
  console.log("y", y);
  console.log("z", z);

  return z;
}

