// Control flow example: block / if / else / while / break / continue
export function main(): bigint {
  let x: bigint = 1n;

  // Block scope (shadowing is allowed)
  {
    let x: bigint = 2n;
    console.log("inner x", x);
    {
      let x: bigint = 3n;
      console.log("nested x", x);
    }
  }
  console.log("outer x", x);

  // if / else
  let cond = false;
  if (cond) {
    x = 0n;
  } else {
    x = x + 41n;
  }

  // while + break/continue (no comparisons yet, so use boolean control flags)
  let first = true;
  while (true) {
    if (first) {
      first = false;
      continue;
    }
    break;
  }

  return x;
}

