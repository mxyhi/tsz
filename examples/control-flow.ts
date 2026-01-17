// Control flow example: block / if / else if / else / while / for / break / continue
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

  // if / else if / else
  let a = false;
  let b = false;
  if (a) {
    x = 0n;
  } else if (b) {
    x = 1n;
  } else {
    x = x + 41n;
  }

  // for + break/continue (no comparisons yet, so use boolean control flags)
  let out: bigint = 0n;
  let first_for = true;
  for (let i: bigint = 0n; true; i += 1n) {
    if (first_for) {
      first_for = false;
      continue;
    }
    out = i;
    break;
  }
  console.log("for i", out);

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
