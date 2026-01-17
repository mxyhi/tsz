use super::*;

#[test]
fn parse_minimal_main() {
    let src = "export function main(): bigint { return 42n; }";
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.imports.len(), 0);
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].name, "main");
    assert_eq!(m.functions[0].return_type, Type::BigInt);
}

#[test]
fn parse_import_and_call() {
    let src = r#"
import { foo, bar } from "./lib.ts";
export function main(): bigint { return foo(); }
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.imports.len(), 1);
    assert_eq!(m.imports[0].names.len(), 2);
    assert_eq!(m.imports[0].names[0].name, "foo");
    assert_eq!(m.imports[0].names[1].name, "bar");
    assert_eq!(m.imports[0].from, "./lib.ts");
    assert_eq!(m.functions.len(), 1);
}

#[test]
fn parse_void_return_stmt() {
    let src = "export function main(): void { return; }";
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let Some(Stmt::Return { expr, .. }) = m.functions[0].body.first() else {
        panic!("expected return stmt");
    };
    assert!(expr.is_none());
}

#[test]
fn parse_console_log_stmt() {
    let src = r#"
export function main(): void {
  console.log("hi", 1, 2n);
  return;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].body.len(), 2);

    let Some(Stmt::ConsoleLog { args, .. }) = m.functions[0].body.first() else {
        panic!("expected console.log stmt");
    };
    assert_eq!(args.len(), 3);
}

#[test]
fn parse_let_stmt() {
    let src = r#"
export function main(): bigint {
  let x: bigint = 1n;
  let y = -x;
  return y;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].body.len(), 3);
}

#[test]
fn parse_const_stmt() {
    let src = r#"
export function main(): bigint {
  const x: bigint = 1n;
  const y = -x;
  return y;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].body.len(), 3);
}

#[test]
fn parse_assign_stmt() {
    let src = r#"
export function main(): bigint {
  let x: bigint = 1n;
  x = -x;
  return x;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].body.len(), 3);

    let Some(Stmt::Assign { name, .. }) = m.functions[0].body.get(1) else {
        panic!("expected assign stmt");
    };
    assert_eq!(name, "x");
}

#[test]
fn parse_compound_assign_desugars_to_binary_expr() {
    let src = r#"
export function main(): bigint {
  let x: bigint = 1n;
  x += 2n;
  return x;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    assert_eq!(m.functions[0].body.len(), 3);

    let Some(Stmt::Assign { expr, .. }) = m.functions[0].body.get(1) else {
        panic!("expected assign stmt");
    };
    let Expr::Binary { op: BinaryOp::Add, left, right, .. } = expr else {
        panic!("expected desugared binary add");
    };
    assert!(matches!(left.as_ref(), Expr::Ident { name, .. } if name == "x"));
    assert!(matches!(right.as_ref(), Expr::BigInt { value, .. } if *value == 2));
}

#[test]
fn parse_function_params() {
    let src = r#"
export function add(a: bigint, b: bigint): bigint { return a; }
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    assert_eq!(m.functions.len(), 1);
    let f = &m.functions[0];
    assert_eq!(f.name, "add");
    assert_eq!(f.params.len(), 2);
    assert_eq!(f.params[0].name, "a");
    assert_eq!(f.params[0].ty, Type::BigInt);
    assert_eq!(f.params[1].name, "b");
    assert_eq!(f.params[1].ty, Type::BigInt);
}

#[test]
fn parse_call_with_args() {
    let src = r#"
function add(a: bigint, b: bigint): bigint { return a; }
export function main(): bigint { return add(1n, 2n); }
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[1];
    let Some(Stmt::Return { expr: Some(Expr::Call { callee, args, .. }), .. }) = f.body.first() else {
        panic!("expected return add(1n, 2n)");
    };
    assert_eq!(callee, "add");
    assert_eq!(args.len(), 2);
}

#[test]
fn parse_binary_ops_precedence_and_paren() {
    let src = r#"
export function main(): number {
  return (1 + 2) * 3 + 4 / 2;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[0];
    let Some(Stmt::Return { expr: Some(expr), .. }) = f.body.first() else {
        panic!("expected return");
    };

    let Expr::Binary { op: BinaryOp::Add, left, right, .. } = expr else {
        panic!("expected top-level add");
    };

    // Left: (1 + 2) * 3
    let Expr::Binary {
        op: BinaryOp::Mul,
        left: mul_l,
        right: mul_r,
        ..
    } = left.as_ref()
    else {
        panic!("expected mul");
    };
    assert!(matches!(mul_r.as_ref(), Expr::Number { value, .. } if *value == 3.0));
    assert!(matches!(mul_l.as_ref(), Expr::Binary { op: BinaryOp::Add, .. }));

    // Right: 4 / 2
    assert!(matches!(right.as_ref(), Expr::Binary { op: BinaryOp::Div, .. }));
}

#[test]
fn parse_block_stmt() {
    let src = r#"
export function main(): void {
  { let x = 1n; }
  return;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[0];
    let Some(Stmt::Block { stmts, .. }) = f.body.first() else {
        panic!("expected block stmt");
    };
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_if_else_stmt() {
    let src = r#"
export function main(): bigint {
  if (true) { return 1n; } else { return 2n; }
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[0];
    let Some(Stmt::If { else_branch, .. }) = f.body.first() else {
        panic!("expected if stmt");
    };
    assert!(else_branch.is_some());
}

#[test]
fn parse_while_stmt_with_break() {
    let src = r#"
export function main(): void {
  while (true) {
    break;
  }
  return;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[0];
    let Some(Stmt::While { .. }) = f.body.first() else {
        panic!("expected while stmt");
    };
}

#[test]
fn parse_while_stmt_with_continue_single_stmt_body() {
    let src = r#"
export function main(): void {
  while (true) continue;
  return;
}
"#;
    let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
    let f = &m.functions[0];
    let Some(Stmt::While { .. }) = f.body.first() else {
        panic!("expected while stmt");
    };
}
