use crate::HirStmt;

pub(super) fn stmts_may_fallthrough(stmts: &[HirStmt]) -> bool {
    let mut can_reach_next = true;
    for stmt in stmts {
        if !can_reach_next {
            break;
        }
        can_reach_next = stmt_may_fallthrough(stmt);
    }
    can_reach_next
}

fn stmt_may_fallthrough(stmt: &HirStmt) -> bool {
    match stmt {
        HirStmt::Let { .. } | HirStmt::Assign { .. } | HirStmt::ConsoleLog { .. } | HirStmt::Error { .. } => true,
        HirStmt::Return { .. } | HirStmt::Break { .. } | HirStmt::Continue { .. } => false,
        HirStmt::If {
            then_body,
            else_body,
            ..
        } => {
            let then_fall = stmts_may_fallthrough(then_body);
            match else_body {
                None => true,
                Some(else_body) => then_fall || stmts_may_fallthrough(else_body),
            }
        }
        HirStmt::While { .. } => {
            // Without constant-condition analysis, conservatively assume `while` may fall through
            // (the condition can be false on entry).
            true
        }
    }
}
