use crate::common::ast::{Expr, Stmt};
use crate::common::span::Spanned;
use crate::common::types::IProposition;
use crate::frontend::typechecker::TypingContext;
use std::sync::Arc;

pub(super) fn collect_array_modifications<'src>(
    stmts: &[Spanned<Stmt<'src>>],
) -> Vec<(String, Vec<Expr<'src>>)> {
    let mut modifications = Vec::new();
    collect_array_modifications_inner(stmts, &mut modifications);
    modifications
}

fn collect_array_modifications_inner<'src>(
    stmts: &[Spanned<Stmt<'src>>],
    modifications: &mut Vec<(String, Vec<Expr<'src>>)>,
) {
    for stmt in stmts {
        match &stmt.0 {
            Stmt::Assignment { lhs, .. } => {
                if let Some((name, indices)) = extract_array_access(&lhs.0) {
                    modifications.push((name.to_string(), indices));
                }
            }
            Stmt::Expr(expr) => {
                if let Expr::If {
                    then_block,
                    else_block,
                    ..
                } = &expr.0
                {
                    collect_array_modifications_inner(&then_block.statements, modifications);
                    if let Some(else_stmts) = else_block {
                        collect_array_modifications_inner(&else_stmts.statements, modifications);
                    }
                }
            }
            Stmt::For { body, .. } => {
                collect_array_modifications_inner(&body.statements, modifications);
            }
            Stmt::Region { body } => {
                collect_array_modifications_inner(&body.statements, modifications);
            }
            _ => {}
        }
    }
}

pub(super) fn extract_array_access<'src>(
    expr: &Expr<'src>,
) -> Option<(&'src str, Vec<Expr<'src>>)> {
    let mut indices: Vec<Expr<'src>> = Vec::new();
    let mut cur = expr;
    loop {
        match cur {
            Expr::Index { base, index } => {
                indices.push(index.0.clone());
                cur = &base.0;
            }
            Expr::Variable(name) => {
                indices.reverse();
                return Some((name, indices));
            }
            _ => return None,
        }
    }
}

pub(super) fn invalidate_array_props_selectively<'src>(
    ctx: &TypingContext<'src>,
    arr_name: &str,
    assigned_indices: &[Expr<'src>],
) -> TypingContext<'src> {
    use crate::common::ast::BinOp;

    let dummy_span = chumsky::span::SimpleSpan::new(0, 0);
    let arr_name_owned = arr_name.to_string();
    let assigned_indices: Vec<Expr<'src>> = assigned_indices.to_vec();

    ctx.retain_propositions(|prop| {
        if prop.var != arr_name_owned {
            return true;
        }
        match &prop.predicate.0 {
            Expr::Forall { .. } | Expr::Exists { .. } => false,
            Expr::BinOp {
                op: BinOp::Eq, lhs, ..
            } => {
                let Some((root, prop_indices)) = extract_array_access(&lhs.0) else {
                    return true;
                };
                if root != arr_name_owned.as_str() || prop_indices.len() != assigned_indices.len() {
                    return true;
                }

                let mut all_concrete_equal = true;
                for (p, a) in prop_indices.iter().zip(assigned_indices.iter()) {
                    match (ctx.resolve_expr_to_int(p), ctx.resolve_expr_to_int(a)) {
                        (Some(pi), Some(ai)) if pi != ai => return true,
                        (Some(_), Some(_)) => {}
                        _ => all_concrete_equal = false,
                    }
                }
                if all_concrete_equal {
                    return false;
                }

                let mut goal_expr: Option<Expr<'src>> = None;
                for (p, a) in prop_indices.iter().zip(assigned_indices.iter()) {
                    let ne = Expr::BinOp {
                        op: BinOp::NotEq,
                        lhs: Box::new((a.clone(), dummy_span)),
                        rhs: Box::new((p.clone(), dummy_span)),
                    };
                    goal_expr = Some(match goal_expr.take() {
                        None => ne,
                        Some(acc) => Expr::BinOp {
                            op: BinOp::Or,
                            lhs: Box::new((acc, dummy_span)),
                            rhs: Box::new((ne, dummy_span)),
                        },
                    });
                }
                let goal = IProposition {
                    var: arr_name_owned.clone(),
                    predicate: Arc::new((goal_expr.unwrap(), dummy_span)),
                };
                crate::frontend::typechecker::smt::check_provable(ctx, &goal)
            }
            _ => true,
        }
    })
}
