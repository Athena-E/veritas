use crate::common::ast::{Block, Expr, Literal, Stmt};

/// Substitute a variable with a literal integer in an expression.
pub(super) fn substitute_var_with_literal<'src>(
    expr: &Expr<'src>,
    var_name: &str,
    value: i128,
) -> Expr<'src> {
    match expr {
        Expr::Variable(name) if *name == var_name => Expr::Literal(Literal::Int(value)),
        Expr::Variable(_) => expr.clone(),
        Expr::Literal(_) => expr.clone(),
        Expr::Error => Expr::Error,
        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((substitute_var_with_literal(&lhs.0, var_name, value), lhs.1)),
            rhs: Box::new((substitute_var_with_literal(&rhs.0, var_name, value), rhs.1)),
        },
        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((
                substitute_var_with_literal(&cond.0, var_name, value),
                cond.1,
            )),
        },
        Expr::Borrow { kind, expr } => Expr::Borrow {
            kind: *kind,
            expr: Box::new((
                substitute_var_with_literal(&expr.0, var_name, value),
                expr.1,
            )),
        },
        Expr::Call { func_name, args } => Expr::Call {
            func_name,
            args: (
                args.0
                    .iter()
                    .map(|(arg, span)| (substitute_var_with_literal(arg, var_name, value), *span))
                    .collect(),
                args.1,
            ),
        },
        Expr::Index { base, index } => Expr::Index {
            base: Box::new((
                substitute_var_with_literal(&base.0, var_name, value),
                base.1,
            )),
            index: Box::new((
                substitute_var_with_literal(&index.0, var_name, value),
                index.1,
            )),
        },
        Expr::ArrayInit { value: v, length } => Expr::ArrayInit {
            value: Box::new((substitute_var_with_literal(&v.0, var_name, value), v.1)),
            length: Box::new((
                substitute_var_with_literal(&length.0, var_name, value),
                length.1,
            )),
        },
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            Expr::If {
                cond: Box::new((
                    substitute_var_with_literal(&cond.0, var_name, value),
                    cond.1,
                )),
                then_block: Block {
                    statements: then_block
                        .statements
                        .iter()
                        .map(|(stmt, span)| (substitute_var_in_stmt(stmt, var_name, value), *span))
                        .collect(),
                    trailing_expr: then_block.trailing_expr.as_ref().map(|e| {
                        Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))
                    }),
                },
                else_block: else_block.as_ref().map(|block| Block {
                    statements: block
                        .statements
                        .iter()
                        .map(|(stmt, span)| (substitute_var_in_stmt(stmt, var_name, value), *span))
                        .collect(),
                    trailing_expr: block.trailing_expr.as_ref().map(|e| {
                        Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))
                    }),
                }),
            }
        }
        Expr::Forall {
            var,
            start,
            end,
            body,
        } => {
            if *var == var_name {
                expr.clone()
            } else {
                Expr::Forall {
                    var,
                    start: Box::new((
                        substitute_var_with_literal(&start.0, var_name, value),
                        start.1,
                    )),
                    end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
                    body: Box::new((
                        substitute_var_with_literal(&body.0, var_name, value),
                        body.1,
                    )),
                }
            }
        }
        Expr::Exists {
            var,
            start,
            end,
            body,
        } => {
            if *var == var_name {
                expr.clone()
            } else {
                Expr::Exists {
                    var,
                    start: Box::new((
                        substitute_var_with_literal(&start.0, var_name, value),
                        start.1,
                    )),
                    end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
                    body: Box::new((
                        substitute_var_with_literal(&body.0, var_name, value),
                        body.1,
                    )),
                }
            }
        }
    }
}

/// Substitute a variable with a literal integer in a statement.
fn substitute_var_in_stmt<'src>(stmt: &Stmt<'src>, var_name: &str, value: i128) -> Stmt<'src> {
    match stmt {
        Stmt::Let {
            is_mut,
            name,
            ty,
            value: v,
        } => Stmt::Let {
            is_mut: *is_mut,
            name,
            ty: ty.clone(),
            value: (substitute_var_with_literal(&v.0, var_name, value), v.1),
        },
        Stmt::Assignment { lhs, rhs } => Stmt::Assignment {
            lhs: (substitute_var_with_literal(&lhs.0, var_name, value), lhs.1),
            rhs: (substitute_var_with_literal(&rhs.0, var_name, value), rhs.1),
        },
        Stmt::Return { expr } => Stmt::Return {
            expr: Box::new((
                substitute_var_with_literal(&expr.0, var_name, value),
                expr.1,
            )),
        },
        Stmt::Expr(e) => Stmt::Expr((substitute_var_with_literal(&e.0, var_name, value), e.1)),
        Stmt::For {
            var,
            start,
            end,
            invariant,
            body,
        } => Stmt::For {
            var,
            start: Box::new((
                substitute_var_with_literal(&start.0, var_name, value),
                start.1,
            )),
            end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
            invariant: invariant
                .as_ref()
                .map(|(inv, span)| (substitute_var_with_literal(inv, var_name, value), *span)),
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|(s, span)| (substitute_var_in_stmt(s, var_name, value), *span))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))),
            },
        },
        Stmt::While {
            condition,
            invariant,
            body,
        } => Stmt::While {
            condition: Box::new((
                substitute_var_with_literal(&condition.0, var_name, value),
                condition.1,
            )),
            invariant: invariant
                .as_ref()
                .map(|(inv, span)| (substitute_var_with_literal(inv, var_name, value), *span)),
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|(s, span)| (substitute_var_in_stmt(s, var_name, value), *span))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))),
            },
        },
        Stmt::Region { body } => Stmt::Region {
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|(s, span)| (substitute_var_in_stmt(s, var_name, value), *span))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))),
            },
        },
    }
}
