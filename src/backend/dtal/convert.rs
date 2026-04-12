//! Conversion from source AST expressions to DTAL constraints/indices
//!
//! These functions convert `Expr` (source AST) to `Constraint`/`IndexExpr`
//! (first-order DTAL domain). They are used during TIR→DTAL codegen and
//! during `DtalType::from_itype` conversion.

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};

/// Convert an Expr to a Constraint (for boolean expressions)
pub fn expr_to_constraint(expr: &Expr) -> Option<Constraint> {
    match expr {
        Expr::BinOp { op, lhs, rhs } => {
            match op {
                // Comparison operators -> Constraint
                BinOp::Eq => Some(Constraint::Eq(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::NotEq => Some(Constraint::Ne(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Lt => Some(Constraint::Lt(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Lte => Some(Constraint::Le(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Gt => Some(Constraint::Gt(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Gte => Some(Constraint::Ge(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                // Logical operators
                BinOp::And => Some(Constraint::And(
                    Box::new(expr_to_constraint(&lhs.0)?),
                    Box::new(expr_to_constraint(&rhs.0)?),
                )),
                BinOp::Or => Some(Constraint::Or(
                    Box::new(expr_to_constraint(&lhs.0)?),
                    Box::new(expr_to_constraint(&rhs.0)?),
                )),
                BinOp::Implies => Some(Constraint::Implies(
                    Box::new(expr_to_constraint(&lhs.0)?),
                    Box::new(expr_to_constraint(&rhs.0)?),
                )),
                // Arithmetic operators can't be converted to constraints directly
                _ => None,
            }
        }
        Expr::UnaryOp {
            op: UnaryOp::Not,
            cond,
        } => Some(Constraint::Not(Box::new(expr_to_constraint(&cond.0)?))),
        Expr::Literal(Literal::Bool(true)) => Some(Constraint::True),
        Expr::Literal(Literal::Bool(false)) => Some(Constraint::False),
        Expr::Forall {
            var,
            start,
            end,
            body,
        } => Some(Constraint::Forall {
            var: var.to_string(),
            lower: expr_to_index(&start.0)?,
            upper: expr_to_index(&end.0)?,
            body: Box::new(expr_to_constraint(&body.0)?),
        }),
        Expr::Exists {
            var,
            start,
            end,
            body,
        } => Some(Constraint::Exists {
            var: var.to_string(),
            lower: expr_to_index(&start.0)?,
            upper: expr_to_index(&end.0)?,
            body: Box::new(expr_to_constraint(&body.0)?),
        }),
        _ => None,
    }
}

/// Convert an Expr to an IndexExpr (for arithmetic expressions)
pub fn expr_to_index(expr: &Expr) -> Option<IndexExpr> {
    match expr {
        Expr::Literal(Literal::Int(n)) => Some(IndexExpr::Const(*n)),
        Expr::Variable(name) => Some(IndexExpr::Var(name.to_string())),
        Expr::BinOp { op, lhs, rhs } => {
            let l = expr_to_index(&lhs.0)?;
            let r = expr_to_index(&rhs.0)?;
            match op {
                BinOp::Add => Some(IndexExpr::Add(Box::new(l), Box::new(r))),
                BinOp::Sub => Some(IndexExpr::Sub(Box::new(l), Box::new(r))),
                BinOp::Mul => Some(IndexExpr::Mul(Box::new(l), Box::new(r))),
                BinOp::Div => Some(IndexExpr::Div(Box::new(l), Box::new(r))),
                BinOp::Mod => Some(IndexExpr::Mod(Box::new(l), Box::new(r))),
                _ => None,
            }
        }
        // Unary negation: -e  →  Sub(0, e)
        Expr::UnaryOp {
            op: UnaryOp::Neg,
            cond,
        } => {
            let inner = expr_to_index(&cond.0)?;
            Some(IndexExpr::Sub(
                Box::new(IndexExpr::Const(0)),
                Box::new(inner),
            ))
        }
        Expr::Index { base, index } => {
            if let Expr::Variable(name) = &base.0 {
                let idx = expr_to_index(&index.0)?;
                Some(IndexExpr::Select(name.to_string(), Box::new(idx)))
            } else {
                None
            }
        }
        _ => None,
    }
}
