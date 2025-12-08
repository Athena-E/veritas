// Helper functions for type checking
// constant folding, proposition extraction, context joining

use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::span::Span;
use crate::common::types::{IProposition, IType, IValue};
use chumsky::prelude::SimpleSpan;
use std::sync::Arc;

/// Constant folding (join-op from formal semantics)
/// Attempts to compute the result type of binary operations on singleton types
pub fn join_op<'src>(op: BinOp, ty1: &IType<'src>, ty2: &IType<'src>) -> IType<'src> {
    match (op, ty1, ty2) {
        // Arithmetic on singleton ints
        (BinOp::Add, IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) => {
            IType::SingletonInt(IValue::Int(n1 + n2))
        }
        (BinOp::Sub, IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) => {
            IType::SingletonInt(IValue::Int(n1 - n2))
        }
        (BinOp::Mul, IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) => {
            IType::SingletonInt(IValue::Int(n1 * n2))
        }

        // Fallback to base int type
        (BinOp::Add | BinOp::Sub | BinOp::Mul, _, _) => IType::Int,

        // Comparisons always produce bool
        _ => IType::Bool,
    }
}

/// Converts comparison expressions into propositions
pub fn extract_proposition<'src>(expr: &Expr<'src>) -> Option<IProposition<'src>> {
    match expr {
        // Simple comparisons: x op n
        Expr::BinOp { op: BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NotEq, lhs, .. } => {
            // Check if lhs is a variable
            if let Expr::Variable(var_name) = &lhs.0 {
                Some(IProposition {
                    var: var_name.to_string(),
                    predicate: Arc::new((expr.clone(), SimpleSpan::new(0, 0))),
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Negate a proposition
/// for else-branches
pub fn negate_proposition<'src>(prop: &IProposition<'src>) -> IProposition<'src> {
    let negated_expr = negate_expr(&prop.predicate.0);

    IProposition {
        var: prop.var.clone(),
        predicate: Arc::new((negated_expr, prop.predicate.1)),
    }
}

/// Negate an expression
fn negate_expr<'src>(expr: &Expr<'src>) -> Expr<'src> {
    match expr {
        // Negate comparisons by flipping the operator
        Expr::BinOp { op: BinOp::Lt, lhs, rhs } => Expr::BinOp {
            op: BinOp::Gte,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp { op: BinOp::Lte, lhs, rhs } => Expr::BinOp {
            op: BinOp::Gt,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp { op: BinOp::Gt, lhs, rhs } => Expr::BinOp {
            op: BinOp::Lte,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp { op: BinOp::Gte, lhs, rhs } => Expr::BinOp {
            op: BinOp::Lt,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp { op: BinOp::Eq, lhs, rhs } => Expr::BinOp {
            op: BinOp::NotEq,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp { op: BinOp::NotEq, lhs, rhs } => Expr::BinOp {
            op: BinOp::Eq,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },

        _ => Expr::UnaryOp {
            op: UnaryOp::Not,
            cond: Box::new((expr.clone(), SimpleSpan::new(0, 0))),
        },
    }
}

/// Check array bounds using the actual index expression
/// This preserves variable names so SMT can use context propositions
pub fn check_array_bounds_expr<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    index_expr: &Expr<'src>,
    index_ty: &IType<'src>,
    array_size: &IValue,
    array_type: &IType<'src>,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::{check_provable, TypeError};

    let dummy_span = SimpleSpan::new(0, 0);

    // Create proposition: 0 <= index_expr
    let lower_bound = IProposition {
        var: "idx".to_string(),
        predicate: Arc::new((
            Expr::BinOp {
                op: BinOp::Gte,
                lhs: Box::new((index_expr.clone(), dummy_span)),
                rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy_span)),
            },
            dummy_span,
        )),
    };

    // Create proposition: index_expr < size
    let upper_bound = IProposition {
        var: "idx".to_string(),
        predicate: Arc::new((
            Expr::BinOp {
                op: BinOp::Lt,
                lhs: Box::new((index_expr.clone(), dummy_span)),
                rhs: Box::new((value_to_expr_from_ivalue(array_size), dummy_span)),
            },
            dummy_span,
        )),
    };

    // Check lower bound: 0 <= index
    if !check_provable(ctx, &lower_bound) {
        return Err(TypeError::InvalidArrayAccess {
            array_type: array_type.clone(),
            index_expr: format!("{}", index_ty),
            reason: "Index may be negative".to_string(),
            span,
        });
    }

    // Check upper bound: index < size
    if !check_provable(ctx, &upper_bound) {
        return Err(TypeError::InvalidArrayAccess {
            array_type: array_type.clone(),
            index_expr: format!("{}", index_ty),
            reason: format!("Index may be >= array size ({})", array_size),
            span,
        });
    }

    Ok(())
}

/// Convert IValue to expression for SMT translation
fn value_to_expr_from_ivalue<'src>(val: &'src IValue) -> Expr<'src> {
    match val {
        IValue::Int(n) => Expr::Literal(Literal::Int(*n)),
        IValue::Symbolic(s) => Expr::Variable(s.as_str()),
        IValue::Bool(b) => Expr::Literal(Literal::Bool(*b)),
    }
}
