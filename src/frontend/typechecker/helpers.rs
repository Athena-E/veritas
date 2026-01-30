// Helper functions for type checking
// constant folding, proposition extraction, context joining, SMT synthesis

use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::span::Span;
use crate::common::types::{IProposition, IType, IValue};
use chumsky::prelude::SimpleSpan;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

/// Global counter for generating fresh variable names
static FRESH_VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a fresh variable name that won't conflict with user variables
/// Uses the prefix `_synth_` which is unlikely to be used by programmers
pub fn fresh_var_name() -> String {
    let id = FRESH_VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("_synth_{}", id)
}

/// Reset the fresh variable counter (useful for testing)
#[allow(dead_code)]
pub fn reset_fresh_var_counter() {
    FRESH_VAR_COUNTER.store(0, Ordering::SeqCst);
}

/// Build a refined type that captures the expression's value symbolically
/// For expr `n + n`, produces `{v: int | v = n + n}`
/// This enables SMT-based synthesis: the solver can derive properties
/// from the equality constraint combined with known refinements
pub fn build_equality_refinement<'src>(expr: &Expr<'src>, span: Span) -> IType<'src> {
    let bound_var = fresh_var_name();
    let bound_var_leaked: &'src str = Box::leak(bound_var.clone().into_boxed_str());

    let v_expr = Expr::Variable(bound_var_leaked);

    // Build predicate: v = expr
    let eq_predicate = Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((v_expr, span)),
        rhs: Box::new((expr.clone(), span)),
    };

    IType::RefinedInt {
        base: Arc::new(IType::Int),
        prop: IProposition {
            var: bound_var,
            predicate: Arc::new((eq_predicate, span)),
        },
    }
}

/// Constant folding (join-op from formal semantics)
/// Attempts to compute the result type of binary operations on singleton types
pub fn join_op<'src>(op: BinOp, ty1: &IType<'src>, ty2: &IType<'src>) -> IType<'src> {
    match (op, ty1, ty2) {
        // Arithmetic on singleton ints
        (
            BinOp::Add,
            IType::SingletonInt(IValue::Int(n1)),
            IType::SingletonInt(IValue::Int(n2)),
        ) => IType::SingletonInt(IValue::Int(n1 + n2)),
        (
            BinOp::Sub,
            IType::SingletonInt(IValue::Int(n1)),
            IType::SingletonInt(IValue::Int(n2)),
        ) => IType::SingletonInt(IValue::Int(n1 - n2)),
        (
            BinOp::Mul,
            IType::SingletonInt(IValue::Int(n1)),
            IType::SingletonInt(IValue::Int(n2)),
        ) => IType::SingletonInt(IValue::Int(n1 * n2)),

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
        Expr::BinOp {
            op: BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NotEq,
            lhs,
            ..
        } => {
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
        Expr::BinOp {
            op: BinOp::Lt,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::Gte,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp {
            op: BinOp::Lte,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::Gt,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp {
            op: BinOp::Gt,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::Lte,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp {
            op: BinOp::Gte,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::Lt,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp {
            op: BinOp::Eq,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::NotEq,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        Expr::BinOp {
            op: BinOp::NotEq,
            lhs,
            rhs,
        } => Expr::BinOp {
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
    use crate::frontend::typechecker::{TypeError, check_provable};

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

/// Rename the bound variable in a proposition
pub fn rename_prop_var<'src>(
    prop: &IProposition<'src>,
    old_var: &str,
    new_var: &str,
) -> IProposition<'src> {
    let renamed_predicate = rename_expr_var(&prop.predicate.0, old_var, new_var);
    IProposition {
        var: new_var.to_string(),
        predicate: Arc::new((renamed_predicate, prop.predicate.1)),
    }
}

/// Rename a variable in an expression
pub fn rename_expr_var<'src>(expr: &Expr<'src>, old: &str, new: &str) -> Expr<'src> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Variable(name) => {
            if *name == old {
                // Convert to owned string for Variable
                // Note: This requires Variable to accept owned strings
                // For now, we leak the string (acceptable for type checking)
                let leaked: &'src str = Box::leak(new.to_string().into_boxed_str());
                Expr::Variable(leaked)
            } else {
                Expr::Variable(name)
            }
        }
        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((rename_expr_var(&lhs.0, old, new), lhs.1)),
            rhs: Box::new((rename_expr_var(&rhs.0, old, new), rhs.1)),
        },
        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((rename_expr_var(&cond.0, old, new), cond.1)),
        },
        Expr::Call { func_name, args } => Expr::Call {
            func_name,
            args: (
                args.0
                    .iter()
                    .map(|arg| (rename_expr_var(&arg.0, old, new), arg.1))
                    .collect(),
                args.1,
            ),
        },
        Expr::Index { base, index } => Expr::Index {
            base: Box::new((rename_expr_var(&base.0, old, new), base.1)),
            index: Box::new((rename_expr_var(&index.0, old, new), index.1)),
        },
        Expr::ArrayInit { value, length } => Expr::ArrayInit {
            value: Box::new((rename_expr_var(&value.0, old, new), value.1)),
            length: Box::new((rename_expr_var(&length.0, old, new), length.1)),
        },
        Expr::If {
            cond,
            then_block,
            else_block,
        } => Expr::If {
            cond: Box::new((rename_expr_var(&cond.0, old, new), cond.1)),
            then_block: then_block
                .iter()
                .map(|stmt| (rename_stmt_var(&stmt.0, old, new), stmt.1))
                .collect(),
            else_block: else_block.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|stmt| (rename_stmt_var(&stmt.0, old, new), stmt.1))
                    .collect()
            }),
        },
    }
}

/// Rename a variable in a statement (helper for rename_expr_var)
fn rename_stmt_var<'src>(
    stmt: &crate::common::ast::Stmt<'src>,
    old: &str,
    new: &str,
) -> crate::common::ast::Stmt<'src> {
    use crate::common::ast::Stmt;

    match stmt {
        Stmt::Let {
            is_mut,
            name,
            ty,
            value,
        } => Stmt::Let {
            is_mut: *is_mut,
            name,
            ty: ty.clone(),
            value: (rename_expr_var(&value.0, old, new), value.1),
        },
        Stmt::Assignment { lhs, rhs } => Stmt::Assignment {
            lhs: (rename_expr_var(&lhs.0, old, new), lhs.1),
            rhs: (rename_expr_var(&rhs.0, old, new), rhs.1),
        },
        Stmt::Return { expr } => Stmt::Return {
            expr: Box::new((rename_expr_var(&expr.0, old, new), expr.1)),
        },
        Stmt::Expr(spanned_expr) => {
            Stmt::Expr((rename_expr_var(&spanned_expr.0, old, new), spanned_expr.1))
        }
        Stmt::For {
            var,
            start,
            end,
            invariant,
            body,
        } => Stmt::For {
            var,
            start: Box::new((rename_expr_var(&start.0, old, new), start.1)),
            end: Box::new((rename_expr_var(&end.0, old, new), end.1)),
            invariant: invariant
                .as_ref()
                .map(|inv| (rename_expr_var(&inv.0, old, new), inv.1)),
            body: body
                .iter()
                .map(|s| (rename_stmt_var(&s.0, old, new), s.1))
                .collect(),
        },
    }
}
