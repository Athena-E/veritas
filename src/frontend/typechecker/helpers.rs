// Helper functions for type checking
// constant folding, proposition extraction, context joining, SMT synthesis

use crate::common::ast::{BinOp, Block, Expr, Literal, UnaryOp};
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
pub fn build_equality_refinement<'src>(
    expr: &Expr<'src>,
    span: Span,
    base: IType<'src>,
) -> IType<'src> {
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
        base: Arc::new(base),
        prop: IProposition {
            var: bound_var,
            predicate: Arc::new((eq_predicate, span)),
        },
    }
}

/// Constant folding (join-op from formal semantics)
/// Attempts to compute the result type of binary operations on singleton types
pub fn join_op<'src>(op: BinOp, ty1: &IType<'src>, ty2: &IType<'src>) -> IType<'src> {
    // Try constant folding when both operands are compile-time singleton ints.
    // Uses checked arithmetic to avoid silently wrapping at compile time.
    if let (IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) =
        (ty1, ty2)
    {
        if let Some(folded) = checked_fold(op, *n1, *n2) {
            return IType::SingletonInt(IValue::Int(folded));
        }
    }

    match op {
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Mod
        | BinOp::BitAnd
        | BinOp::BitOr
        | BinOp::BitXor
        | BinOp::Shl
        | BinOp::Shr => IType::Int,

        // Comparisons always produce bool
        _ => IType::Bool,
    }
}

/// Attempt to constant-fold a binary arithmetic op on two values.
/// Computes the mathematical result in i128, then checks it fits within
/// the i128 range (which subsumes i64 and u64 ranges).
///
/// For type-specific overflow checking (i64 or u64 bounds), use
/// `checked_fold_in_range` instead.
///
/// Returns `None` if the op overflows i128, is undefined (shift count
/// out of range, div by zero), or is not foldable.
pub fn checked_fold(op: BinOp, n1: i128, n2: i128) -> Option<i128> {
    match op {
        BinOp::Add => n1.checked_add(n2),
        BinOp::Sub => n1.checked_sub(n2),
        BinOp::Mul => n1.checked_mul(n2),
        BinOp::Div if n2 != 0 => n1.checked_div(n2),
        BinOp::Mod if n2 != 0 => n1.checked_rem(n2),
        BinOp::Div | BinOp::Mod => None,
        BinOp::BitAnd => Some(n1 & n2),
        BinOp::BitOr => Some(n1 | n2),
        BinOp::BitXor => Some(n1 ^ n2),
        BinOp::Shl => u32::try_from(n2).ok().and_then(|k| n1.checked_shl(k)),
        BinOp::Shr => u32::try_from(n2).ok().and_then(|k| n1.checked_shr(k)),
        _ => None,
    }
}

/// Constant-fold with a range check: compute the result in i128, then
/// verify it lies within `[lo, hi]`. Returns `None` if the result is
/// out of the specified range or if `checked_fold` itself fails.
///
/// Use `(i64::MIN as i128, i64::MAX as i128)` for i64-typed folding,
/// `(0, u64::MAX as i128)` for u64-typed folding.
pub fn checked_fold_in_range(op: BinOp, n1: i128, n2: i128, lo: i128, hi: i128) -> Option<i128> {
    let result = checked_fold(op, n1, n2)?;
    if result >= lo && result <= hi {
        Some(result)
    } else {
        None
    }
}

/// Phase 2 entry point: when the typechecker detects i64-mode arithmetic
/// (or has `check_overflow` enabled globally), this helper rejects any
/// compile-time-known arithmetic that would overflow the given bounds.
///
/// `range_lo` and `range_hi` specify the valid result range:
///  - For i64: `(i64::MIN as i128, i64::MAX as i128)`
///  - For u64 (future): `(0, u64::MAX as i128)`
///  - For --check-overflow on int: same as i64
///
/// If both operands are singleton ints and the result exceeds the range,
/// this raises `IntegerOverflow`.
pub fn check_const_fold_overflow<'src>(
    op: BinOp,
    ty1: &IType<'src>,
    ty2: &IType<'src>,
    range_lo: i128,
    range_hi: i128,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::TypeError;

    // Only flag arith ops that can overflow — bitwise and/or/xor never do.
    let can_overflow = matches!(
        op,
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::Shl | BinOp::Shr
    );
    if !can_overflow {
        return Ok(());
    }
    if let (IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) =
        (ty1, ty2)
    {
        // Divide-by-zero is a separate error; don't mask it with IntegerOverflow.
        if matches!(op, BinOp::Div | BinOp::Mod) && *n2 == 0 {
            return Ok(());
        }
        if checked_fold_in_range(op, *n1, *n2, range_lo, range_hi).is_none() {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Shl => "<<",
                BinOp::Shr => ">>",
                _ => unreachable!(),
            };
            return Err(TypeError::IntegerOverflow {
                op: op_str.to_string(),
                span,
            });
        }
    }
    Ok(())
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

        // !(P ==> Q) becomes P && !Q
        Expr::BinOp {
            op: BinOp::Implies,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::And,
            lhs: lhs.clone(),
            rhs: Box::new((negate_expr(&rhs.0), rhs.1)),
        },

        // !forall{P} → exists{!P}
        Expr::Forall {
            var,
            start,
            end,
            body,
        } => Expr::Exists {
            var,
            start: start.clone(),
            end: end.clone(),
            body: Box::new((negate_expr(&body.0), body.1)),
        },

        // !exists{P} → forall{!P}
        Expr::Exists {
            var,
            start,
            end,
            body,
        } => Expr::Forall {
            var,
            start: start.clone(),
            end: end.clone(),
            body: Box::new((negate_expr(&body.0), body.1)),
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

/// Check that a binary arithmetic operation cannot overflow 64-bit signed range.
///
/// Builds the proposition `INT_MIN <= lhs op rhs <= INT_MAX` and asks the SMT oracle
/// to prove it under the current typing context. If the oracle cannot discharge it,
/// returns `TypeError::IntegerOverflow`.
///
/// Phase 1: this helper exists but is not yet called from `synth_expr`. Wiring happens
/// in Phase 3 behind the `--check-overflow` flag.
#[allow(dead_code)]
pub fn check_no_overflow<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    op: BinOp,
    lhs_expr: &Expr<'src>,
    rhs_expr: &Expr<'src>,
    range_lo: i128,
    range_hi: i128,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::{TypeError, check_provable};

    // Bitwise ops (except shifts) and comparison/logical ops cannot overflow.
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Shl | BinOp::Shr | BinOp::Div | BinOp::Mod => {}
        _ => return Ok(()),
    }

    let dummy_span = SimpleSpan::new(0, 0);
    let op_str = match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Shl => "<<",
        BinOp::Shr => ">>",
        BinOp::Div => "/",
        BinOp::Mod => "%",
        _ => unreachable!(),
    };

    // result = lhs op rhs
    let result_expr = Expr::BinOp {
        op,
        lhs: Box::new((lhs_expr.clone(), dummy_span)),
        rhs: Box::new((rhs_expr.clone(), dummy_span)),
    };

    // range_lo <= result
    let lower = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((Expr::Literal(Literal::Int(range_lo)), dummy_span)),
        rhs: Box::new((result_expr.clone(), dummy_span)),
    };
    // result <= range_hi
    let upper = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((result_expr, dummy_span)),
        rhs: Box::new((Expr::Literal(Literal::Int(range_hi)), dummy_span)),
    };
    // (range_lo <= result) && (result <= range_hi)
    let in_range = Expr::BinOp {
        op: BinOp::And,
        lhs: Box::new((lower, dummy_span)),
        rhs: Box::new((upper, dummy_span)),
    };

    let prop = IProposition {
        var: "_ov".to_string(),
        predicate: Arc::new((in_range, dummy_span)),
    };

    if !check_provable(ctx, &prop) {
        return Err(TypeError::IntegerOverflow {
            op: op_str.to_string(),
            span,
        });
    }

    // For shifts, additionally prove the count is in [0, 64).
    // x86 masks the count to 6 bits, so `x << 65` silently becomes `x << 1`
    // — a correctness bug the type system should catch.
    if matches!(op, BinOp::Shl | BinOp::Shr) {
        // rhs >= 0
        let count_lower = Expr::BinOp {
            op: BinOp::Gte,
            lhs: Box::new((rhs_expr.clone(), dummy_span)),
            rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy_span)),
        };
        // rhs < 64
        let count_upper = Expr::BinOp {
            op: BinOp::Lt,
            lhs: Box::new((rhs_expr.clone(), dummy_span)),
            rhs: Box::new((Expr::Literal(Literal::Int(64)), dummy_span)),
        };
        let count_in_range = Expr::BinOp {
            op: BinOp::And,
            lhs: Box::new((count_lower, dummy_span)),
            rhs: Box::new((count_upper, dummy_span)),
        };
        let prop = IProposition {
            var: "_shift_count".to_string(),
            predicate: Arc::new((count_in_range, dummy_span)),
        };
        if !check_provable(ctx, &prop) {
            return Err(TypeError::IntegerOverflow {
                op: op_str.to_string(),
                span,
            });
        }
    }

    // For `/` and `%`, additionally rule out range_lo / -1 which also overflows
    // (only relevant for signed types where range_lo is negative).
    if matches!(op, BinOp::Div | BinOp::Mod) && range_lo < 0 {
        // NOT (lhs == range_lo && rhs == -1)
        let lhs_is_min = Expr::BinOp {
            op: BinOp::Eq,
            lhs: Box::new((lhs_expr.clone(), dummy_span)),
            rhs: Box::new((Expr::Literal(Literal::Int(range_lo)), dummy_span)),
        };
        let rhs_is_neg_one = Expr::BinOp {
            op: BinOp::Eq,
            lhs: Box::new((rhs_expr.clone(), dummy_span)),
            rhs: Box::new((Expr::Literal(Literal::Int(-1)), dummy_span)),
        };
        let both = Expr::BinOp {
            op: BinOp::And,
            lhs: Box::new((lhs_is_min, dummy_span)),
            rhs: Box::new((rhs_is_neg_one, dummy_span)),
        };
        let not_both = Expr::UnaryOp {
            op: UnaryOp::Not,
            cond: Box::new((both, dummy_span)),
        };
        let prop = IProposition {
            var: "_ov_div".to_string(),
            predicate: Arc::new((not_both, dummy_span)),
        };
        if !check_provable(ctx, &prop) {
            return Err(TypeError::IntegerOverflow {
                op: op_str.to_string(),
                span,
            });
        }
    }

    Ok(())
}

/// Check that unary negation cannot overflow (operand must not be `INT_MIN`).
///
/// Phase 1: not yet called from `synth_expr`. Wiring happens in Phase 3.
#[allow(dead_code)]
pub fn check_no_negation_overflow<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    operand_expr: &Expr<'src>,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::{TypeError, check_provable};

    let dummy_span = SimpleSpan::new(0, 0);
    let not_min = Expr::BinOp {
        op: BinOp::NotEq,
        lhs: Box::new((operand_expr.clone(), dummy_span)),
        rhs: Box::new((Expr::Literal(Literal::Int(i64::MIN as i128)), dummy_span)),
    };
    let prop = IProposition {
        var: "_neg_ov".to_string(),
        predicate: Arc::new((not_min, dummy_span)),
    };

    if !check_provable(ctx, &prop) {
        return Err(TypeError::NegationOverflow { span });
    }
    Ok(())
}

/// Check that the divisor in a division expression is provably non-zero
pub fn check_divisor_nonzero<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    divisor_expr: &Expr<'src>,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::{TypeError, check_provable};

    let dummy_span = SimpleSpan::new(0, 0);

    let nonzero = IProposition {
        var: "div".to_string(),
        predicate: Arc::new((
            Expr::BinOp {
                op: BinOp::NotEq,
                lhs: Box::new((divisor_expr.clone(), dummy_span)),
                rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy_span)),
            },
            dummy_span,
        )),
    };

    if !check_provable(ctx, &nonzero) {
        return Err(TypeError::DivisionByZero { span });
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
        Expr::Forall {
            var,
            start,
            end,
            body,
        } => {
            if *var == old {
                // Bound variable shadows — don't rename inside body
                Expr::Forall {
                    var,
                    start: Box::new((rename_expr_var(&start.0, old, new), start.1)),
                    end: Box::new((rename_expr_var(&end.0, old, new), end.1)),
                    body: body.clone(),
                }
            } else {
                Expr::Forall {
                    var,
                    start: Box::new((rename_expr_var(&start.0, old, new), start.1)),
                    end: Box::new((rename_expr_var(&end.0, old, new), end.1)),
                    body: Box::new((rename_expr_var(&body.0, old, new), body.1)),
                }
            }
        }
        Expr::Exists {
            var,
            start,
            end,
            body,
        } => {
            if *var == old {
                Expr::Exists {
                    var,
                    start: Box::new((rename_expr_var(&start.0, old, new), start.1)),
                    end: Box::new((rename_expr_var(&end.0, old, new), end.1)),
                    body: body.clone(),
                }
            } else {
                Expr::Exists {
                    var,
                    start: Box::new((rename_expr_var(&start.0, old, new), start.1)),
                    end: Box::new((rename_expr_var(&end.0, old, new), end.1)),
                    body: Box::new((rename_expr_var(&body.0, old, new), body.1)),
                }
            }
        }
        Expr::If {
            cond,
            then_block,
            else_block,
        } => Expr::If {
            cond: Box::new((rename_expr_var(&cond.0, old, new), cond.1)),
            then_block: Block {
                statements: then_block
                    .statements
                    .iter()
                    .map(|stmt| (rename_stmt_var(&stmt.0, old, new), stmt.1))
                    .collect(),
                trailing_expr: then_block
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((rename_expr_var(&e.0, old, new), e.1))),
            },
            else_block: else_block.as_ref().map(|block| Block {
                statements: block
                    .statements
                    .iter()
                    .map(|stmt| (rename_stmt_var(&stmt.0, old, new), stmt.1))
                    .collect(),
                trailing_expr: block
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((rename_expr_var(&e.0, old, new), e.1))),
            }),
        },
    }
}

/// Substitute an expression for all occurrences of a variable in an expression.
/// Unlike `rename_expr_var` which replaces a variable with another variable name,
/// this replaces a variable with an arbitrary expression (e.g., `arr[i]`).
/// Respects bound variable shadowing in quantifiers.
pub fn substitute_expr_for_var<'src>(
    expr: &Expr<'src>,
    var: &str,
    replacement: &Expr<'src>,
) -> Expr<'src> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Variable(name) => {
            if *name == var {
                replacement.clone()
            } else {
                Expr::Variable(name)
            }
        }
        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((substitute_expr_for_var(&lhs.0, var, replacement), lhs.1)),
            rhs: Box::new((substitute_expr_for_var(&rhs.0, var, replacement), rhs.1)),
        },
        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((substitute_expr_for_var(&cond.0, var, replacement), cond.1)),
        },
        Expr::Index { base, index } => Expr::Index {
            base: Box::new((substitute_expr_for_var(&base.0, var, replacement), base.1)),
            index: Box::new((substitute_expr_for_var(&index.0, var, replacement), index.1)),
        },
        Expr::Call { func_name, args } => Expr::Call {
            func_name,
            args: (
                args.0
                    .iter()
                    .map(|arg| (substitute_expr_for_var(&arg.0, var, replacement), arg.1))
                    .collect(),
                args.1,
            ),
        },
        Expr::ArrayInit { value, length } => Expr::ArrayInit {
            value: Box::new((substitute_expr_for_var(&value.0, var, replacement), value.1)),
            length: Box::new((
                substitute_expr_for_var(&length.0, var, replacement),
                length.1,
            )),
        },
        Expr::Forall {
            var: bound,
            start,
            end,
            body,
        } => {
            if *bound == var {
                // Bound variable shadows — don't substitute inside body
                Expr::Forall {
                    var: bound,
                    start: Box::new((substitute_expr_for_var(&start.0, var, replacement), start.1)),
                    end: Box::new((substitute_expr_for_var(&end.0, var, replacement), end.1)),
                    body: body.clone(),
                }
            } else {
                Expr::Forall {
                    var: bound,
                    start: Box::new((substitute_expr_for_var(&start.0, var, replacement), start.1)),
                    end: Box::new((substitute_expr_for_var(&end.0, var, replacement), end.1)),
                    body: Box::new((substitute_expr_for_var(&body.0, var, replacement), body.1)),
                }
            }
        }
        Expr::Exists {
            var: bound,
            start,
            end,
            body,
        } => {
            if *bound == var {
                Expr::Exists {
                    var: bound,
                    start: Box::new((substitute_expr_for_var(&start.0, var, replacement), start.1)),
                    end: Box::new((substitute_expr_for_var(&end.0, var, replacement), end.1)),
                    body: body.clone(),
                }
            } else {
                Expr::Exists {
                    var: bound,
                    start: Box::new((substitute_expr_for_var(&start.0, var, replacement), start.1)),
                    end: Box::new((substitute_expr_for_var(&end.0, var, replacement), end.1)),
                    body: Box::new((substitute_expr_for_var(&body.0, var, replacement), body.1)),
                }
            }
        }
        // For specification-level expressions, If/For shouldn't appear
        other => other.clone(),
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
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|s| (rename_stmt_var(&s.0, old, new), s.1))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((rename_expr_var(&e.0, old, new), e.1))),
            },
        },
        Stmt::While {
            condition,
            invariant,
            body,
        } => Stmt::While {
            condition: Box::new((rename_expr_var(&condition.0, old, new), condition.1)),
            invariant: invariant
                .as_ref()
                .map(|inv| (rename_expr_var(&inv.0, old, new), inv.1)),
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|s| (rename_stmt_var(&s.0, old, new), s.1))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((rename_expr_var(&e.0, old, new), e.1))),
            },
        },
    }
}
