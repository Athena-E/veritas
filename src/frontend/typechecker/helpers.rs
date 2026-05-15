use crate::common::ast::{BinOp, Block, Expr, Literal, UnaryOp};
use crate::common::span::Span;
use crate::common::types::{IProposition, IType, IValue};
use chumsky::prelude::SimpleSpan;
use std::cell::Cell;
use std::sync::Arc;

thread_local! {
    static FRESH_VAR_COUNTER: Cell<u64> = const { Cell::new(0) };
}

pub fn fresh_var_name() -> String {
    let id = FRESH_VAR_COUNTER.with(|counter| {
        let id = counter.get();
        counter.set(id + 1);
        id
    });
    format!("_synth_{}", id)
}

#[allow(dead_code)]
pub fn reset_fresh_var_counter() {
    FRESH_VAR_COUNTER.with(|counter| counter.set(0));
}

pub fn build_equality_refinement<'src>(
    expr: &Expr<'src>,
    span: Span,
    base: IType<'src>,
) -> IType<'src> {
    let bound_var = fresh_var_name();
    let bound_var_leaked: &'src str = Box::leak(bound_var.clone().into_boxed_str());

    let v_expr = Expr::Variable(bound_var_leaked);

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

pub fn join_op<'src>(op: BinOp, ty1: &IType<'src>, ty2: &IType<'src>) -> IType<'src> {
    if let (IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) = (ty1, ty2)
        && let Some(folded) = checked_fold(op, *n1, *n2)
    {
        return IType::SingletonInt(IValue::Int(folded));
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

        _ => IType::Bool,
    }
}

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

pub fn checked_fold_in_range(op: BinOp, n1: i128, n2: i128, lo: i128, hi: i128) -> Option<i128> {
    let result = checked_fold(op, n1, n2)?;
    if result >= lo && result <= hi {
        Some(result)
    } else {
        None
    }
}

pub fn check_const_fold_overflow<'src>(
    op: BinOp,
    ty1: &IType<'src>,
    ty2: &IType<'src>,
    range_lo: i128,
    range_hi: i128,
    span: Span,
) -> Result<(), crate::frontend::typechecker::TypeError<'src>> {
    use crate::frontend::typechecker::TypeError;

    let can_overflow = matches!(
        op,
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::Shl | BinOp::Shr
    );
    if !can_overflow {
        return Ok(());
    }
    if let (IType::SingletonInt(IValue::Int(n1)), IType::SingletonInt(IValue::Int(n2))) = (ty1, ty2)
    {
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

pub fn extract_proposition<'src>(expr: &Expr<'src>) -> Option<IProposition<'src>> {
    match expr {
        Expr::BinOp {
            op: BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NotEq,
            lhs,
            ..
        } => {
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

pub fn negate_proposition<'src>(prop: &IProposition<'src>) -> IProposition<'src> {
    let negated_expr = negate_expr(&prop.predicate.0);

    IProposition {
        var: prop.var.clone(),
        predicate: Arc::new((negated_expr, prop.predicate.1)),
    }
}

fn negate_expr<'src>(expr: &Expr<'src>) -> Expr<'src> {
    match expr {
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

        Expr::BinOp {
            op: BinOp::Implies,
            lhs,
            rhs,
        } => Expr::BinOp {
            op: BinOp::And,
            lhs: lhs.clone(),
            rhs: Box::new((negate_expr(&rhs.0), rhs.1)),
        },

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

    if !check_provable(ctx, &lower_bound) {
        return Err(TypeError::InvalidArrayAccess {
            array_type: array_type.clone(),
            index_expr: format!("{}", index_ty),
            reason: "Index may be negative".to_string(),
            span,
        });
    }

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

    match op {
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Shl
        | BinOp::Shr
        | BinOp::Div
        | BinOp::Mod => {}
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

    let result_expr = Expr::BinOp {
        op,
        lhs: Box::new((lhs_expr.clone(), dummy_span)),
        rhs: Box::new((rhs_expr.clone(), dummy_span)),
    };

    let lower = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((Expr::Literal(Literal::Int(range_lo)), dummy_span)),
        rhs: Box::new((result_expr.clone(), dummy_span)),
    };
    let upper = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((result_expr, dummy_span)),
        rhs: Box::new((Expr::Literal(Literal::Int(range_hi)), dummy_span)),
    };
    let in_range = Expr::BinOp {
        op: BinOp::And,
        lhs: Box::new((lower, dummy_span)),
        rhs: Box::new((upper, dummy_span)),
    };

    let prop = IProposition {
        var: "_ov".to_string(),
        predicate: Arc::new((in_range, dummy_span)),
    };

    if !interval_proves_no_overflow(ctx, op, lhs_expr, rhs_expr, range_lo, range_hi)
        && !check_provable(ctx, &prop)
    {
        return Err(TypeError::IntegerOverflow {
            op: op_str.to_string(),
            span,
        });
    }

    if matches!(op, BinOp::Shl | BinOp::Shr) {
        let count_lower = Expr::BinOp {
            op: BinOp::Gte,
            lhs: Box::new((rhs_expr.clone(), dummy_span)),
            rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy_span)),
        };
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

    if matches!(op, BinOp::Div | BinOp::Mod) && range_lo < 0 {
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

fn interval_proves_no_overflow<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    op: BinOp,
    lhs_expr: &Expr<'src>,
    rhs_expr: &Expr<'src>,
    range_lo: i128,
    range_hi: i128,
) -> bool {
    let Some((lhs_lo, lhs_hi)) = expr_interval(ctx, lhs_expr) else {
        return false;
    };
    let Some((rhs_lo, rhs_hi)) = expr_interval(ctx, rhs_expr) else {
        return false;
    };

    let result = match op {
        BinOp::Add => checked_add_interval(lhs_lo, lhs_hi, rhs_lo, rhs_hi),
        BinOp::Sub => checked_sub_interval(lhs_lo, lhs_hi, rhs_lo, rhs_hi),
        BinOp::Mul => mul_interval(lhs_lo, lhs_hi, rhs_lo, rhs_hi),
        _ => None,
    };

    let Some((result_lo, result_hi)) = result else {
        return false;
    };
    result_lo >= range_lo && result_hi <= range_hi
}

fn checked_add_interval(
    lhs_lo: i128,
    lhs_hi: i128,
    rhs_lo: i128,
    rhs_hi: i128,
) -> Option<(i128, i128)> {
    Some((lhs_lo.checked_add(rhs_lo)?, lhs_hi.checked_add(rhs_hi)?))
}

fn checked_sub_interval(
    lhs_lo: i128,
    lhs_hi: i128,
    rhs_lo: i128,
    rhs_hi: i128,
) -> Option<(i128, i128)> {
    Some((lhs_lo.checked_sub(rhs_hi)?, lhs_hi.checked_sub(rhs_lo)?))
}

fn mul_interval(lhs_lo: i128, lhs_hi: i128, rhs_lo: i128, rhs_hi: i128) -> Option<(i128, i128)> {
    let products = [
        lhs_lo.checked_mul(rhs_lo)?,
        lhs_lo.checked_mul(rhs_hi)?,
        lhs_hi.checked_mul(rhs_lo)?,
        lhs_hi.checked_mul(rhs_hi)?,
    ];
    let lo = *products.iter().min()?;
    let hi = *products.iter().max()?;
    Some((lo, hi))
}

fn expr_interval<'src>(
    ctx: &crate::frontend::typechecker::TypingContext<'src>,
    expr: &Expr<'src>,
) -> Option<(i128, i128)> {
    match expr {
        Expr::Literal(Literal::Int(n)) => Some((*n, *n)),
        Expr::Variable(name) => match ctx.lookup_var(name)? {
            crate::frontend::typechecker::VarBinding::Immutable(ty) => type_interval(&ty),
            crate::frontend::typechecker::VarBinding::Mutable(binding) => {
                type_interval(&binding.current_type)
            }
        },
        Expr::BinOp { op, lhs, rhs } => {
            let (lhs_lo, lhs_hi) = expr_interval(ctx, &lhs.0)?;
            let (rhs_lo, rhs_hi) = expr_interval(ctx, &rhs.0)?;
            match op {
                BinOp::Add => Some((lhs_lo.checked_add(rhs_lo)?, lhs_hi.checked_add(rhs_hi)?)),
                BinOp::Sub => Some((lhs_lo.checked_sub(rhs_hi)?, lhs_hi.checked_sub(rhs_lo)?)),
                BinOp::Mul => mul_interval(lhs_lo, lhs_hi, rhs_lo, rhs_hi),
                _ => None,
            }
        }
        Expr::UnaryOp {
            op: UnaryOp::Neg,
            cond,
        } => {
            let (lo, hi) = expr_interval(ctx, &cond.0)?;
            Some((hi.checked_neg()?, lo.checked_neg()?))
        }
        _ => None,
    }
}

fn type_interval(ty: &IType) -> Option<(i128, i128)> {
    match ty {
        IType::SingletonInt(IValue::Int(n)) => Some((*n, *n)),
        IType::I64 => Some((i64::MIN as i128, i64::MAX as i128)),
        IType::U64 => Some((0, u64::MAX as i128)),
        IType::RefinedInt { base, prop } => {
            let interval = type_interval(base)?;
            refine_interval_with_expr(interval, &prop.var, &prop.predicate.0)
        }
        IType::Master(inner) => type_interval(inner),
        _ => None,
    }
}

fn refine_interval_with_expr<'src>(
    interval: (i128, i128),
    var_name: &str,
    expr: &Expr<'src>,
) -> Option<(i128, i128)> {
    match expr {
        Expr::BinOp {
            op: BinOp::And,
            lhs,
            rhs,
        } => {
            let interval = refine_interval_with_expr(interval, var_name, &lhs.0)?;
            refine_interval_with_expr(interval, var_name, &rhs.0)
        }
        Expr::BinOp { op, lhs, rhs } => {
            refine_interval_with_bound(interval, var_name, *op, &lhs.0, &rhs.0).or(Some(interval))
        }
        _ => Some(interval),
    }
}

fn refine_interval_with_bound<'src>(
    (lo, hi): (i128, i128),
    var_name: &str,
    op: BinOp,
    lhs: &Expr<'src>,
    rhs: &Expr<'src>,
) -> Option<(i128, i128)> {
    match (lhs, rhs) {
        (Expr::Variable(name), Expr::Literal(Literal::Int(n))) if *name == var_name => {
            apply_upper_lower_bound((lo, hi), op, *n)
        }
        (Expr::Literal(Literal::Int(n)), Expr::Variable(name)) if *name == var_name => {
            apply_upper_lower_bound((lo, hi), reverse_cmp(op)?, *n)
        }
        _ => None,
    }
}

fn apply_upper_lower_bound((lo, hi): (i128, i128), op: BinOp, n: i128) -> Option<(i128, i128)> {
    match op {
        BinOp::Eq => Some((lo.max(n), hi.min(n))),
        BinOp::Lt => Some((lo, hi.min(n.checked_sub(1)?))),
        BinOp::Lte => Some((lo, hi.min(n))),
        BinOp::Gt => Some((lo.max(n.checked_add(1)?), hi)),
        BinOp::Gte => Some((lo.max(n), hi)),
        _ => None,
    }
}

fn reverse_cmp(op: BinOp) -> Option<BinOp> {
    match op {
        BinOp::Eq => Some(BinOp::Eq),
        BinOp::Lt => Some(BinOp::Gt),
        BinOp::Lte => Some(BinOp::Gte),
        BinOp::Gt => Some(BinOp::Lt),
        BinOp::Gte => Some(BinOp::Lte),
        _ => None,
    }
}

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

fn value_to_expr_from_ivalue<'src>(val: &'src IValue) -> Expr<'src> {
    match val {
        IValue::Int(n) => Expr::Literal(Literal::Int(*n)),
        IValue::Symbolic(s) => Expr::Variable(s.as_str()),
        IValue::Bool(b) => Expr::Literal(Literal::Bool(*b)),
    }
}

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

pub fn rename_expr_var<'src>(expr: &Expr<'src>, old: &str, new: &str) -> Expr<'src> {
    match expr {
        Expr::Error => Expr::Error,
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::Variable(name) => {
            if *name == old {
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
        Expr::Borrow { kind, expr } => Expr::Borrow {
            kind: *kind,
            expr: Box::new((rename_expr_var(&expr.0, old, new), expr.1)),
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
        Expr::Borrow { kind, expr } => Expr::Borrow {
            kind: *kind,
            expr: Box::new((substitute_expr_for_var(&expr.0, var, replacement), expr.1)),
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
        other => other.clone(),
    }
}

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
        Stmt::Region { body } => Stmt::Region {
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
