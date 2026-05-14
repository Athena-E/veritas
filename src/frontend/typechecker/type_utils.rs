use crate::common::ast::Type as AstType;
use crate::common::span::Spanned;
use crate::common::types::{IProposition, IType, IValue};
use crate::frontend::typechecker::substitution::substitute_var_with_literal;
use crate::frontend::typechecker::{TypeError, TypingContext};
use std::sync::Arc;

/// Convert AST type to semantic internal type
pub(super) fn ast_type_to_itype<'src>(
    ty: &Spanned<AstType<'src>>,
) -> Result<IType<'src>, TypeError<'src>> {
    match &ty.0 {
        AstType::Unit => Ok(IType::Unit),
        AstType::Int => Ok(IType::Int),
        AstType::I64 => Ok(IType::I64),
        AstType::U64 => Ok(IType::U64),
        AstType::Bool => Ok(IType::Bool),

        AstType::Array { element_type, size } => {
            let elem_ty = ast_type_to_itype(element_type)?;

            // Evaluate size to IValue
            let size_val = eval_array_size(size)?;

            Ok(IType::Array {
                element_type: Arc::new(elem_ty),
                size: size_val,
            })
        }

        AstType::Ref(inner) => {
            let inner_ty = ast_type_to_itype(inner)?;
            Ok(IType::Ref(Arc::new(inner_ty)))
        }

        AstType::RefMut(inner) => {
            let inner_ty = ast_type_to_itype(inner)?;
            Ok(IType::RefMut(Arc::new(inner_ty)))
        }

        AstType::RefinedInt { var, predicate } => {
            // RefinedInt has base type of Int implicitly
            let prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new(*predicate.clone()),
            };

            Ok(IType::RefinedInt {
                base: Arc::new(IType::Int),
                prop,
            })
        }

        AstType::RefinedI64 { var, predicate } => {
            let prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new(*predicate.clone()),
            };

            Ok(IType::RefinedInt {
                base: Arc::new(IType::I64),
                prop,
            })
        }

        AstType::RefinedU64 { var, predicate } => {
            let prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new(*predicate.clone()),
            };

            Ok(IType::RefinedInt {
                base: Arc::new(IType::U64),
                prop,
            })
        }

        AstType::SingletonInt(expr) => {
            // Evaluate the expression to get the singleton value
            let value = eval_array_size(expr)?;
            Ok(IType::SingletonInt(value))
        }
    }
}

/// Add implicit `[INT_MIN, INT_MAX]` range propositions for an i64 binding.
///
/// Z3's Int sort is unbounded. When the user writes `x: i64`, we axiomatise
/// that the value is representable as a 64-bit signed integer so that
/// arithmetic overflow obligations like `INT_MIN <= x + y <= INT_MAX` can
/// be discharged.
pub(super) fn add_i64_range_props<'src>(
    ctx: TypingContext<'src>,
    var_name: &'src str,
) -> TypingContext<'src> {
    use crate::common::ast::{BinOp, Expr, Literal};
    use chumsky::prelude::SimpleSpan;

    let dummy = SimpleSpan::new(0, 0);
    let var = Expr::Variable(var_name);

    // var >= INT_MIN
    let lower = Expr::BinOp {
        op: BinOp::Gte,
        lhs: Box::new((var.clone(), dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(i64::MIN as i128)), dummy)),
    };
    let lower_prop = IProposition {
        var: var_name.to_string(),
        predicate: Arc::new((lower, dummy)),
    };

    // var <= INT_MAX
    let upper = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((var, dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(i64::MAX as i128)), dummy)),
    };
    let upper_prop = IProposition {
        var: var_name.to_string(),
        predicate: Arc::new((upper, dummy)),
    };

    ctx.with_proposition(lower_prop)
        .with_proposition(upper_prop)
}

/// Add implicit `[0, U64_MAX]` range propositions for a u64 binding.
///
/// Same idea as `add_i64_range_props` but for unsigned 64-bit integers:
/// the value is in `[0, 18446744073709551615]`.
pub(super) fn add_u64_range_props<'src>(
    ctx: TypingContext<'src>,
    var_name: &'src str,
) -> TypingContext<'src> {
    use crate::common::ast::{BinOp, Expr, Literal};
    use chumsky::prelude::SimpleSpan;

    let dummy = SimpleSpan::new(0, 0);
    let var = Expr::Variable(var_name);

    // var >= 0
    let lower = Expr::BinOp {
        op: BinOp::Gte,
        lhs: Box::new((var.clone(), dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy)),
    };
    let lower_prop = IProposition {
        var: var_name.to_string(),
        predicate: Arc::new((lower, dummy)),
    };

    // var <= U64_MAX
    let upper = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((var, dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(u64::MAX as i128)), dummy)),
    };
    let upper_prop = IProposition {
        var: var_name.to_string(),
        predicate: Arc::new((upper, dummy)),
    };

    ctx.with_proposition(lower_prop)
        .with_proposition(upper_prop)
}

/// Add implicit axioms for symbolic array lengths:
///   len >= 0       (arrays cannot have negative length)
///   len <= INT_MAX (array length is representable as a machine integer)
///
/// These are load-bearing for proving loop counter arithmetic on i64 indices.
pub(super) fn add_array_length_axioms<'src>(
    ctx: TypingContext<'src>,
    size_var: &str,
) -> TypingContext<'src> {
    use crate::common::ast::{BinOp, Expr, Literal};
    use chumsky::prelude::SimpleSpan;

    let dummy = SimpleSpan::new(0, 0);
    // We leak the string to get a &'src str for use in Expr::Variable.
    let var_name: &'src str = Box::leak(size_var.to_string().into_boxed_str());
    let var = Expr::Variable(var_name);

    // len >= 0
    let nonneg = Expr::BinOp {
        op: BinOp::Gte,
        lhs: Box::new((var.clone(), dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(0)), dummy)),
    };
    let nonneg_prop = IProposition {
        var: size_var.to_string(),
        predicate: Arc::new((nonneg, dummy)),
    };

    // len <= INT_MAX
    let bounded = Expr::BinOp {
        op: BinOp::Lte,
        lhs: Box::new((var, dummy)),
        rhs: Box::new((Expr::Literal(Literal::Int(i64::MAX as i128)), dummy)),
    };
    let bounded_prop = IProposition {
        var: size_var.to_string(),
        predicate: Arc::new((bounded, dummy)),
    };

    ctx.with_proposition(nonneg_prop)
        .with_proposition(bounded_prop)
}

/// Evaluate array size expression to IValue
fn eval_array_size<'src>(
    expr: &Spanned<crate::common::ast::Expr<'src>>,
) -> Result<IValue, TypeError<'src>> {
    use crate::common::ast::{Expr, Literal};

    match &expr.0 {
        Expr::Literal(Literal::Int(n)) => Ok(IValue::Int(*n)),
        Expr::Variable(name) => Ok(IValue::Symbolic(name.to_string())),
        _ => Err(TypeError::NotAConstant { span: expr.1 }),
    }
}

/// Substitute "result" in postcondition with the return value.
/// When the return expression is a bare variable, rename "result" to that variable
/// so existing propositions in the context (e.g. pointwise array facts) are visible.
/// When the return type is a singleton int, substitute the literal value directly.
pub(super) fn substitute_result_in_postcond<'src>(
    postcond: &IProposition<'src>,
    return_ty: &IType<'src>,
    return_expr: &crate::common::ast::Expr<'src>,
) -> IProposition<'src> {
    use crate::frontend::typechecker::helpers::rename_expr_var;

    match return_expr {
        crate::common::ast::Expr::Variable(var_name) => {
            let renamed = rename_expr_var(&postcond.predicate.0, "result", var_name);
            IProposition {
                var: var_name.to_string(),
                predicate: Arc::new((renamed, postcond.predicate.1)),
            }
        }
        _ => match return_ty {
            IType::SingletonInt(IValue::Int(n)) => {
                let subst_expr = substitute_var_with_literal(&postcond.predicate.0, "result", *n);
                IProposition {
                    var: "_".to_string(),
                    predicate: Arc::new((subst_expr, chumsky::span::SimpleSpan::new(0, 0))),
                }
            }
            _ => postcond.clone(),
        },
    }
}

/// Check whether an array type has a symbolic size in any inner (non-outermost)
/// dimension. Symbolic sizes are only supported in the outermost dimension
/// because the backend needs a concrete stride to flatten nested access.
pub(super) fn has_symbolic_inner_dim<'src>(ty: &IType<'src>) -> bool {
    match ty {
        IType::Array { element_type, .. } => inner_has_symbolic(element_type),
        _ => false,
    }
}

fn inner_has_symbolic<'src>(ty: &IType<'src>) -> bool {
    match ty {
        IType::Array { element_type, size } => {
            matches!(size, IValue::Symbolic(_)) || inner_has_symbolic(element_type)
        }
        _ => false,
    }
}

pub(super) fn contains_array_type<'src>(ty: &IType<'src>) -> bool {
    match ty {
        IType::Array { .. } => true,
        IType::RefinedInt { base, .. } => contains_array_type(base),
        _ => false,
    }
}

pub(super) fn array_contains_reference_type<'src>(ty: &IType<'src>) -> bool {
    match ty {
        IType::Array { element_type, .. } => {
            matches!(element_type.as_ref(), IType::Ref(_) | IType::RefMut(_))
                || array_contains_reference_type(element_type)
        }
        IType::RefinedInt { base, .. } => array_contains_reference_type(base),
        _ => false,
    }
}

pub(super) fn source_type_contains_array<'src>(ty: &AstType<'src>) -> bool {
    match ty {
        AstType::Array { .. } => true,
        AstType::Ref(inner) | AstType::RefMut(inner) => source_type_contains_array(&inner.0),
        AstType::Unit
        | AstType::Int
        | AstType::I64
        | AstType::U64
        | AstType::Bool
        | AstType::SingletonInt(_)
        | AstType::RefinedInt { .. }
        | AstType::RefinedI64 { .. }
        | AstType::RefinedU64 { .. } => false,
    }
}
