// Given an expression e, synthesize its type T and produce a typed expression e'

use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::span::Spanned;
use crate::common::tast::{TBlock, TExpr};
use crate::common::types::{IType, IValue};
use crate::frontend::typechecker::helpers::check_const_fold_overflow;
use crate::frontend::typechecker::{
    TypeError, TypingContext, VarBinding, build_equality_refinement, check_array_bounds_expr,
    check_divisor_nonzero, check_stmts, extract_proposition, is_subtype, join_op,
    negate_proposition,
};
use std::sync::Arc;

/// Synthesize the type of an expression
/// Returns a typed expression and its type, or a type error
pub fn synth_expr<'src>(
    ctx: &TypingContext<'src>,
    expr: &Spanned<Expr<'src>>,
) -> Result<(Spanned<TExpr<'src>>, IType<'src>), TypeError<'src>> {
    let span = expr.1;

    match &expr.0 {
        // INT-LIT: Integer literals have singleton types
        Expr::Literal(Literal::Int(n)) => {
            let ty = IType::SingletonInt(IValue::Int(*n));
            let texpr = TExpr::Literal {
                value: Literal::Int(*n),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // BOOL-LIT: Boolean literals have singleton types
        Expr::Literal(Literal::Bool(b)) => {
            let ty = IType::Bool;
            let texpr = TExpr::Literal {
                value: Literal::Bool(*b),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // VAR: Look up variable in context
        Expr::Variable(name) => {
            match ctx.lookup_var(name) {
                Some(VarBinding::Immutable(ty)) => {
                    let texpr = TExpr::Variable {
                        name: name.to_string(),
                        ty: ty.clone(),
                    };
                    Ok(((texpr, span), ty.clone()))
                }
                Some(VarBinding::Mutable(binding)) => {
                    // For mutable variables, use the current type
                    let ty = binding.current_type.clone();
                    let texpr = TExpr::Variable {
                        name: name.to_string(),
                        ty: ty.clone(),
                    };
                    Ok(((texpr, span), ty))
                }
                None => Err(TypeError::UndefinedVariable {
                    name: name.to_string(),
                    span,
                }),
            }
        }

        // BINOP-ARITH: Arithmetic operations with SMT synthesis
        Expr::BinOp {
            op:
                op @ (BinOp::Add
                | BinOp::Sub
                | BinOp::Mul
                | BinOp::Div
                | BinOp::Mod
                | BinOp::BitAnd
                | BinOp::BitOr
                | BinOp::BitXor
                | BinOp::Shl
                | BinOp::Shr),
            lhs,
            rhs,
        } => {
            let (tlhs, ty1) = synth_expr(ctx, lhs)?;
            let (trhs, ty2) = synth_expr(ctx, rhs)?;

            // Check both operands are subtypes of int
            if !is_subtype(ctx, &ty1, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty1,
                    span: lhs.1,
                });
            }
            if !is_subtype(ctx, &ty2, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty2,
                    span: rhs.1,
                });
            }

            // Determine arithmetic mode: i64 if both operands are subtypes of i64
            let lhs_i64 = is_subtype(ctx, &ty1, &IType::I64);
            let rhs_i64 = is_subtype(ctx, &ty2, &IType::I64);
            let i64_mode = lhs_i64 && rhs_i64;

            let lhs_u64 = is_subtype(ctx, &ty1, &IType::U64);
            let rhs_u64 = is_subtype(ctx, &ty2, &IType::U64);
            let u64_mode = lhs_u64 && rhs_u64 && !i64_mode;

            // Division safety: prove divisor is non-zero
            if *op == BinOp::Div || *op == BinOp::Mod {
                check_divisor_nonzero(ctx, &rhs.0, rhs.1)?;
            }

            // Phase 2: reject compile-time-known overflowing folds.
            // In i64 mode or --check-overflow mode, check against i64 bounds.
            // In u64 mode, check against u64 bounds.
            if u64_mode {
                check_const_fold_overflow(*op, &ty1, &ty2, 0, u64::MAX as i128, span)?;
            } else if i64_mode || ctx.check_overflow {
                check_const_fold_overflow(
                    *op,
                    &ty1,
                    &ty2,
                    i64::MIN as i128,
                    i64::MAX as i128,
                    span,
                )?;
            }

            // i64/u64 mode: emit overflow obligation via Z3
            if u64_mode {
                use crate::frontend::typechecker::helpers::check_no_overflow;
                check_no_overflow(ctx, *op, &lhs.0, &rhs.0, 0, u64::MAX as i128, span)?;
            } else if i64_mode || ctx.check_overflow {
                use crate::frontend::typechecker::helpers::check_no_overflow;
                check_no_overflow(
                    ctx,
                    *op,
                    &lhs.0,
                    &rhs.0,
                    i64::MIN as i128,
                    i64::MAX as i128,
                    span,
                )?;
            }

            // Choose the base type for the result refinement
            let result_base = if i64_mode {
                IType::I64
            } else if u64_mode {
                IType::U64
            } else {
                IType::Int
            };

            // Try constant folding first for precise singleton types
            let ty = match join_op(*op, &ty1, &ty2) {
                IType::Int => {
                    // Constant folding failed (non-singleton operands)
                    // Use SMT synthesis: produce {v | v = lhs op rhs}
                    let result_expr = Expr::BinOp {
                        op: *op,
                        lhs: Box::new((**lhs).clone()),
                        rhs: Box::new((**rhs).clone()),
                    };
                    build_equality_refinement(&result_expr, span, result_base)
                }
                singleton_ty => singleton_ty, // Keep singleton if folding worked
            };

            let texpr = TExpr::BinOp {
                op: *op,
                lhs: Box::new(tlhs),
                rhs: Box::new(trhs),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // BINOP-CMP: Comparison operations
        Expr::BinOp {
            op: op @ (BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NotEq),
            lhs,
            rhs,
        } => {
            let (tlhs, ty1) = synth_expr(ctx, lhs)?;
            let (trhs, ty2) = synth_expr(ctx, rhs)?;

            // Check both operands are subtypes of int
            if !is_subtype(ctx, &ty1, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty1,
                    span: lhs.1,
                });
            }
            if !is_subtype(ctx, &ty2, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty2,
                    span: rhs.1,
                });
            }

            let ty = IType::Bool;
            let texpr = TExpr::BinOp {
                op: *op,
                lhs: Box::new(tlhs),
                rhs: Box::new(trhs),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // BINOP-BOOL: Boolean operations
        Expr::BinOp {
            op: op @ (BinOp::And | BinOp::Or | BinOp::Implies),
            lhs,
            rhs,
        } => {
            let (tlhs, ty1) = synth_expr(ctx, lhs)?;
            let (trhs, ty2) = synth_expr(ctx, rhs)?;

            // Check both operands are subtypes of bool
            if !is_subtype(ctx, &ty1, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: ty1,
                    span: lhs.1,
                });
            }
            if !is_subtype(ctx, &ty2, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: ty2,
                    span: rhs.1,
                });
            }

            let ty = IType::Bool;
            let texpr = TExpr::BinOp {
                op: *op,
                lhs: Box::new(tlhs),
                rhs: Box::new(trhs),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // UNARY-NOT: Logical negation
        Expr::UnaryOp {
            op: UnaryOp::Not,
            cond,
        } => {
            let (tcond, ty) = synth_expr(ctx, cond)?;

            if !is_subtype(ctx, &ty, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: ty,
                    span: cond.1,
                });
            }

            let result_ty = IType::Bool;
            let texpr = TExpr::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(tcond),
                ty: result_ty.clone(),
            };
            Ok(((texpr, span), result_ty))
        }

        // UNARY-NEG: Arithmetic negation
        Expr::UnaryOp {
            op: UnaryOp::Neg,
            cond,
        } => {
            let (tcond, ty) = synth_expr(ctx, cond)?;

            if !is_subtype(ctx, &ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty,
                    span: cond.1,
                });
            }

            // u64 negation: unsigned types cannot be negated at all
            let neg_u64 = is_subtype(ctx, &ty, &IType::U64) && !is_subtype(ctx, &ty, &IType::I64);
            if neg_u64 {
                return Err(TypeError::IntegerOverflow {
                    op: "-".to_string(),
                    span,
                });
            }

            // i64 negation: check operand != INT_MIN
            let neg_i64 = is_subtype(ctx, &ty, &IType::I64);
            if neg_i64 || ctx.check_overflow {
                use crate::frontend::typechecker::helpers::check_no_negation_overflow;
                check_no_negation_overflow(ctx, &cond.0, cond.1)?;
            }

            let result_ty = if neg_i64 { IType::I64 } else { IType::Int };
            let texpr = TExpr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(tcond),
                ty: result_ty.clone(),
            };
            Ok(((texpr, span), result_ty))
        }

        // ARRAY-INDEX: Array indexing
        Expr::Index { base, index } => {
            let (tbase, base_ty) = synth_expr(ctx, base)?;
            let (tindex, index_ty) = synth_expr(ctx, index)?;

            // Check index is an integer
            if !is_subtype(ctx, &index_ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: index_ty,
                    span: index.1,
                });
            }

            // Extract element type from array
            match &base_ty {
                IType::Array { element_type, size } => {
                    // Check array bounds - returns error if cannot prove safe
                    // Use the actual index expression so SMT can use context propositions
                    check_array_bounds_expr(ctx, &index.0, &index_ty, size, &base_ty, index.1)?;

                    let elem_ty = (**element_type).clone();
                    let texpr = TExpr::Index {
                        base: Box::new(tbase),
                        index: Box::new(tindex),
                        ty: elem_ty.clone(),
                    };
                    Ok(((texpr, span), elem_ty))
                }
                _ => Err(TypeError::NotAnArray {
                    found: base_ty,
                    span: base.1,
                }),
            }
        }

        // FUNC-CALL: Function call
        Expr::Call { func_name, args } => {
            // Look up function signature
            let sig =
                ctx.lookup_function(func_name)
                    .ok_or_else(|| TypeError::UndefinedFunction {
                        name: func_name.to_string(),
                        span,
                    })?;

            // Check argument count
            if args.0.len() != sig.parameters.len() {
                return Err(TypeError::WrongNumberOfArguments {
                    expected: sig.parameters.len(),
                    found: args.0.len(),
                    span: args.1,
                });
            }

            // Synth and check each argument
            let mut typed_args = Vec::new();
            let mut arg_types = Vec::new();
            for (arg, (_param_name, param_ty)) in args.0.iter().zip(sig.parameters.iter()) {
                let (targ, arg_ty) = synth_expr(ctx, arg)?;

                if !is_subtype(ctx, &arg_ty, param_ty) {
                    return Err(TypeError::TypeMismatch {
                        expected: param_ty.clone(),
                        found: arg_ty,
                        span: arg.1,
                    });
                }

                typed_args.push(targ);
                arg_types.push(arg_ty);
            }

            // Check precondition if present
            if let Some(ref precond) = sig.precondition {
                // Substitute argument values into the precondition
                let arg_exprs: Vec<&Expr> = args.0.iter().map(|a| &a.0).collect();
                let substituted_precond =
                    substitute_args_in_prop(precond, &sig.parameters, &arg_types, &arg_exprs);

                // Substitute symbolic array sizes.
                // For each parameter `arr: [T; n]` where n is symbolic, bind
                // n → concrete_size from the actual argument's type, so the
                // precondition can reason about the concrete length.
                let substituted_precond =
                    substitute_symbolic_sizes(&substituted_precond, &sig.parameters, &arg_types);

                if !crate::frontend::typechecker::check_provable(ctx, &substituted_precond) {
                    return Err(TypeError::PreconditionViolation {
                        function: func_name.to_string(),
                        precondition: precond.clone(),
                        span,
                    });
                }
            }

            let ret_ty = sig.return_type.clone();
            let texpr = TExpr::Call {
                func_name: func_name.to_string(),
                args: typed_args,
                ty: ret_ty.clone(),
            };
            Ok(((texpr, span), ret_ty))
        }

        // ARRAY-INIT: Array initialization [e; n]
        Expr::ArrayInit { value, length } => {
            let (tvalue, elem_ty) = synth_expr(ctx, value)?;

            let size = eval_to_ivalue(length)?;

            // Synthesize the length expression for the typed AST
            let (tlength, _) = synth_expr(ctx, length)?;

            // Keep the synthesized element type (including singletons and refined types)
            // SMT synthesis allows SingletonInt(0) <: {v | v >= 0} via SMT proof
            let array_ty = IType::Array {
                element_type: Arc::new(elem_ty),
                size,
            };

            let texpr = TExpr::ArrayInit {
                value: Box::new(tvalue),
                length: Box::new(tlength),
                ty: array_ty.clone(),
            };
            Ok(((texpr, span), array_ty))
        }

        // IF-EXPR: If expression with flow-sensitive typing
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            // Synthesize condition
            let (tcond, cond_ty) = synth_expr(ctx, cond)?;

            if !is_subtype(ctx, &cond_ty, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: cond_ty,
                    span: cond.1,
                });
            }

            // Extract proposition from condition
            let mut then_ctx = ctx.clone();
            if let Some(prop) = extract_proposition(&cond.0) {
                then_ctx = then_ctx.with_proposition(prop);
            }

            // Check then block statements, then handle trailing expression
            let (tthen_stmts, then_ctx_out) = check_stmts(&then_ctx, &then_block.statements)?;
            let (then_trailing, then_result_ty) = if let Some(trailing) = &then_block.trailing_expr
            {
                let (texpr, ty) = synth_expr(&then_ctx_out, trailing)?;
                (Some(Box::new(texpr)), Some(ty))
            } else {
                (None, None)
            };

            let tthen_block = TBlock {
                statements: tthen_stmts,
                trailing_expr: then_trailing,
            };

            // Type the else block
            let (telse_block, result_ty) = if let Some(else_stmts) = else_block {
                // Extract negated proposition for else branch
                let mut else_ctx = ctx.clone();
                if let Some(prop) = extract_proposition(&cond.0) {
                    let neg_prop = negate_proposition(&prop);
                    else_ctx = else_ctx.with_proposition(neg_prop);
                }

                let (telse_stmts, else_ctx_out) = check_stmts(&else_ctx, &else_stmts.statements)?;
                let (else_trailing, else_result_ty) =
                    if let Some(trailing) = &else_stmts.trailing_expr {
                        let (texpr, ty) = synth_expr(&else_ctx_out, trailing)?;
                        (Some(Box::new(texpr)), Some(ty))
                    } else {
                        (None, None)
                    };

                let telse_block_val = TBlock {
                    statements: telse_stmts,
                    trailing_expr: else_trailing,
                };

                // If both branches have a trailing expression, use their join
                let ty = match (&then_result_ty, &else_result_ty) {
                    (Some(t1), Some(t2)) => {
                        if is_subtype(ctx, t1, t2) {
                            t2.clone()
                        } else if is_subtype(ctx, t2, t1) {
                            t1.clone()
                        } else {
                            IType::Int // widen to int as fallback
                        }
                    }
                    _ => IType::Unit,
                };
                (Some(telse_block_val), ty)
            } else {
                (None, then_result_ty.unwrap_or(IType::Unit))
            };
            let _ = then_ctx_out;

            let texpr = TExpr::If {
                cond: Box::new(tcond),
                then_block: tthen_block,
                else_block: telse_block,
                ty: result_ty.clone(),
            };

            Ok(((texpr, span), result_ty))
        }

        // FORALL/EXISTS: Quantifier expressions (specification-only)
        Expr::Forall {
            var,
            start,
            end,
            body,
        }
        | Expr::Exists {
            var,
            start,
            end,
            body,
        } => {
            // Synthesize start and end — verify both are <: Int
            let (_tstart, start_ty) = synth_expr(ctx, start)?;
            if !is_subtype(ctx, &start_ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: start_ty,
                    span: start.1,
                });
            }
            let (_tend, end_ty) = synth_expr(ctx, end)?;
            if !is_subtype(ctx, &end_ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: end_ty,
                    span: end.1,
                });
            }

            // Extend context with var: Int and range propositions
            let mut extended_ctx = ctx.with_immutable(var.to_string(), IType::Int);

            // Add range propositions: var >= start && var < end
            let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);
            let lower_bound_expr = Expr::BinOp {
                op: BinOp::Gte,
                lhs: Box::new((Expr::Variable(var), dummy_span)),
                rhs: Box::new((*start.clone()).clone()),
            };
            extended_ctx = extended_ctx.with_proposition(crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((lower_bound_expr, dummy_span)),
            });
            let upper_bound_expr = Expr::BinOp {
                op: BinOp::Lt,
                lhs: Box::new((Expr::Variable(var), dummy_span)),
                rhs: Box::new((*end.clone()).clone()),
            };
            extended_ctx = extended_ctx.with_proposition(crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((upper_bound_expr, dummy_span)),
            });

            // Synthesize body — verify it is <: Bool
            let (_tbody, body_ty) = synth_expr(&extended_ctx, body)?;
            if !is_subtype(&extended_ctx, &body_ty, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: body_ty,
                    span: body.1,
                });
            }

            // Quantifiers only appear in specification contexts (invariants, requires, ensures).
            // Return a dummy TExpr — the invariant is consumed as raw Expr, not TExpr.
            // Callers in specification context should use allow_quantifiers=true.
            if ctx.allow_quantifiers {
                Ok((
                    (
                        TExpr::Literal {
                            value: Literal::Bool(true),
                            ty: IType::Bool,
                        },
                        span,
                    ),
                    IType::Bool,
                ))
            } else {
                Err(TypeError::UnsupportedFeature {
                    feature: "quantifier in runtime expression".to_string(),
                    span,
                })
            }
        }

        _ => Err(TypeError::UnsupportedFeature {
            feature: "this expression form".to_string(),
            span,
        }),
    }
}

/// Evaluate an expression to a compile-time value
fn eval_to_ivalue<'src>(expr: &Spanned<Expr<'src>>) -> Result<IValue, TypeError<'src>> {
    match &expr.0 {
        Expr::Literal(Literal::Int(n)) => Ok(IValue::Int(*n)),
        Expr::Variable(name) => Ok(IValue::Symbolic(name.to_string())),
        _ => Err(TypeError::NotAConstant { span: expr.1 }),
    }
}

/// Substitute argument types/values into a proposition (precondition or postcondition).
/// This replaces parameter names with their actual argument values when possible.
/// Substitute symbolic array-size variables in a proposition using the
/// concrete sizes of the actual arguments.
///
/// For each parameter typed `[T; n]` with `n: Symbolic`, the argument must
/// have a concrete `Int(k)` size (enforced by subtyping); we then rewrite
/// every `Expr::Variable(n)` in the proposition to `Literal::Int(k)`.
pub(super) fn substitute_symbolic_sizes<'src>(
    prop: &crate::common::types::IProposition<'src>,
    params: &[(String, IType<'src>)],
    arg_types: &[IType<'src>],
) -> crate::common::types::IProposition<'src> {
    use crate::common::types::IProposition;
    use chumsky::span::SimpleSpan;

    let mut expr = prop.predicate.0.clone();
    for ((_, param_ty), arg_ty) in params.iter().zip(arg_types.iter()) {
        if let (
            IType::Array {
                size: IValue::Symbolic(name),
                ..
            },
            IType::Array {
                size: IValue::Int(k),
                ..
            },
        ) = (param_ty, arg_ty)
        {
            expr =
                crate::frontend::typechecker::check::substitute_var_with_literal(&expr, name, *k);
        }
    }

    IProposition {
        var: prop.var.clone(),
        predicate: Arc::new((expr, SimpleSpan::new(0, 0))),
    }
}

pub(super) fn substitute_args_in_prop<'src>(
    precond: &crate::common::types::IProposition<'src>,
    params: &[(String, IType<'src>)],
    arg_types: &[IType<'src>],
    arg_exprs: &[&Expr<'src>],
) -> crate::common::types::IProposition<'src> {
    use crate::common::types::IProposition;
    use crate::frontend::typechecker::helpers::rename_expr_var;
    use chumsky::span::SimpleSpan;

    // First pass: substitute parameter names with argument expressions.
    // - If argument is a variable: rename param -> arg_var
    // - If argument is a complex expression (e.g., 0 - n): substitute the
    //   entire expression tree for the parameter name in the precondition
    let mut renamed_expr = precond.predicate.0.clone();
    for ((param_name, _), arg_expr) in params.iter().zip(arg_exprs.iter()) {
        match arg_expr {
            Expr::Variable(arg_var) if *arg_var != param_name.as_str() => {
                renamed_expr = rename_expr_var(&renamed_expr, param_name.as_str(), arg_var);
            }
            Expr::Variable(_) => {
                // Same name — no rename needed
            }
            _ => {
                // Complex expression: substitute the expression tree for the param name
                use crate::frontend::typechecker::helpers::substitute_expr_for_var;
                renamed_expr =
                    substitute_expr_for_var(&renamed_expr, param_name.as_str(), arg_expr);
            }
        }
    }

    // Second pass: substitute singleton values (int literals)
    let mut substitutions: std::collections::HashMap<&str, &IType> =
        std::collections::HashMap::new();
    for ((param_name, _), (arg_ty, arg_expr)) in
        params.iter().zip(arg_types.iter().zip(arg_exprs.iter()))
    {
        // Use the argument variable name if it was renamed, otherwise the param name
        let key: &str = if let Expr::Variable(arg_var) = arg_expr {
            arg_var
        } else {
            param_name.as_str()
        };
        substitutions.insert(key, arg_ty);
    }

    let substituted_expr = substitute_in_expr(&renamed_expr, &substitutions);

    IProposition {
        var: precond.var.clone(),
        predicate: Arc::new((substituted_expr, SimpleSpan::new(0, 0))),
    }
}

/// Substitute parameter names with argument values in an expression
fn substitute_in_expr<'src>(
    expr: &Expr<'src>,
    subs: &std::collections::HashMap<&str, &IType<'src>>,
) -> Expr<'src> {
    match expr {
        Expr::Variable(name) => {
            // If this variable is a parameter, substitute with actual value if it's a singleton
            if let Some(arg_ty) = subs.get(name) {
                match arg_ty {
                    IType::SingletonInt(IValue::Int(n)) => Expr::Literal(Literal::Int(*n)),
                    // For symbolic or non-singleton types, keep the variable reference
                    // (the variable will be looked up in the context during proof checking)
                    _ => expr.clone(),
                }
            } else {
                expr.clone()
            }
        }

        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((substitute_in_expr(&lhs.0, subs), lhs.1)),
            rhs: Box::new((substitute_in_expr(&rhs.0, subs), rhs.1)),
        },

        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((substitute_in_expr(&cond.0, subs), cond.1)),
        },

        Expr::Forall {
            var,
            start,
            end,
            body,
        } => {
            if subs.contains_key(var) {
                // Bound variable shadows substitution
                expr.clone()
            } else {
                Expr::Forall {
                    var,
                    start: Box::new((substitute_in_expr(&start.0, subs), start.1)),
                    end: Box::new((substitute_in_expr(&end.0, subs), end.1)),
                    body: Box::new((substitute_in_expr(&body.0, subs), body.1)),
                }
            }
        }

        Expr::Exists {
            var,
            start,
            end,
            body,
        } => {
            if subs.contains_key(var) {
                expr.clone()
            } else {
                Expr::Exists {
                    var,
                    start: Box::new((substitute_in_expr(&start.0, subs), start.1)),
                    end: Box::new((substitute_in_expr(&end.0, subs), end.1)),
                    body: Box::new((substitute_in_expr(&body.0, subs), body.1)),
                }
            }
        }

        // Other expression forms stay as-is
        _ => expr.clone(),
    }
}
