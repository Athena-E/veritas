// Given an expression e, synthesize its type T and produce a typed expression e'

use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::span::{Span, Spanned};
use crate::common::tast::TExpr;
use crate::common::types::{IType, IValue};
use crate::frontend::typechecker::{TypeError, TypingContext, VarBinding, is_subtype, join_op, check_array_bounds_expr, extract_proposition, negate_proposition, check_stmts};
use std::sync::Arc;

/// Widen singleton types to their base types
/// Temp fix: array element types give [int; n] not [int(0); n]
fn widen_type(ty: IType) -> IType {
    match ty {
        IType::SingletonInt(_) => IType::Int,
        // Could add SingletonBool -> Bool if needed
        other => other,
    }
}

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

        // BINOP-ARITH: Arithmetic operations
        Expr::BinOp { op: op @ (BinOp::Add | BinOp::Sub | BinOp::Mul), lhs, rhs } => {
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

            // Use constant folding to produce precise singleton types when possible
            let ty = join_op(*op, &ty1, &ty2);
            let texpr = TExpr::BinOp {
                op: *op,
                lhs: Box::new(tlhs),
                rhs: Box::new(trhs),
                ty: ty.clone(),
            };
            Ok(((texpr, span), ty))
        }

        // BINOP-CMP: Comparison operations
        Expr::BinOp { op: op @ (BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::NotEq), lhs, rhs } => {
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
        Expr::BinOp { op: op @ (BinOp::And | BinOp::Or), lhs, rhs } => {
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
        Expr::UnaryOp { op: UnaryOp::Not, cond } => {
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
            let sig = ctx.lookup_function(func_name)
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
                // For now, create a modified proposition with actual argument types
                let substituted_precond = substitute_args_in_precond(precond, &sig.parameters, &arg_types);

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

            // Widen singleton types for array elements
            // Temp fix: [0; 10] has type [int; 10], not [int(0); 10]
            let widened_elem_ty = widen_type(elem_ty);

            let array_ty = IType::Array {
                element_type: Arc::new(widened_elem_ty),
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
        Expr::If { cond, then_block, else_block } => {
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

            // Check then block
            let (tthen_block, _) = check_stmts(&then_ctx, then_block)?;

            // Type the else block
            let (telse_block, result_ty) = if let Some(else_stmts) = else_block {
                // Extract negated proposition for else branch
                let mut else_ctx = ctx.clone();
                if let Some(prop) = extract_proposition(&cond.0) {
                    let neg_prop = negate_proposition(&prop);
                    else_ctx = else_ctx.with_proposition(neg_prop);
                }

                let (telse_stmts, _) = check_stmts(&else_ctx, else_stmts)?;

                // Temp fix: if-expressions with both branches produce unit type
                // In a full implementation, need to handle return expressions
                (Some(telse_stmts), IType::Unit)
            } else {
                (None, IType::Unit)
            };

            let texpr = TExpr::If {
                cond: Box::new(tcond),
                then_block: tthen_block,
                else_block: telse_block,
                ty: result_ty.clone(),
            };

            Ok(((texpr, span), result_ty))
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
        _ => Err(TypeError::NotAConstant {
            span: expr.1,
        }),
    }
}

/// Substitute argument types/values into a precondition
/// This replaces parameter names with their actual argument values when possible
fn substitute_args_in_precond<'src>(
    precond: &crate::common::types::IProposition<'src>,
    params: &[(String, IType<'src>)],
    arg_types: &[IType<'src>],
) -> crate::common::types::IProposition<'src> {
    use crate::common::types::IProposition;
    use chumsky::span::SimpleSpan;

    // Build a substitution map from parameter names to argument types
    let mut substitutions: std::collections::HashMap<&str, &IType> = std::collections::HashMap::new();
    for ((param_name, _), arg_ty) in params.iter().zip(arg_types.iter()) {
        substitutions.insert(param_name.as_str(), arg_ty);
    }

    // Substitute in the predicate expression
    let substituted_expr = substitute_in_expr(&precond.predicate.0, &substitutions);

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

        // Other expression forms stay as-is
        _ => expr.clone(),
    }
}
