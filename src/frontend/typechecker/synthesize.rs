// Given an expression e, synthesize its type T and produce a typed expression e'

use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::span::{Span, Spanned};
use crate::common::tast::TExpr;
use crate::common::types::{IType, IValue};
use crate::frontend::typechecker::{TypeError, TypingContext, VarBinding, is_subtype};
use std::sync::Arc;

/// Synthesize the type of an expression
///
/// Returns a typed expression and its type, or a type error
pub fn synth_expr<'src>(
    ctx: &TypingContext<'src>,
    expr: &Spanned<Expr<'src>>,
) -> Result<(Spanned<TExpr<'src>>, IType<'src>), TypeError<'src>> {
    let span = expr.1;

    match &expr.0 {
        // INT-LIT: Integer literals have singleton types
        //  n  int(n)
        Expr::Literal(Literal::Int(n)) => {
            let ty = IType::SingletonInt(IValue::Int(*n));
            let texpr = TExpr::Literal {
                value: Literal::Int(*n),
                ty: ty.clone(),
            };
            Ok((Spanned(texpr, span), ty))
        }

        // BOOL-LIT: Boolean literals have singleton types
        //  true  bool  (we use base bool, not singleton for simplicity)
        Expr::Literal(Literal::Bool(b)) => {
            let ty = IType::Bool;
            let texpr = TExpr::Literal {
                value: Literal::Bool(*b),
                ty: ty.clone(),
            };
            Ok((Spanned(texpr, span), ty))
        }

        // VAR: Look up variable in context
        //  x  T
        Expr::Variable(name) => {
            match ctx.lookup_variable(name) {
                Some(VarBinding::Immutable(ty)) => {
                    let texpr = TExpr::Variable {
                        name: name.to_string(),
                        ty: ty.clone(),
                    };
                    Ok((Spanned(texpr, span), ty.clone()))
                }
                Some(VarBinding::Mutable(binding)) => {
                    // For mutable variables, use the current type
                    let ty = binding.current_type.clone();
                    let texpr = TExpr::Variable {
                        name: name.to_string(),
                        ty: ty.clone(),
                    };
                    Ok((Spanned(texpr, span), ty))
                }
                None => Err(TypeError::UndefinedVariable {
                    name: name.to_string(),
                    span,
                }),
            }
        }

        // BINOP-ARITH: Arithmetic operations
        // e1  T1,  e2  T2,  T1 <: int,  T2 <: int
        //  e1 op e2  int   
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

            let ty = IType::Int;
            let texpr = TExpr::BinOp {
                op: *op,
                lhs: Box::new(tlhs),
                rhs: Box::new(trhs),
                ty: ty.clone(),
            };
            Ok((Spanned(texpr, span), ty))
        }

        // BINOP-CMP: Comparison operations
        // e1  T1,  e2  T2,  T1 <: int,  T2 <: int
        //  e1 op e2  bool   
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
            Ok((Spanned(texpr, span), ty))
        }

        // BINOP-BOOL: Boolean operations
        // e1  T1,  e2  T2,  T1 <: bool,  T2 <: bool
        //  e1 op e2  bool   
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
            Ok((Spanned(texpr, span), ty))
        }

        // UNARY-NOT: Logical negation
        // e  T,  T <: bool
        //  !e  bool
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
                cond: Box::new(tcond),
                ty: result_ty.clone(),
            };
            Ok((Spanned(texpr, span), result_ty))
        }

        // UNARY-NEG: Arithmetic negation
        // e  T,  T <: int
        //  -e  int
        Expr::UnaryOp { op: UnaryOp::Neg, cond } => {
            let (tcond, ty) = synth_expr(ctx, cond)?;

            if !is_subtype(ctx, &ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: ty,
                    span: cond.1,
                });
            }

            let result_ty = IType::Int;
            let texpr = TExpr::UnaryOp {
                op: UnaryOp::Neg,
                cond: Box::new(tcond),
                ty: result_ty.clone(),
            };
            Ok((Spanned(texpr, span), result_ty))
        }

        // ARRAY-INDEX: Array indexing
        // e1  [T; n],  e2  T_idx,  T_idx <: int
        //  e1[e2]  T
        Expr::ArrayIndex { array, index } => {
            let (tarray, array_ty) = synth_expr(ctx, array)?;
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
            match &array_ty {
                IType::Array { element_type, .. } => {
                    let elem_ty = (**element_type).clone();
                    let texpr = TExpr::ArrayIndex {
                        array: Box::new(tarray),
                        index: Box::new(tindex),
                        ty: elem_ty.clone(),
                    };
                    Ok((Spanned(texpr, span), elem_ty))
                }
                _ => Err(TypeError::NotAnArray {
                    found: array_ty,
                    span: array.1,
                }),
            }
        }

        // FUNC-CALL: Function call
        // f: (T1, ..., Tn) -> T_ret 
        // e1  S1, ..., en  Sn
        // S1 <: T1, ..., Sn <: Tn
        //  f(e1, ..., en)  T_ret
        Expr::FunctionCall { name, args } => {
            // Look up function signature
            let sig = ctx.lookup_function(name)
                .ok_or_else(|| TypeError::UndefinedFunction {
                    name: name.to_string(),
                    span,
                })?;

            // Check argument count
            if args.len() != sig.params.len() {
                return Err(TypeError::WrongNumberOfArguments {
                    expected: sig.params.len(),
                    found: args.len(),
                    span,
                });
            }

            // Synth and check each argument
            let mut typed_args = Vec::new();
            for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                let (targ, arg_ty) = synth_expr(ctx, arg)?;

                if !is_subtype(ctx, &arg_ty, param_ty) {
                    return Err(TypeError::TypeMismatch {
                        expected: param_ty.clone(),
                        found: arg_ty,
                        span: arg.1,
                    });
                }

                typed_args.push(targ);
            }

            let ret_ty = sig.return_type.clone();
            let texpr = TExpr::FunctionCall {
                name: name.to_string(),
                args: typed_args,
                ty: ret_ty.clone(),
            };
            Ok((Spanned(texpr, span), ret_ty))
        }

        // ARRAY-INIT: Array initialization [e; n]
        // e  T,  n is a compile-time constant
        //  [e; n]  [T; n]
        Expr::ArrayInit { value, length } => {
            let (tvalue, elem_ty) = synth_expr(ctx, value)?;

            // Evaluate length to IValue
            let size = eval_to_ivalue(length)?;

            let array_ty = IType::Array {
                element_type: Arc::new(elem_ty),
                size,
            };

            let texpr = TExpr::ArrayInit {
                value: Box::new(tvalue),
                length: Box::new(*length.clone()),
                ty: array_ty.clone(),
            };
            Ok((Spanned(texpr, span), array_ty))
        }

        _ => Err(TypeError::UnsupportedFeature {
            feature: "this expression form".to_string(),
            span,
        }),
    }
}

/// Evaluate an expression to a compile-time value
fn eval_to_ivalue(expr: &Spanned<Expr>) -> Result<IValue, TypeError> {
    match &expr.0 {
        Expr::Literal(Literal::Int(n)) => Ok(IValue::Int(*n)),
        Expr::Variable(name) => Ok(IValue::Symbolic(name.to_string())),
        _ => Err(TypeError::NotAConstant {
            span: expr.1,
        }),
    }
}
