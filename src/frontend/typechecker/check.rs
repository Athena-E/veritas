// Statement and program type checking
//
// Implements:
// - Statement checking: ; ;   s  (s', ', ')
// - Function checking: Check function bodies and return types
// - Program checking: Top-level orchestration

use crate::common::ast::{Function, Program, Stmt, Type as AstType};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TExpr, TFunction, TFunctionBody, TParameter, TProgram, TStmt};
use crate::common::types::{FunctionSignature, IType, IValue};
use crate::frontend::typechecker::{
    TypeError, TypingContext, is_subtype, synth_expr,
};
use im::HashMap;
use std::sync::Arc;

/// Check a statement and produce a typed statement with updated context
///
/// Returns (typed_stmt, new_context)
pub fn check_stmt<'src>(
    ctx: &TypingContext<'src>,
    stmt: &Spanned<Stmt<'src>>,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    let span = stmt.1;

    match &stmt.0 {
        // LET-IMMUT: Immutable variable binding
        // ; ;   e  T_e
        // T_e <: T_ann
        // ; ;   let x: T_ann = e  (let x: T_e = e', [x: T_e], )
        Stmt::Let { name, ty, value, is_mut: false } => {
            // Synthesize type of initializer
            let (tvalue, value_ty) = synth_expr(ctx, value)?;

            // Convert annotated type to semantic type
            let ann_ty = ast_type_to_itype(ty)?;

            // Check value type is subtype of annotation
            if !is_subtype(ctx, &value_ty, &ann_ty) {
                return Err(TypeError::TypeMismatch {
                    expected: ann_ty,
                    found: value_ty,
                    span: value.1,
                });
            }

            // Add to context with the synthesized type (more precise)
            let new_ctx = ctx.with_immutable(name.to_string(), value_ty.clone());

            let tstmt = TStmt::Let {
                is_mut: false,
                name: name.to_string(),
                declared_ty: ann_ty,
                value: tvalue,
                checked_ty: value_ty,
            };

            Ok(((tstmt, span), new_ctx))
        }

        // LET-MUT: Mutable variable binding
        // ; ;   e  T_e
        // T_e <: T_ann
        // ; ;   let mut x: T_ann = e  (let mut x: T_e = e', , [x: (T_e, M(T_ann))])
        Stmt::Let { name, ty, value, is_mut: true } => {
            // Synthesize type of initializer
            let (tvalue, value_ty) = synth_expr(ctx, value)?;

            // Convert annotated type to semantic type
            let ann_ty = ast_type_to_itype(ty)?;

            // Check value type is subtype of annotation
            if !is_subtype(ctx, &value_ty, &ann_ty) {
                return Err(TypeError::TypeMismatch {
                    expected: ann_ty.clone(),
                    found: value_ty,
                    span: value.1,
                });
            }

            // Add to mutable context with current type and master type
            let master_ty = IType::Master(Arc::new(ann_ty.clone()));
            let new_ctx = ctx.with_mutable(name.to_string(), value_ty.clone(), master_ty);

            let tstmt = TStmt::Let {
                is_mut: true,
                name: name.to_string(),
                declared_ty: ann_ty,
                value: tvalue,
                checked_ty: value_ty,
            };

            Ok(((tstmt, span), new_ctx))
        }

        // ASSIGN: Assignment
        // Handles both variable assignment and array indexing assignment
        Stmt::Assignment { lhs, rhs } => {
            // Synthesize type of RHS value
            let (trhs, rhs_ty) = synth_expr(ctx, rhs)?;

            // Check what kind of LHS we have
            match &lhs.0 {
                // Variable assignment
                crate::common::ast::Expr::Variable(var_name) => {
                    // Look up mutable variable
                    let binding = ctx.lookup_mutable(var_name)
                        .ok_or_else(|| TypeError::NotMutable {
                            name: var_name.to_string(),
                            span,
                        })?;

                    // Extract master type and check subtyping
                    let master_base = match &binding.master_type {
                        IType::Master(base) => base.as_ref(),
                        _ => &binding.master_type,
                    };

                    if !is_subtype(ctx, &rhs_ty, master_base) {
                        return Err(TypeError::TypeMismatch {
                            expected: master_base.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

                    // Update mutable variable's current type
                    let new_ctx = ctx.with_mutable_update(var_name, rhs_ty.clone())
                        .map_err(|e| TypeError::InvalidAssignment {
                            variable: var_name.to_string(),
                            reason: e,
                            span,
                        })?;

                    // Create TExpr for the left-hand side
                    let tlhs_expr = TExpr::Variable {
                        name: var_name.to_string(),
                        ty: binding.current_type.clone(),
                    };

                    let tstmt = TStmt::Assignment {
                        lhs: (tlhs_expr, lhs.1),
                        rhs: trhs,
                    };

                    Ok(((tstmt, span), new_ctx))
                }

                // Array indexing assignment
                crate::common::ast::Expr::Index { base, index } => {
                    let (tbase, base_ty) = synth_expr(ctx, base)?;
                    let (tindex, index_ty) = synth_expr(ctx, index)?;

                    // Check index is int
                    if !is_subtype(ctx, &index_ty, &IType::Int) {
                        return Err(TypeError::TypeMismatch {
                            expected: IType::Int,
                            found: index_ty,
                            span: index.1,
                        });
                    }

                    // Check array type and extract element type
                    let elem_ty = match &base_ty {
                        IType::Array { element_type, .. } => element_type.as_ref(),
                        _ => return Err(TypeError::NotAnArray {
                            found: base_ty,
                            span: base.1,
                        }),
                    };

                    // Check value type matches element type
                    if !is_subtype(ctx, &rhs_ty, elem_ty) {
                        return Err(TypeError::TypeMismatch {
                            expected: elem_ty.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

                    // Create typed LHS (the index expression)
                    let tlhs_expr = TExpr::Index {
                        base: Box::new(tbase),
                        index: Box::new(tindex),
                        ty: elem_ty.clone(),
                    };

                    let tstmt = TStmt::Assignment {
                        lhs: (tlhs_expr, lhs.1),
                        rhs: trhs,
                    };

                    // Context unchanged for array assignment
                    Ok(((tstmt, span), ctx.clone()))
                }

                _ => Err(TypeError::InvalidAssignment {
                    variable: format!("{:?}", lhs.0),
                    reason: "Invalid assignment target".to_string(),
                    span: lhs.1,
                }),
            }
        }

        // RETURN: Return statement
        Stmt::Return { expr } => {
            let (texpr, _) = synth_expr(ctx, expr)?;

            let tstmt = TStmt::Return {
                expr: Box::new(texpr),
            };

            Ok(((tstmt, span), ctx.clone()))
        }

        // FOR-LOOP: For loop with range
        // ; ;   start  T_start,  T_start <: int
        // ; ;   end  T_end,  T_end <: int
        // ; [i: int];   body  (body', _, _)
        // ; ;   for i in start..end { body }  (for i in start..end { body' }, , )
        Stmt::For { var, start, end, body } => {
            let (tstart, start_ty) = synth_expr(ctx, start)?;
            let (tend, end_ty) = synth_expr(ctx, end)?;

            // Check start and end are integers
            if !is_subtype(ctx, &start_ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: start_ty,
                    span: start.1,
                });
            }
            if !is_subtype(ctx, &end_ty, &IType::Int) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Int,
                    found: end_ty,
                    span: end.1,
                });
            }

            // Add loop variable to context (immutable)
            let loop_ctx = ctx.with_immutable(var.to_string(), IType::Int);

            // Check body statements
            let (tbody, _) = check_stmts(&loop_ctx, body)?;

            let tstmt = TStmt::For {
                var: var.to_string(),
                var_ty: IType::Int,
                start: Box::new(tstart),
                end: Box::new(tend),
                body: tbody,
            };

            // Context unchanged after loop (loop variable goes out of scope)
            Ok(((tstmt, span), ctx.clone()))
        }

        // EXPR-STMT: Expression statement
        // ; ;   e  T
        // ; ;   e;  (e', , )
        Stmt::Expr(expr) => {
            let (texpr, _) = synth_expr(ctx, &expr)?;

            let tstmt = TStmt::Expr(texpr);

            Ok(((tstmt, span), ctx.clone()))
        }
    }
}

/// Check a sequence of statements
///
/// Returns (typed_stmts, final_context)
pub fn check_stmts<'src>(
    ctx: &TypingContext<'src>,
    stmts: &[Spanned<Stmt<'src>>],
) -> Result<(Vec<Spanned<TStmt<'src>>>, TypingContext<'src>), TypeError<'src>> {
    let mut current_ctx = ctx.clone();
    let mut typed_stmts = Vec::new();

    for stmt in stmts {
        let (tstmt, new_ctx) = check_stmt(&current_ctx, stmt)?;
        typed_stmts.push(tstmt);
        current_ctx = new_ctx;
    }

    Ok((typed_stmts, current_ctx))
}

/// Check a function
///
/// Returns typed function
pub fn check_function<'src>(
    global_ctx: &TypingContext<'src>,
    func: &Spanned<Function<'src>>,
) -> Result<TFunction<'src>, TypeError<'src>> {
    let (func_inner, func_span) = func;

    // Convert parameter types to semantic types
    let mut param_types = Vec::new();
    for spanned_param in &func_inner.parameters {
        let param = &spanned_param.0;
        param_types.push(ast_type_to_itype(&param.ty)?);
    }

    // Convert return type
    let return_type = ast_type_to_itype(&func_inner.return_type)?;

    // Create context with parameters
    let mut func_ctx = global_ctx.clone();
    for (spanned_param, ty) in func_inner.parameters.iter().zip(param_types.iter()) {
        let param = &spanned_param.0;
        func_ctx = func_ctx.with_immutable(param.name.to_string(), ty.clone());
    }

    // Check body statements
    let (tbody, final_ctx) = check_stmts(&func_ctx, &func_inner.body.statements)?;

    // Check return expression (if present)
    let treturn = if let Some(ret_expr) = &func_inner.body.return_expr {
        let (texpr, ret_ty) = synth_expr(&final_ctx, ret_expr)?;

        // Check return type matches signature
        if !is_subtype(&final_ctx, &ret_ty, &return_type) {
            return Err(TypeError::TypeMismatch {
                expected: return_type.clone(),
                found: ret_ty,
                span: ret_expr.1,
            });
        }

        Some(texpr)
    } else {
        // No return expression - check that return type is unit
        if !matches!(return_type, IType::Unit) {
            return Err(TypeError::MissingReturn {
                expected: return_type,
                span: *func_span,
            });
        }
        None
    };

    // Build typed function
    let tparams: Vec<TParameter> = func_inner.parameters.iter().zip(param_types.iter())
        .map(|(spanned_param, ty)| TParameter {
            name: spanned_param.0.name.to_string(),
            ty: ty.clone(),
        })
        .collect();

    let tfunc = TFunction {
        name: func_inner.name.to_string(),
        parameters: tparams,
        return_type,
        body: TFunctionBody {
            statements: tbody,
            return_expr: treturn.map(Box::new),
        },
        span: *func_span,
    };

    Ok(tfunc)
}

/// Check an entire program
pub fn check_program<'src>(
    program: &Program<'src>,
) -> Result<TProgram<'src>, TypeError<'src>> {
    // Collect function signatures
    let mut signatures = HashMap::new();

    for spanned_func in &program.functions {
        let (func, func_span) = spanned_func;

        // Convert parameter types (with names)
        let mut parameters = Vec::new();
        for spanned_param in &func.parameters {
            let param = &spanned_param.0;
            let ty = ast_type_to_itype(&param.ty)?;
            parameters.push((param.name.to_string(), ty));
        }

        // Convert return type
        let return_type = ast_type_to_itype(&func.return_type)?;

        let sig = FunctionSignature {
            name: func.name.to_string(),
            parameters,
            return_type,
            precondition: None,  // TODO: Parse preconditions from AST
            span: *func_span,
        };

        signatures.insert(func.name.to_string(), sig);
    }

    // Create global context with all function signatures
    let global_ctx = TypingContext::with_functions(signatures);

    // Check each function
    let mut tfunctions = Vec::new();

    for spanned_func in &program.functions {
        let tfunc = check_function(&global_ctx, spanned_func)?;
        tfunctions.push(tfunc);
    }

    Ok(TProgram {
        functions: tfunctions,
    })
}

/// Convert AST type to semantic internal type
fn ast_type_to_itype<'src>(ty: &Spanned<AstType<'src>>) -> Result<IType<'src>, TypeError<'src>> {
    match &ty.0 {
        AstType::Unit => Ok(IType::Unit),
        AstType::Int => Ok(IType::Int),
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

        AstType::SingletonInt(expr) => {
            // Evaluate the expression to get the singleton value
            let value = eval_array_size(expr)?;
            Ok(IType::SingletonInt(value))
        }
    }
}

/// Evaluate array size expression to IValue
fn eval_array_size<'src>(expr: &Spanned<crate::common::ast::Expr<'src>>) -> Result<IValue, TypeError<'src>> {
    use crate::common::ast::{Expr, Literal};

    match &expr.0 {
        Expr::Literal(Literal::Int(n)) => Ok(IValue::Int(*n)),
        Expr::Variable(name) => Ok(IValue::Symbolic(name.to_string())),
        _ => Err(TypeError::NotAConstant {
            span: expr.1,
        }),
    }
}
