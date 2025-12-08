// Statement and program type checking

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
/// Returns (typed_stmt, new_context)
pub fn check_stmt<'src>(
    ctx: &TypingContext<'src>,
    stmt: &Spanned<Stmt<'src>>,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    let span = stmt.1;

    match &stmt.0 {
        // LET-IMMUT: Immutable variable binding
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
                    let (elem_ty, array_size) = match &base_ty {
                        IType::Array { element_type, size } => (element_type.as_ref(), size),
                        _ => return Err(TypeError::NotAnArray {
                            found: base_ty,
                            span: base.1,
                        }),
                    };

                    // Check array bounds using the actual index expression
                    crate::frontend::typechecker::check_array_bounds_expr(
                        ctx,
                        &index.0,
                        &index_ty,
                        array_size,
                        &base_ty,
                        index.1,
                    )?;

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
            let (texpr, ret_ty) = synth_expr(ctx, expr)?;

            // Check return type matches expected return type
            if let Some(expected) = ctx.get_expected_return() {
                if !is_subtype(ctx, &ret_ty, expected) {
                    return Err(TypeError::ReturnTypeMismatch {
                        expected: expected.clone(),
                        found: ret_ty,
                        span: expr.1,
                    });
                }
            }

            let tstmt = TStmt::Return {
                expr: Box::new(texpr),
            };

            Ok(((tstmt, span), ctx.clone()))
        }

        // FOR-LOOP: For loop with range and optional invariant
        Stmt::For { var, start, end, invariant, body } => {
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
            let mut loop_ctx = ctx.with_immutable(var.to_string(), IType::Int);

            // Add propositions about loop variable bounds: var >= start && var < end
            // This allows the SMT solver to prove array bounds within the loop
            let dummy_span = chumsky::span::SimpleSpan::new(0, 0);

            // Create proposition: var >= start
            let lower_bound_expr = crate::common::ast::Expr::BinOp {
                op: crate::common::ast::BinOp::Gte,
                lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                rhs: Box::new((*start.clone()).clone()),
            };
            let lower_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((lower_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(lower_bound_prop);

            // Create proposition: var < end
            let upper_bound_expr = crate::common::ast::Expr::BinOp {
                op: crate::common::ast::BinOp::Lt,
                lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                rhs: Box::new((*end.clone()).clone()),
            };
            let upper_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((upper_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(upper_bound_prop);

            // Process invariant if present
            let tinvariant = if let Some(inv_expr) = invariant {
                // Type-check the invariant expression (must be bool)
                let (tinv, inv_ty) = synth_expr(&loop_ctx, inv_expr)?;

                if !is_subtype(&loop_ctx, &inv_ty, &IType::Bool) {
                    return Err(TypeError::TypeMismatch {
                        expected: IType::Bool,
                        found: inv_ty,
                        span: inv_expr.1,
                    });
                }

                // Check that invariant holds at loop entry (with i = start)
                // For now, we add the invariant as an assumption for the loop body
                // A full implementation would verify it holds initially and is preserved

                // Add invariant as proposition to loop body context
                let inv_prop = crate::common::types::IProposition {
                    var: var.to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                loop_ctx = loop_ctx.with_proposition(inv_prop);

                Some(tinv)
            } else {
                None
            };

            // Check body statements with invariant in context
            let (tbody, _) = check_stmts(&loop_ctx, body)?;

            let tstmt = TStmt::For {
                var: var.to_string(),
                var_ty: IType::Int,
                start: Box::new(tstart),
                end: Box::new(tend),
                invariant: tinvariant,
                body: tbody,
            };

            // Context unchanged after loop (loop variable goes out of scope)
            Ok(((tstmt, span), ctx.clone()))
        }

        // EXPR-STMT: Expression statement
        // Special handling for if-expressions to support context joining
        Stmt::Expr(expr) => {
            match &expr.0 {
                // If-expression as statement: handle context joining
                crate::common::ast::Expr::If { cond, then_block, else_block } => {
                    check_if_stmt(ctx, cond, then_block, else_block.as_ref(), span)
                }
                // Other expressions: context unchanged
                _ => {
                    let (texpr, _) = synth_expr(ctx, &expr)?;
                    let tstmt = TStmt::Expr(texpr);
                    Ok(((tstmt, span), ctx.clone()))
                }
            }
        }
    }
}

/// Check an if-statement with context joining
/// Returns the joined context after both branches merge
fn check_if_stmt<'src>(
    ctx: &TypingContext<'src>,
    cond: &Box<Spanned<crate::common::ast::Expr<'src>>>,
    then_block: &[Spanned<crate::common::ast::Stmt<'src>>],
    else_block: Option<&Vec<Spanned<crate::common::ast::Stmt<'src>>>>,
    span: Span,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    use crate::frontend::typechecker::{extract_proposition, negate_proposition};

    // Synthesize condition
    let (tcond, cond_ty) = synth_expr(ctx, cond)?;

    if !is_subtype(ctx, &cond_ty, &IType::Bool) {
        return Err(TypeError::TypeMismatch {
            expected: IType::Bool,
            found: cond_ty,
            span: cond.1,
        });
    }

    // Create then-branch context with condition proposition
    let mut then_ctx = ctx.clone();
    if let Some(prop) = extract_proposition(&cond.0) {
        then_ctx = then_ctx.with_proposition(prop);
    }

    // Check then block and get final context
    let (tthen_block, then_final_ctx) = check_stmts(&then_ctx, then_block)?;

    // Check else block (if present) and get final context
    let (telse_block, else_final_ctx) = if let Some(else_stmts) = else_block {
        let mut else_ctx = ctx.clone();
        if let Some(prop) = extract_proposition(&cond.0) {
            let neg_prop = negate_proposition(&prop);
            else_ctx = else_ctx.with_proposition(neg_prop);
        }

        let (typed_else, else_ctx_final) = check_stmts(&else_ctx, else_stmts)?;
        (Some(typed_else), else_ctx_final)
    } else {
        // No else branch - context unchanged from original
        (None, ctx.clone())
    };

    // Join the contexts from both branches
    let joined_ctx = TypingContext::join_mutable_contexts(&then_final_ctx, &else_final_ctx);

    let texpr = TExpr::If {
        cond: Box::new(tcond),
        then_block: tthen_block,
        else_block: telse_block,
        ty: IType::Unit,
    };

    let tstmt = TStmt::Expr((texpr, span));

    Ok(((tstmt, span), joined_ctx))
}

/// Check a sequence of statements
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

    let return_type = ast_type_to_itype(&func_inner.return_type)?;

    // Create context with parameters and expected return type
    let mut func_ctx = global_ctx.clone();
    for (spanned_param, ty) in func_inner.parameters.iter().zip(param_types.iter()) {
        let param = &spanned_param.0;
        func_ctx = func_ctx.with_immutable(param.name.to_string(), ty.clone());
    }
    // Set expected return type for checking return statements
    func_ctx = func_ctx.with_expected_return(return_type.clone());

    // Add precondition to context (if present) - this allows the function body
    // to assume the precondition holds
    if let Some(precond_expr) = &func_inner.precondition {
        let var_name = func_inner.parameters.first()
            .map(|p| p.0.name.to_string())
            .unwrap_or_else(|| "_".to_string());

        let precond_prop = crate::common::types::IProposition {
            var: var_name,
            predicate: Arc::new(precond_expr.clone()),
        };
        func_ctx = func_ctx.with_proposition(precond_prop);
    }

    let (tbody, final_ctx) = check_stmts(&func_ctx, &func_inner.body.statements)?;

    // Check if body contains any return statements
    fn has_return_stmt(stmts: &[Spanned<TStmt>]) -> bool {
        for (stmt, _) in stmts {
            match stmt {
                TStmt::Return { .. } => return true,
                TStmt::For { body, .. } => {
                    if has_return_stmt(body) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

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
        if !matches!(return_type, IType::Unit) && !has_return_stmt(&tbody) {
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
    let mut signatures = HashMap::new();

    for spanned_func in &program.functions {
        let (func, func_span) = spanned_func;

        // Convert parameter types
        let mut parameters = Vec::new();
        for spanned_param in &func.parameters {
            let param = &spanned_param.0;
            let ty = ast_type_to_itype(&param.ty)?;
            parameters.push((param.name.to_string(), ty));
        }

        // Convert return type
        let return_type = ast_type_to_itype(&func.return_type)?;

        // Convert precondition to IProposition
        let precondition = func.precondition.as_ref().map(|precond_expr| {
            // For preconditions, the bound variable is typically the first parameter
            // or we use a generic "_" if there are no parameters
            let var_name = func.parameters.first()
                .map(|p| p.0.name.to_string())
                .unwrap_or_else(|| "_".to_string());

            crate::common::types::IProposition {
                var: var_name,
                predicate: Arc::new(precond_expr.clone()),
            }
        });

        let sig = FunctionSignature {
            name: func.name.to_string(),
            parameters,
            return_type,
            precondition,
            span: *func_span,
        };

        signatures.insert(func.name.to_string(), sig);
    }

    let global_ctx = TypingContext::with_functions(signatures);

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
