// Statement and program type checking

use crate::common::ast::{Block, Expr, Function, Program, Stmt, Type as AstType};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TBlock, TExpr, TFunction, TFunctionBody, TParameter, TProgram, TStmt};
use crate::common::types::{FunctionSignature, IProposition, IType, IValue};
use crate::frontend::typechecker::{TypeError, TypingContext, is_subtype, synth_expr};
use im::HashMap;
use std::sync::Arc;

/// Recursively walk an expression and replace `arr[i]` subexpressions with their
/// resolved concrete values from the typing context. Returns `(resolved_expr, any_resolved)`.
fn resolve_array_reads_in_expr<'src>(
    ctx: &TypingContext<'src>,
    expr: &Expr<'src>,
) -> (Expr<'src>, bool) {
    match expr {
        Expr::Index { base, index } => {
            if let Expr::Variable(arr_name) = &base.0
                && let Some(resolved) = ctx.resolve_array_element_value(arr_name, &index.0)
            {
                return (resolved, true);
            }
            (expr.clone(), false)
        }
        Expr::BinOp { op, lhs, rhs } => {
            let (new_lhs, l_resolved) = resolve_array_reads_in_expr(ctx, &lhs.0);
            let (new_rhs, r_resolved) = resolve_array_reads_in_expr(ctx, &rhs.0);
            if l_resolved || r_resolved {
                (
                    Expr::BinOp {
                        op: *op,
                        lhs: Box::new((new_lhs, lhs.1)),
                        rhs: Box::new((new_rhs, rhs.1)),
                    },
                    true,
                )
            } else {
                (expr.clone(), false)
            }
        }
        Expr::UnaryOp { op, cond } => {
            let (new_cond, resolved) = resolve_array_reads_in_expr(ctx, &cond.0);
            if resolved {
                (
                    Expr::UnaryOp {
                        op: *op,
                        cond: Box::new((new_cond, cond.1)),
                    },
                    true,
                )
            } else {
                (expr.clone(), false)
            }
        }
        Expr::Call { func_name, args } => {
            let mut any_resolved = false;
            let mut new_args = Vec::new();
            for (arg, sp) in &args.0 {
                let (new_arg, resolved) = resolve_array_reads_in_expr(ctx, arg);
                any_resolved |= resolved;
                new_args.push((new_arg, *sp));
            }
            if any_resolved {
                (
                    Expr::Call {
                        func_name,
                        args: (new_args, args.1),
                    },
                    true,
                )
            } else {
                (expr.clone(), false)
            }
        }
        Expr::ArrayInit { value, length } => {
            let (new_value, resolved) = resolve_array_reads_in_expr(ctx, &value.0);
            if resolved {
                (
                    Expr::ArrayInit {
                        value: Box::new((new_value, value.1)),
                        length: length.clone(),
                    },
                    true,
                )
            } else {
                (expr.clone(), false)
            }
        }
        _ => (expr.clone(), false),
    }
}

/// Check a statement and produce a typed statement with updated context
/// Returns (typed_stmt, new_context)
pub fn check_stmt<'src>(
    ctx: &TypingContext<'src>,
    stmt: &Spanned<Stmt<'src>>,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    let span = stmt.1;

    match &stmt.0 {
        // LET-IMMUT: Immutable variable binding
        Stmt::Let {
            name,
            ty,
            value,
            is_mut: false,
        } => {
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
            let mut new_ctx = ctx.with_immutable(name.to_string(), value_ty.clone());

            // Resolve array reads in the RHS expression and snapshot their values
            // so that subsequent mutations to the array don't drag this binding along.
            let (resolved_rhs, any_resolved) = resolve_array_reads_in_expr(ctx, &value.0);
            if any_resolved {
                let dummy_span = chumsky::span::SimpleSpan::new(0, 0);
                let name_leaked: &'src str = Box::leak(name.to_string().into_boxed_str());
                let eq_expr = Expr::BinOp {
                    op: crate::common::ast::BinOp::Eq,
                    lhs: Box::new((Expr::Variable(name_leaked), dummy_span)),
                    rhs: Box::new((resolved_rhs, dummy_span)),
                };
                let prop = IProposition {
                    var: name.to_string(),
                    predicate: Arc::new((eq_expr, dummy_span)),
                };
                new_ctx = new_ctx.with_proposition(prop);
            }

            // Propagate postcondition from function calls to the binding
            if let Some(prop) = postcondition_for_call(ctx, name, &value.0) {
                new_ctx = new_ctx.with_proposition(prop);
            }

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
        Stmt::Let {
            name,
            ty,
            value,
            is_mut: true,
        } => {
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
            // For arrays, use the annotated type so we can assign values matching the element type
            // (not just the singleton from initialization)
            let master_ty = IType::Master(Arc::new(ann_ty.clone()));
            let current_ty = match &ann_ty {
                IType::Array { .. } | IType::RefinedInt { .. } => ann_ty.clone(),
                _ => value_ty.clone(),
            };
            let mut new_ctx = ctx.with_mutable(name.to_string(), current_ty.clone(), master_ty);

            // For mutable array init, add pointwise propositions for each index
            // rather than a single forall. This way, `arr[k] = v` only invalidates
            // the proposition at index k, preserving knowledge of other elements.
            // e.g. `let mut a = [0;3]` produces:
            //   select(a, 0) == 0, select(a, 1) == 0, select(a, 2) == 0
            if let IType::Array { element_type, size } = &value_ty
                && let IValue::Int(n) = size
            {
                let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);
                let var_leaked: &'src str = Box::leak(name.to_string().into_boxed_str());
                for idx in 0..*n {
                    let arr_index = crate::common::ast::Expr::Index {
                        base: Box::new((
                            crate::common::ast::Expr::Variable(var_leaked),
                            dummy_span,
                        )),
                        index: Box::new((
                            crate::common::ast::Expr::Literal(crate::common::ast::Literal::Int(
                                idx,
                            )),
                            dummy_span,
                        )),
                    };
                    let rhs_expr = match element_type.as_ref() {
                        IType::SingletonInt(IValue::Int(v)) => {
                            crate::common::ast::Expr::Literal(crate::common::ast::Literal::Int(*v))
                        }
                        _ => continue,
                    };
                    let eq_expr = crate::common::ast::Expr::BinOp {
                        op: crate::common::ast::BinOp::Eq,
                        lhs: Box::new((arr_index, dummy_span)),
                        rhs: Box::new((rhs_expr, dummy_span)),
                    };
                    new_ctx = new_ctx.with_proposition(IProposition {
                        var: name.to_string(),
                        predicate: Arc::new((eq_expr, dummy_span)),
                    });
                }
            }

            // Resolve array reads in the RHS and snapshot their values
            let (resolved_rhs, any_resolved) = resolve_array_reads_in_expr(ctx, &value.0);
            if any_resolved {
                let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);
                let name_leaked: &'src str = Box::leak(name.to_string().into_boxed_str());
                let eq_expr = Expr::BinOp {
                    op: crate::common::ast::BinOp::Eq,
                    lhs: Box::new((Expr::Variable(name_leaked), dummy_span)),
                    rhs: Box::new((resolved_rhs, dummy_span)),
                };
                let prop = IProposition {
                    var: name.to_string(),
                    predicate: Arc::new((eq_expr, dummy_span)),
                };
                new_ctx = new_ctx.with_proposition(prop);
            }

            // Propagate postcondition from function calls to the binding
            if let Some(prop) = postcondition_for_call(ctx, name, &value.0) {
                new_ctx = new_ctx.with_proposition(prop);
            }

            let tstmt = TStmt::Let {
                is_mut: true,
                name: name.to_string(),
                declared_ty: ann_ty,
                value: tvalue,
                checked_ty: current_ty,
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
                    let binding =
                        ctx.lookup_mutable(var_name)
                            .ok_or_else(|| TypeError::NotMutable {
                                name: var_name.to_string(),
                                span,
                            })?;

                    // Extract master type and check subtyping
                    let master_base = match &binding.master_type {
                        IType::Master(base) => base.as_ref(),
                        _ => &binding.master_type,
                    };

                    if !check_expr_satisfies_refined(ctx, &rhs.0, &rhs_ty, master_base) {
                        return Err(TypeError::TypeMismatch {
                            expected: master_base.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

                    // Update mutable variable's current type
                    let mut new_ctx =
                        ctx.with_mutable_update(var_name, rhs_ty.clone())
                            .map_err(|e| TypeError::InvalidAssignment {
                                variable: var_name.to_string(),
                                reason: e,
                                span,
                            })?;

                    // Resolve array reads in the RHS and snapshot their values
                    let (resolved_rhs, any_resolved) = resolve_array_reads_in_expr(ctx, &rhs.0);
                    if any_resolved {
                        let dummy_span = chumsky::span::SimpleSpan::new(0, 0);
                        let name_leaked: &'src str =
                            Box::leak(var_name.to_string().into_boxed_str());
                        let eq_expr = Expr::BinOp {
                            op: crate::common::ast::BinOp::Eq,
                            lhs: Box::new((Expr::Variable(name_leaked), dummy_span)),
                            rhs: Box::new((resolved_rhs, dummy_span)),
                        };
                        let prop = IProposition {
                            var: var_name.to_string(),
                            predicate: Arc::new((eq_expr, dummy_span)),
                        };
                        new_ctx = new_ctx.with_proposition(prop);
                    }

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
                        _ => {
                            return Err(TypeError::NotAnArray {
                                found: base_ty,
                                span: base.1,
                            });
                        }
                    };

                    // Check array bounds using the actual index expression
                    crate::frontend::typechecker::check_array_bounds_expr(
                        ctx, &index.0, &index_ty, array_size, &base_ty, index.1,
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

                    // Add pointwise proposition: arr[i]...[k] == rhs.
                    // Supports nested index chains (2D+) by extracting the root
                    // array name and the full index tuple.
                    let mut new_ctx = ctx.clone();

                    if let Some((arr_name, indices)) = extract_array_access(&lhs.0) {
                        let dummy_span = chumsky::span::SimpleSpan::new(0, 0);

                        // Snapshot array reads in the RHS so the proposition
                        // doesn't contain live references that become stale
                        // after later mutations.
                        let (resolved, any_resolved) = resolve_array_reads_in_expr(ctx, &rhs.0);
                        let snapshot_rhs = if any_resolved {
                            resolved
                        } else {
                            rhs.0.clone()
                        };

                        // Rebuild the nested Index chain: arr[i0][i1]...[ik]
                        let mut lhs_access = crate::common::ast::Expr::Variable(arr_name);
                        for idx in &indices {
                            lhs_access = crate::common::ast::Expr::Index {
                                base: Box::new((lhs_access, dummy_span)),
                                index: Box::new((idx.clone(), dummy_span)),
                            };
                        }
                        let eq_expr = crate::common::ast::Expr::BinOp {
                            op: crate::common::ast::BinOp::Eq,
                            lhs: Box::new((lhs_access, dummy_span)),
                            rhs: Box::new((snapshot_rhs, dummy_span)),
                        };
                        let prop = IProposition {
                            var: arr_name.to_string(),
                            predicate: Arc::new((eq_expr, dummy_span)),
                        };

                        new_ctx = invalidate_array_props_selectively(&new_ctx, arr_name, &indices);
                        new_ctx = new_ctx.with_proposition(prop);
                    }

                    Ok(((tstmt, span), new_ctx))
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
            if let Some(expected) = ctx.get_expected_return()
                && !is_subtype(ctx, &ret_ty, expected)
            {
                return Err(TypeError::ReturnTypeMismatch {
                    expected: expected.clone(),
                    found: ret_ty,
                    span: expr.1,
                });
            }

            // Check postcondition if present
            if let Some(postcond) = ctx.get_postcondition() {
                let substituted = substitute_result_in_postcond(postcond, &ret_ty, &expr.0);
                if !check_postcondition_provable(ctx, &substituted) {
                    return Err(TypeError::PostconditionViolation {
                        function: ctx.get_current_function().cloned().unwrap_or_default(),
                        postcondition: postcond.clone(),
                        return_type: ret_ty.clone(),
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
        Stmt::For {
            var,
            start,
            end,
            invariant,
            body,
        } => {
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

            let dummy_span = chumsky::span::SimpleSpan::new(0, 0);
            let lower_bound_expr = crate::common::ast::Expr::BinOp {
                op: crate::common::ast::BinOp::Gte,
                lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                rhs: Box::new((*start.clone()).clone()),
            };
            let upper_bound_expr = crate::common::ast::Expr::BinOp {
                op: crate::common::ast::BinOp::Lt,
                lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                rhs: Box::new((*end.clone()).clone()),
            };
            let loop_bounds_expr = crate::common::ast::Expr::BinOp {
                op: crate::common::ast::BinOp::And,
                lhs: Box::new((lower_bound_expr.clone(), dummy_span)),
                rhs: Box::new((upper_bound_expr.clone(), dummy_span)),
            };
            let loop_var_ty = IType::RefinedInt {
                base: Arc::new(IType::Int),
                prop: IProposition {
                    var: var.to_string(),
                    predicate: Arc::new((loop_bounds_expr, dummy_span)),
                },
            };

            // Add loop variable to context (immutable)
            let mut loop_ctx = ctx.with_immutable(var.to_string(), loop_var_ty.clone());

            // Add propositions about loop variable bounds: var >= start && var < end
            // This allows the SMT solver to prove array bounds within the loop

            // Create proposition: var >= start
            let lower_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((lower_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(lower_bound_prop);

            // Create proposition: var < end
            let upper_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((upper_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(upper_bound_prop);

            // Process invariant if present
            let tinvariant = if let Some(inv_expr) = invariant {
                // Type-check the invariant expression (must be bool)
                // Allow quantifiers in specification context
                let mut spec_ctx = loop_ctx.clone();
                spec_ctx.allow_quantifiers = true;
                let (_tinv, inv_ty) = synth_expr(&spec_ctx, inv_expr)?;

                if !is_subtype(&loop_ctx, &inv_ty, &IType::Bool) {
                    return Err(TypeError::TypeMismatch {
                        expected: IType::Bool,
                        found: inv_ty,
                        span: inv_expr.1,
                    });
                }

                // Step 3: Verify invariant holds at loop entry (base case)
                // Substitute start for var in invariant
                use crate::frontend::typechecker::helpers::substitute_expr_for_var;
                let inv_at_entry = substitute_expr_for_var(&inv_expr.0, var, &start.0);
                let inv_at_entry_prop = IProposition {
                    var: var.to_string(),
                    predicate: Arc::new((inv_at_entry, inv_expr.1)),
                };
                if !crate::frontend::typechecker::check_provable(ctx, &inv_at_entry_prop) {
                    return Err(TypeError::InvariantNotEstablished {
                        invariant_span: inv_expr.1,
                    });
                }

                // Add invariant as proposition to loop body context
                let inv_prop = IProposition {
                    var: var.to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                loop_ctx = loop_ctx.with_proposition(inv_prop);

                // Convert invariant to Constraint for lowering
                crate::backend::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            // Check body statements with invariant in context
            let (tbody, body_ctx) = check_stmts(&loop_ctx, &body.statements)?;

            // Step 4: Verify loop body preserves invariant (inductive step)
            if let Some(inv_expr) = invariant {
                use crate::frontend::typechecker::helpers::substitute_expr_for_var;
                let var_plus_1 = crate::common::ast::Expr::BinOp {
                    op: crate::common::ast::BinOp::Add,
                    lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                    rhs: Box::new((
                        crate::common::ast::Expr::Literal(crate::common::ast::Literal::Int(1)),
                        dummy_span,
                    )),
                };
                let inv_at_next = substitute_expr_for_var(&inv_expr.0, var, &var_plus_1);
                let inv_at_next_prop = IProposition {
                    var: var.to_string(),
                    predicate: Arc::new((inv_at_next, inv_expr.1)),
                };
                if !crate::frontend::typechecker::check_provable(&body_ctx, &inv_at_next_prop) {
                    return Err(TypeError::InvariantNotPreserved {
                        invariant_span: inv_expr.1,
                    });
                }
            }

            let tstmt = TStmt::For {
                var: var.to_string(),
                var_ty: loop_var_ty,
                start: Box::new(tstart),
                end: Box::new(tend),
                invariant: tinvariant,
                body: TBlock {
                    statements: tbody,
                    trailing_expr: None,
                },
            };

            // Step 5: Project invariant into post-loop context
            let post_ctx = if let Some(inv_expr) = invariant {
                use crate::frontend::typechecker::helpers::substitute_expr_for_var;
                let mut post = ctx.clone();

                // Invalidate pointwise props for arrays modified in loop body.
                // Use selective invalidation: only remove propositions for
                // indices that might overlap with the assigned indices.
                let modifications = collect_array_modifications(&body.statements);
                for (arr_name, indices) in &modifications {
                    // Add loop variable bounds to the SMT context so the
                    // selective-invalidation prover can use them when checking
                    // whether each pointwise prop's indices collide.
                    let mut smt_ctx = post.clone();
                    let lower_bound = crate::common::ast::Expr::BinOp {
                        op: crate::common::ast::BinOp::Gte,
                        lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                        rhs: Box::new((*start.clone()).clone()),
                    };
                    smt_ctx = smt_ctx.with_proposition(IProposition {
                        var: var.to_string(),
                        predicate: Arc::new((lower_bound, dummy_span)),
                    });
                    let upper_bound = crate::common::ast::Expr::BinOp {
                        op: crate::common::ast::BinOp::Lt,
                        lhs: Box::new((crate::common::ast::Expr::Variable(var), dummy_span)),
                        rhs: Box::new((*end.clone()).clone()),
                    };
                    smt_ctx = smt_ctx.with_proposition(IProposition {
                        var: var.to_string(),
                        predicate: Arc::new((upper_bound, dummy_span)),
                    });
                    post = invalidate_array_props_selectively(&smt_ctx, arr_name, indices);
                }

                // Substitute end for var in invariant
                let inv_at_end = substitute_expr_for_var(&inv_expr.0, var, &end.0);
                let inv_at_end_prop = IProposition {
                    var: var.to_string(),
                    predicate: Arc::new((inv_at_end, inv_expr.1)),
                };
                post = post.with_proposition(inv_at_end_prop);
                post
            } else {
                ctx.clone()
            };

            Ok(((tstmt, span), post_ctx))
        }

        // WHILE-LOOP: While loop with condition and optional invariant
        Stmt::While {
            condition,
            invariant,
            body,
        } => {
            let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);

            // Step 1: Synthesize condition, check it's bool
            let (tcond, cond_ty) = synth_expr(ctx, condition)?;
            if !is_subtype(ctx, &cond_ty, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: cond_ty,
                    span: condition.1,
                });
            }

            // Build loop context: start with current context + condition is true
            let cond_prop = IProposition {
                var: "_cond".to_string(),
                predicate: Arc::new(*condition.clone()),
            };
            let mut loop_ctx = ctx.with_proposition(cond_prop);

            // Step 2: Process invariant if present
            let tinvariant = if let Some(inv_expr) = invariant {
                // Type-check the invariant expression (must be bool)
                let mut spec_ctx = loop_ctx.clone();
                spec_ctx.allow_quantifiers = true;
                let (_tinv, inv_ty) = synth_expr(&spec_ctx, inv_expr)?;

                if !is_subtype(&loop_ctx, &inv_ty, &IType::Bool) {
                    return Err(TypeError::TypeMismatch {
                        expected: IType::Bool,
                        found: inv_ty,
                        span: inv_expr.1,
                    });
                }

                // Verify invariant holds at loop entry (base case)
                let inv_prop = IProposition {
                    var: "_inv".to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                if !crate::frontend::typechecker::check_provable(ctx, &inv_prop) {
                    return Err(TypeError::InvariantNotEstablished {
                        invariant_span: inv_expr.1,
                    });
                }

                // Add invariant as proposition to loop body context
                loop_ctx = loop_ctx.with_proposition(inv_prop);

                // Convert invariant to Constraint for lowering
                crate::backend::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            // Step 3: Check body statements
            let (tbody, body_ctx) = check_stmts(&loop_ctx, &body.statements)?;

            // Step 4: Verify loop body preserves invariant (inductive step)
            if let Some(inv_expr) = invariant {
                let inv_prop = IProposition {
                    var: "_inv".to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                if !crate::frontend::typechecker::check_provable(&body_ctx, &inv_prop) {
                    return Err(TypeError::InvariantNotPreserved {
                        invariant_span: inv_expr.1,
                    });
                }
            }

            let tstmt = TStmt::While {
                condition: Box::new(tcond),
                invariant: tinvariant,
                body: TBlock {
                    statements: tbody,
                    trailing_expr: None,
                },
            };

            // Step 5: Post-loop context
            // After the loop: invariant still holds (if present), condition is false
            let mut post_ctx = ctx.clone();

            // Invalidate array propositions modified in the loop body.
            // While loops don't have a simple index bound, so fall back to the
            // component-wise selective invalidation without added loop bounds;
            // if any component is symbolic and SMT can't prove distinctness,
            // the prop is dropped.
            let modifications = collect_array_modifications(&body.statements);
            for (arr_name, indices) in &modifications {
                post_ctx = invalidate_array_props_selectively(&post_ctx, arr_name, indices);
            }

            // Add invariant to post-loop context (it was preserved)
            if let Some(inv_expr) = invariant {
                let inv_prop = IProposition {
                    var: "_inv".to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                post_ctx = post_ctx.with_proposition(inv_prop);
            }

            // Add negated condition (loop exited because condition is false)
            let negated_cond = Expr::UnaryOp {
                op: crate::common::ast::UnaryOp::Not,
                cond: condition.clone(),
            };
            let neg_prop = IProposition {
                var: "_cond".to_string(),
                predicate: Arc::new((negated_cond, dummy_span)),
            };
            post_ctx = post_ctx.with_proposition(neg_prop);

            Ok(((tstmt, span), post_ctx))
        }

        // EXPR-STMT: Expression statement
        // Special handling for if-expressions to support context joining
        Stmt::Expr(expr) => {
            match &expr.0 {
                // If-expression as statement: handle context joining
                crate::common::ast::Expr::If {
                    cond,
                    then_block,
                    else_block,
                } => check_if_stmt(ctx, cond, then_block, else_block.as_ref(), span),
                // Other expressions: context unchanged
                _ => {
                    let (texpr, _) = synth_expr(ctx, expr)?;
                    let tstmt = TStmt::Expr(texpr);
                    Ok(((tstmt, span), ctx.clone()))
                }
            }
        }
    }
}

/// Collect (array_name, index_expr) pairs for arrays modified by index
/// assignment in a statement list. Recurses into if-blocks and nested for-loops.
fn collect_array_modifications<'src>(
    stmts: &[Spanned<Stmt<'src>>],
) -> Vec<(String, Vec<Expr<'src>>)> {
    let mut modifications = Vec::new();
    collect_array_modifications_inner(stmts, &mut modifications);
    modifications
}

fn collect_array_modifications_inner<'src>(
    stmts: &[Spanned<Stmt<'src>>],
    modifications: &mut Vec<(String, Vec<Expr<'src>>)>,
) {
    for stmt in stmts {
        match &stmt.0 {
            Stmt::Assignment { lhs, .. } => {
                if let Some((name, indices)) = extract_array_access(&lhs.0) {
                    modifications.push((name.to_string(), indices));
                }
            }
            Stmt::Expr(expr) => {
                if let Expr::If {
                    then_block,
                    else_block,
                    ..
                } = &expr.0
                {
                    collect_array_modifications_inner(&then_block.statements, modifications);
                    if let Some(else_stmts) = else_block {
                        collect_array_modifications_inner(&else_stmts.statements, modifications);
                    }
                }
            }
            Stmt::For { body, .. } => {
                collect_array_modifications_inner(&body.statements, modifications);
            }
            _ => {}
        }
    }
}

/// Check an if-statement with context joining
/// Returns the joined context after both branches merge
fn check_if_stmt<'src>(
    ctx: &TypingContext<'src>,
    cond: &Spanned<crate::common::ast::Expr<'src>>,
    then_block: &Block<'src>,
    else_block: Option<&Block<'src>>,
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

    // Check then block (statements + trailing_expr) and get final context
    let (tthen_block, then_final_ctx) = check_block_as_stmt(&then_ctx, then_block)?;

    // Check else block (if present) and get final context
    let (telse_block, else_final_ctx) = if let Some(else_blk) = else_block {
        let mut else_ctx = ctx.clone();
        if let Some(prop) = extract_proposition(&cond.0) {
            let neg_prop = negate_proposition(&prop);
            else_ctx = else_ctx.with_proposition(neg_prop);
        }

        let (typed_else, else_ctx_final) = check_block_as_stmt(&else_ctx, else_blk)?;
        (Some(typed_else), else_ctx_final)
    } else {
        // No else branch - context unchanged from original
        (None, ctx.clone())
    };

    // Join the contexts from both branches, passing the pre-branch context
    // so that semantic index matching can be used for array propositions.
    let joined_ctx =
        TypingContext::join_mutable_contexts_with_base(&then_final_ctx, &else_final_ctx, Some(ctx));

    let texpr = TExpr::If {
        cond: Box::new(tcond),
        then_block: tthen_block,
        else_block: telse_block,
        ty: IType::Unit,
    };

    let tstmt = TStmt::Expr((texpr, span));

    Ok(((tstmt, span), joined_ctx))
}

/// Check a block used as a statement body (e.g., then/else block of an if-statement).
/// Processes both the block's statements and its trailing_expr (if any).
/// When the trailing_expr is an if-else, it's checked as a nested if-statement
/// so that context joining works correctly for mutable variable updates.
fn check_block_as_stmt<'src>(
    ctx: &TypingContext<'src>,
    block: &Block<'src>,
) -> Result<(TBlock<'src>, TypingContext<'src>), TypeError<'src>> {
    let (typed_stmts, stmts_ctx) = check_stmts(ctx, &block.statements)?;

    // If there's a trailing expression, check it and thread context
    if let Some(trailing) = &block.trailing_expr {
        match &trailing.0 {
            // Trailing if-else: check as a statement for context joining
            crate::common::ast::Expr::If {
                cond,
                then_block,
                else_block,
            } => {
                let (if_tstmt, final_ctx) = check_if_stmt(
                    &stmts_ctx,
                    cond,
                    then_block,
                    else_block.as_ref(),
                    trailing.1,
                )?;
                // Wrap the if-statement result back into the block's statements
                let mut all_stmts = typed_stmts;
                all_stmts.push(if_tstmt);
                Ok((
                    TBlock {
                        statements: all_stmts,
                        trailing_expr: None,
                    },
                    final_ctx,
                ))
            }
            // Other trailing expressions: synthesize and include
            _ => {
                let (texpr, _ty) = synth_expr(&stmts_ctx, trailing)?;
                Ok((
                    TBlock {
                        statements: typed_stmts,
                        trailing_expr: Some(Box::new(texpr)),
                    },
                    stmts_ctx,
                ))
            }
        }
    } else {
        Ok((
            TBlock {
                statements: typed_stmts,
                trailing_expr: None,
            },
            stmts_ctx,
        ))
    }
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

        // Reject nested arrays with a symbolic inner dimension. The backend
        // needs a concrete stride to flatten multi-dim access, so only the
        // outermost dimension may be symbolic (e.g. `[[int; 5]; n]` is OK,
        // but `[[int; n]; 5]` is not).
        if has_symbolic_inner_dim(ty) {
            return Err(TypeError::UnsupportedFeature {
                feature: format!(
                    "symbolic inner array dimension in parameter `{}` (only the outermost dimension of a nested array may be symbolic)",
                    param.name
                ),
                span: spanned_param.1,
            });
        }

        func_ctx = func_ctx.with_immutable(param.name.to_string(), ty.clone());

        // For i64-typed parameters, add implicit range bounds [INT_MIN, INT_MAX].
        // Z3 models integers as unbounded; these axioms tell it the value is
        // representable as a 64-bit signed machine integer.
        if is_subtype(&func_ctx, ty, &IType::I64) {
            func_ctx = add_i64_range_props(func_ctx, param.name);
        }
        if is_subtype(&func_ctx, ty, &IType::U64) {
            func_ctx = add_u64_range_props(func_ctx, param.name);
        }

        // For array parameters with symbolic size, add axioms:
        //   len >= 0  (arrays cannot have negative length)
        //   len <= INT_MAX  (array length fits in a machine word)
        // These are load-bearing for proving i64 loop counter arithmetic.
        if let IType::Array {
            size: IValue::Symbolic(size_var),
            ..
        } = ty
        {
            func_ctx = add_array_length_axioms(func_ctx, size_var);
        }
    }
    // Set expected return type for checking return statements
    func_ctx = func_ctx.with_expected_return(return_type.clone());

    // Add precondition to context (if present) - this allows the function body
    // to assume the precondition holds
    if let Some(precond_expr) = &func_inner.precondition {
        let var_name = func_inner
            .parameters
            .first()
            .map(|p| p.0.name.to_string())
            .unwrap_or_else(|| "_".to_string());

        let precond_prop = crate::common::types::IProposition {
            var: var_name,
            predicate: Arc::new(precond_expr.clone()),
        };
        func_ctx = func_ctx.with_proposition(precond_prop);
    }

    // Store postcondition and current function name in context for return checking
    let postcondition = global_ctx
        .lookup_function(func_inner.name)
        .and_then(|sig| sig.postcondition.clone());

    if let Some(ref pc) = postcondition {
        func_ctx = func_ctx.with_postcondition(pc.clone());
    }
    func_ctx = func_ctx.with_current_function(func_inner.name.to_string());

    let (tbody, final_ctx) = check_stmts(&func_ctx, &func_inner.body.statements)?;

    // Check if body contains any return statements
    fn has_return_stmt(stmts: &[Spanned<TStmt>]) -> bool {
        for (stmt, _) in stmts {
            match stmt {
                TStmt::Return { .. } => return true,
                TStmt::For { body, .. } => {
                    if has_return_stmt(&body.statements) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    // Check return expression (if present)
    let treturn = if let Some(ret_expr) = &func_inner.body.trailing_expr {
        let (texpr, ret_ty) = synth_expr(&final_ctx, ret_expr)?;

        // Check return type matches signature
        if !check_expr_satisfies_refined(&final_ctx, &ret_expr.0, &ret_ty, &return_type) {
            return Err(TypeError::TypeMismatch {
                expected: return_type.clone(),
                found: ret_ty,
                span: ret_expr.1,
            });
        }

        // Verify postcondition at implicit return
        if let Some(ref postcond) = postcondition {
            let substituted = substitute_result_in_postcond(postcond, &ret_ty, &ret_expr.0);
            if !check_postcondition_provable(&final_ctx, &substituted) {
                return Err(TypeError::PostconditionViolation {
                    function: func_inner.name.to_string(),
                    postcondition: postcond.clone(),
                    return_type: ret_ty.clone(),
                    span: ret_expr.1,
                });
            }
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
    let tparams: Vec<TParameter> = func_inner
        .parameters
        .iter()
        .zip(param_types.iter())
        .map(|(spanned_param, ty)| TParameter {
            name: spanned_param.0.name.to_string(),
            ty: ty.clone(),
        })
        .collect();

    let precondition = global_ctx
        .lookup_function(func_inner.name)
        .and_then(|sig| sig.precondition.clone());

    let tfunc = TFunction {
        name: func_inner.name.to_string(),
        parameters: tparams,
        return_type,
        precondition,
        postcondition,
        body: TFunctionBody {
            statements: tbody,
            trailing_expr: treturn.map(Box::new),
        },
        span: *func_span,
    };

    Ok(tfunc)
}

/// Check an entire program
pub fn check_program<'src>(program: &Program<'src>) -> Result<TProgram<'src>, TypeError<'src>> {
    check_program_with_options(program, false, false)
}

pub fn check_program_bare_metal<'src>(
    program: &Program<'src>,
) -> Result<TProgram<'src>, TypeError<'src>> {
    check_program_with_options(program, true, false)
}

/// Check a program with an explicit `check_overflow` flag.
/// When `check_overflow` is true, arithmetic operations must be proved
/// to stay within `[INT_MIN, INT_MAX]` (Phase 3 and later).
pub fn check_program_with_overflow<'src>(
    program: &Program<'src>,
    bare_metal: bool,
    check_overflow: bool,
) -> Result<TProgram<'src>, TypeError<'src>> {
    check_program_with_options(program, bare_metal, check_overflow)
}

fn check_program_with_options<'src>(
    program: &Program<'src>,
    bare_metal: bool,
    check_overflow: bool,
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
        if !bare_metal && contains_array_type(&return_type) {
            return Err(TypeError::UnsupportedFeature {
                feature:
                    "returning arrays on the hosted target is not yet supported with function-local region allocation".to_string(),
                span: *func_span,
            });
        }

        // Convert precondition to IProposition
        let precondition = func.precondition.as_ref().map(|precond_expr| {
            // For preconditions, the bound variable is typically the first parameter
            // or we use a generic "_" if there are no parameters
            let var_name = func
                .parameters
                .first()
                .map(|p| p.0.name.to_string())
                .unwrap_or_else(|| "_".to_string());

            crate::common::types::IProposition {
                var: var_name,
                predicate: Arc::new(precond_expr.clone()),
            }
        });

        // Convert postcondition to IProposition
        // For postconditions, the bound variable is "result"
        let postcondition =
            func.postcondition
                .as_ref()
                .map(|postcond_expr| crate::common::types::IProposition {
                    var: "result".to_string(),
                    predicate: Arc::new(postcond_expr.clone()),
                });

        // Validate postcondition only references `result` and parameter names
        if let Some(postcond_expr) = &func.postcondition {
            let mut allowed: std::collections::HashSet<&str> = std::collections::HashSet::new();
            allowed.insert("result");
            for p in &func.parameters {
                allowed.insert(p.0.name);
            }
            if let Some(bad_var) = find_invalid_free_var(&postcond_expr.0, &allowed) {
                return Err(TypeError::InvalidPostconditionVariable {
                    variable: bad_var.to_string(),
                    function: func.name.to_string(),
                    span: postcond_expr.1,
                });
            }
        }

        let sig = FunctionSignature {
            name: func.name.to_string(),
            parameters,
            return_type,
            precondition,
            postcondition,
            span: *func_span,
        };

        signatures.insert(func.name.to_string(), sig);
    }

    // Register runtime intrinsic signatures
    let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);

    // Linux-only intrinsics (use syscalls — not available on bare metal)
    if !bare_metal {
        signatures.insert(
            "print_int".into(),
            FunctionSignature {
                name: "print_int".into(),
                parameters: vec![("n".into(), IType::Int)],
                return_type: IType::Unit,
                precondition: None,
                postcondition: None,
                span: dummy_span,
            },
        );
        signatures.insert(
            "print_char".into(),
            FunctionSignature {
                name: "print_char".into(),
                parameters: vec![("c".into(), IType::Int)],
                return_type: IType::Unit,
                precondition: None,
                postcondition: None,
                span: dummy_span,
            },
        );
        signatures.insert(
            "read_int".into(),
            FunctionSignature {
                name: "read_int".into(),
                parameters: vec![],
                return_type: IType::Int,
                precondition: None,
                postcondition: None,
                span: dummy_span,
            },
        );
    } // end if !bare_metal

    // Port I/O intrinsics (available on both Linux and bare metal)
    signatures.insert(
        "port_in".into(),
        FunctionSignature {
            name: "port_in".into(),
            parameters: vec![("port".into(), IType::Int)],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            span: dummy_span,
        },
    );
    signatures.insert(
        "port_out".into(),
        FunctionSignature {
            name: "port_out".into(),
            parameters: vec![("port".into(), IType::Int), ("value".into(), IType::Int)],
            return_type: IType::Unit,
            precondition: None,
            postcondition: None,
            span: dummy_span,
        },
    );

    let mut global_ctx = TypingContext::with_functions(signatures);
    global_ctx.check_overflow = check_overflow;

    // Process constant declarations — add as immutable singleton bindings
    for (constant, _span) in &program.constants {
        let ty = ast_type_to_itype(&constant.ty)?;
        // Evaluate constant value to a singleton
        let (_, value_ty) = synth_expr(&global_ctx, &constant.value)?;
        // Constants must be compile-time known (singleton)
        if !is_subtype(&global_ctx, &value_ty, &ty) {
            return Err(TypeError::TypeMismatch {
                expected: ty,
                found: value_ty,
                span: constant.value.1,
            });
        }
        global_ctx = global_ctx.with_immutable(constant.name.to_string(), value_ty);
    }

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
fn add_i64_range_props<'src>(ctx: TypingContext<'src>, var_name: &'src str) -> TypingContext<'src> {
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
fn add_u64_range_props<'src>(ctx: TypingContext<'src>, var_name: &'src str) -> TypingContext<'src> {
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
fn add_array_length_axioms<'src>(ctx: TypingContext<'src>, size_var: &str) -> TypingContext<'src> {
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
fn substitute_result_in_postcond<'src>(
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
fn has_symbolic_inner_dim<'src>(ty: &IType<'src>) -> bool {
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

fn contains_array_type<'src>(ty: &IType<'src>) -> bool {
    match ty {
        IType::Array { .. } => true,
        IType::RefinedInt { base, .. } => contains_array_type(base),
        _ => false,
    }
}

/// Substitute a variable with a literal integer in an expression
pub(super) fn substitute_var_with_literal<'src>(
    expr: &crate::common::ast::Expr<'src>,
    var_name: &str,
    value: i128,
) -> crate::common::ast::Expr<'src> {
    use crate::common::ast::{Expr, Literal};

    match expr {
        Expr::Variable(name) if *name == var_name => Expr::Literal(Literal::Int(value)),
        Expr::Variable(_) => expr.clone(),
        Expr::Literal(_) => expr.clone(),
        Expr::Error => Expr::Error,
        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((substitute_var_with_literal(&lhs.0, var_name, value), lhs.1)),
            rhs: Box::new((substitute_var_with_literal(&rhs.0, var_name, value), rhs.1)),
        },
        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((
                substitute_var_with_literal(&cond.0, var_name, value),
                cond.1,
            )),
        },
        Expr::Call { func_name, args } => Expr::Call {
            func_name,
            args: (
                args.0
                    .iter()
                    .map(|(arg, span)| (substitute_var_with_literal(arg, var_name, value), *span))
                    .collect(),
                args.1,
            ),
        },
        Expr::Index { base, index } => Expr::Index {
            base: Box::new((
                substitute_var_with_literal(&base.0, var_name, value),
                base.1,
            )),
            index: Box::new((
                substitute_var_with_literal(&index.0, var_name, value),
                index.1,
            )),
        },
        Expr::ArrayInit { value: v, length } => Expr::ArrayInit {
            value: Box::new((substitute_var_with_literal(&v.0, var_name, value), v.1)),
            length: Box::new((
                substitute_var_with_literal(&length.0, var_name, value),
                length.1,
            )),
        },
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            Expr::If {
                cond: Box::new((
                    substitute_var_with_literal(&cond.0, var_name, value),
                    cond.1,
                )),
                then_block: Block {
                    statements: then_block
                        .statements
                        .iter()
                        .map(|(stmt, span)| (substitute_var_in_stmt(stmt, var_name, value), *span))
                        .collect(),
                    trailing_expr: then_block.trailing_expr.as_ref().map(|e| {
                        Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))
                    }),
                },
                else_block: else_block.as_ref().map(|block| Block {
                    statements: block
                        .statements
                        .iter()
                        .map(|(stmt, span)| (substitute_var_in_stmt(stmt, var_name, value), *span))
                        .collect(),
                    trailing_expr: block.trailing_expr.as_ref().map(|e| {
                        Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))
                    }),
                }),
            }
        }
        Expr::Forall {
            var,
            start,
            end,
            body,
        } => {
            if *var == var_name {
                expr.clone()
            } else {
                Expr::Forall {
                    var,
                    start: Box::new((
                        substitute_var_with_literal(&start.0, var_name, value),
                        start.1,
                    )),
                    end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
                    body: Box::new((
                        substitute_var_with_literal(&body.0, var_name, value),
                        body.1,
                    )),
                }
            }
        }
        Expr::Exists {
            var,
            start,
            end,
            body,
        } => {
            if *var == var_name {
                expr.clone()
            } else {
                Expr::Exists {
                    var,
                    start: Box::new((
                        substitute_var_with_literal(&start.0, var_name, value),
                        start.1,
                    )),
                    end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
                    body: Box::new((
                        substitute_var_with_literal(&body.0, var_name, value),
                        body.1,
                    )),
                }
            }
        }
    }
}

/// Substitute a variable with a literal integer in a statement
fn substitute_var_in_stmt<'src>(
    stmt: &crate::common::ast::Stmt<'src>,
    var_name: &str,
    value: i128,
) -> crate::common::ast::Stmt<'src> {
    use crate::common::ast::Stmt;

    match stmt {
        Stmt::Let {
            is_mut,
            name,
            ty,
            value: v,
        } => Stmt::Let {
            is_mut: *is_mut,
            name,
            ty: ty.clone(),
            value: (substitute_var_with_literal(&v.0, var_name, value), v.1),
        },
        Stmt::Assignment { lhs, rhs } => Stmt::Assignment {
            lhs: (substitute_var_with_literal(&lhs.0, var_name, value), lhs.1),
            rhs: (substitute_var_with_literal(&rhs.0, var_name, value), rhs.1),
        },
        Stmt::Return { expr } => Stmt::Return {
            expr: Box::new((
                substitute_var_with_literal(&expr.0, var_name, value),
                expr.1,
            )),
        },
        Stmt::Expr(e) => Stmt::Expr((substitute_var_with_literal(&e.0, var_name, value), e.1)),
        Stmt::For {
            var,
            start,
            end,
            invariant,
            body,
        } => Stmt::For {
            var,
            start: Box::new((
                substitute_var_with_literal(&start.0, var_name, value),
                start.1,
            )),
            end: Box::new((substitute_var_with_literal(&end.0, var_name, value), end.1)),
            invariant: invariant
                .as_ref()
                .map(|(inv, span)| (substitute_var_with_literal(inv, var_name, value), *span)),
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|(s, span)| (substitute_var_in_stmt(s, var_name, value), *span))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))),
            },
        },
        Stmt::While {
            condition,
            invariant,
            body,
        } => Stmt::While {
            condition: Box::new((
                substitute_var_with_literal(&condition.0, var_name, value),
                condition.1,
            )),
            invariant: invariant
                .as_ref()
                .map(|(inv, span)| (substitute_var_with_literal(inv, var_name, value), *span)),
            body: Block {
                statements: body
                    .statements
                    .iter()
                    .map(|(s, span)| (substitute_var_in_stmt(s, var_name, value), *span))
                    .collect(),
                trailing_expr: body
                    .trailing_expr
                    .as_ref()
                    .map(|e| Box::new((substitute_var_with_literal(&e.0, var_name, value), e.1))),
            },
        },
    }
}

/// Check if an expression of type `expr_ty` can satisfy a refined target type
/// by substituting the expression into the refinement predicate and checking provability.
/// Falls back when `is_subtype` alone cannot prove `int <: {v: int | P}`.
fn check_expr_satisfies_refined<'src>(
    ctx: &TypingContext<'src>,
    expr: &crate::common::ast::Expr<'src>,
    expr_ty: &IType<'src>,
    target: &IType<'src>,
) -> bool {
    if is_subtype(ctx, expr_ty, target) {
        return true;
    }
    if let IType::RefinedInt { base, prop } = target
        && is_subtype(ctx, expr_ty, base)
    {
        use crate::frontend::typechecker::helpers::substitute_expr_for_var;
        let substituted = substitute_expr_for_var(&prop.predicate.0, &prop.var, expr);
        let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);
        let goal = IProposition {
            var: prop.var.clone(),
            predicate: Arc::new((substituted, dummy_span)),
        };
        return crate::frontend::typechecker::smt::SmtOracle::new().is_provable(ctx, &goal);
    }
    false
}

/// Selectively invalidate array element propositions when the assigned index
/// is symbolic. For each pointwise proposition `arr[k] == v`, use SMT to check
/// whether `assigned_index != k` is provable. If so, the proposition is safe to
/// keep. Quantified propositions over the array are always removed.
/// Walk a nested `Index` chain to extract (root_array_name, indices_outer_to_inner).
/// Returns None if the chain doesn't bottom out in a plain `Variable`.
fn extract_array_access<'src>(expr: &Expr<'src>) -> Option<(&'src str, Vec<Expr<'src>>)> {
    let mut indices: Vec<Expr<'src>> = Vec::new();
    let mut cur = expr;
    loop {
        match cur {
            Expr::Index { base, index } => {
                indices.push(index.0.clone());
                cur = &base.0;
            }
            Expr::Variable(name) => {
                indices.reverse();
                return Some((name, indices));
            }
            _ => return None,
        }
    }
}

fn invalidate_array_props_selectively<'src>(
    ctx: &TypingContext<'src>,
    arr_name: &str,
    assigned_indices: &[Expr<'src>],
) -> TypingContext<'src> {
    use crate::common::ast::BinOp;

    let dummy_span = chumsky::span::SimpleSpan::new(0, 0);
    let arr_name_owned = arr_name.to_string();
    let assigned_indices: Vec<Expr<'src>> = assigned_indices.to_vec();

    ctx.retain_propositions(|prop| {
        if prop.var != arr_name_owned {
            return true;
        }
        match &prop.predicate.0 {
            // Quantified propositions are always invalidated
            Expr::Forall { .. } | Expr::Exists { .. } => false,
            // Pointwise: keep if we can prove the slots don't collide
            Expr::BinOp {
                op: BinOp::Eq, lhs, ..
            } => {
                let Some((root, prop_indices)) = extract_array_access(&lhs.0) else {
                    return true;
                };
                if root != arr_name_owned.as_str() || prop_indices.len() != assigned_indices.len() {
                    return true;
                }

                // Concrete fast path: all concretely-known components equal → same slot (drop);
                // any concretely-known component differs → distinct slot (keep).
                let mut all_concrete_equal = true;
                for (p, a) in prop_indices.iter().zip(assigned_indices.iter()) {
                    match (ctx.resolve_expr_to_int(p), ctx.resolve_expr_to_int(a)) {
                        (Some(pi), Some(ai)) if pi != ai => return true,
                        (Some(_), Some(_)) => {}
                        _ => all_concrete_equal = false,
                    }
                }
                if all_concrete_equal {
                    return false;
                }

                // SMT goal: disjunction of component inequalities. If provable,
                // the slots are guaranteed distinct and the proposition survives.
                let mut goal_expr: Option<Expr<'src>> = None;
                for (p, a) in prop_indices.iter().zip(assigned_indices.iter()) {
                    let ne = Expr::BinOp {
                        op: BinOp::NotEq,
                        lhs: Box::new((a.clone(), dummy_span)),
                        rhs: Box::new((p.clone(), dummy_span)),
                    };
                    goal_expr = Some(match goal_expr.take() {
                        None => ne,
                        Some(acc) => Expr::BinOp {
                            op: BinOp::Or,
                            lhs: Box::new((acc, dummy_span)),
                            rhs: Box::new((ne, dummy_span)),
                        },
                    });
                }
                let goal = IProposition {
                    var: arr_name_owned.clone(),
                    predicate: Arc::new((goal_expr.unwrap(), dummy_span)),
                };
                crate::frontend::typechecker::smt::check_provable(ctx, &goal)
            }
            _ => true,
        }
    })
}

/// If the value expression is a function call with a postcondition,
/// produce a proposition with `result` renamed to the binding variable
/// and parameter names substituted with the actual argument values.
fn postcondition_for_call<'src>(
    ctx: &TypingContext<'src>,
    binding_name: &str,
    value_expr: &crate::common::ast::Expr<'src>,
) -> Option<IProposition<'src>> {
    use crate::frontend::typechecker::helpers::rename_expr_var;
    use crate::frontend::typechecker::synthesize::substitute_args_in_prop;

    if let Expr::Call { func_name, args } = value_expr
        && let Some(sig) = ctx.lookup_function(func_name)
        && let Some(ref postcond) = sig.postcondition
    {
        // Rename `result` → binding name
        let binding_leaked: &'src str = Box::leak(binding_name.to_string().into_boxed_str());
        let renamed = rename_expr_var(&postcond.predicate.0, "result", binding_leaked);
        let renamed_prop = IProposition {
            var: binding_name.to_string(),
            predicate: Arc::new((renamed, postcond.predicate.1)),
        };

        // Substitute parameter names with actual argument values/variables
        let arg_exprs: Vec<&Expr> = args.0.iter().map(|a| &a.0).collect();
        let arg_types: Vec<IType> = args
            .0
            .iter()
            .filter_map(|a| synth_expr(ctx, a).ok().map(|(_, ty)| ty))
            .collect();

        if arg_types.len() == sig.parameters.len() {
            return Some(substitute_args_in_prop(
                &renamed_prop,
                &sig.parameters,
                &arg_types,
                &arg_exprs,
            ));
        }

        return Some(renamed_prop);
    }
    None
}

/// Find the first free variable in an expression that is not in the allowed set.
/// Returns None if all free variables are allowed.
/// Respects quantifier-bound variables (forall/exists introduce scoped bindings).
fn find_invalid_free_var<'src>(
    expr: &crate::common::ast::Expr<'src>,
    allowed: &std::collections::HashSet<&str>,
) -> Option<&'src str> {
    use crate::common::ast::Expr;

    match expr {
        Expr::Variable(name) => {
            if allowed.contains(name) {
                None
            } else {
                Some(name)
            }
        }
        Expr::Literal(_) | Expr::Error => None,
        Expr::BinOp { lhs, rhs, .. } => find_invalid_free_var(&lhs.0, allowed)
            .or_else(|| find_invalid_free_var(&rhs.0, allowed)),
        Expr::UnaryOp { cond, .. } => find_invalid_free_var(&cond.0, allowed),
        Expr::Index { base, index } => find_invalid_free_var(&base.0, allowed)
            .or_else(|| find_invalid_free_var(&index.0, allowed)),
        Expr::Call { args, .. } => args
            .0
            .iter()
            .find_map(|(arg, _)| find_invalid_free_var(arg, allowed)),
        Expr::ArrayInit { value, length } => find_invalid_free_var(&value.0, allowed)
            .or_else(|| find_invalid_free_var(&length.0, allowed)),
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            if let Some(v) = find_invalid_free_var(&cond.0, allowed) {
                return Some(v);
            }
            for (stmt, _) in &then_block.statements {
                if let Some(v) = find_invalid_free_var_in_stmt(stmt, allowed) {
                    return Some(v);
                }
            }
            if let Some(else_block) = else_block {
                for (stmt, _) in &else_block.statements {
                    if let Some(v) = find_invalid_free_var_in_stmt(stmt, allowed) {
                        return Some(v);
                    }
                }
            }
            None
        }
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
            if let Some(v) = find_invalid_free_var(&start.0, allowed) {
                return Some(v);
            }
            if let Some(v) = find_invalid_free_var(&end.0, allowed) {
                return Some(v);
            }
            let mut inner_allowed = allowed.clone();
            inner_allowed.insert(var);
            find_invalid_free_var(&body.0, &inner_allowed)
        }
    }
}

fn find_invalid_free_var_in_stmt<'src>(
    stmt: &crate::common::ast::Stmt<'src>,
    allowed: &std::collections::HashSet<&str>,
) -> Option<&'src str> {
    use crate::common::ast::Stmt;

    match stmt {
        Stmt::Let { value, .. } => find_invalid_free_var(&value.0, allowed),
        Stmt::Assignment { lhs, rhs } => find_invalid_free_var(&lhs.0, allowed)
            .or_else(|| find_invalid_free_var(&rhs.0, allowed)),
        Stmt::Return { expr } => find_invalid_free_var(&expr.0, allowed),
        Stmt::Expr(e) => find_invalid_free_var(&e.0, allowed),
        Stmt::For {
            start, end, body, ..
        } => {
            if let Some(v) = find_invalid_free_var(&start.0, allowed) {
                return Some(v);
            }
            if let Some(v) = find_invalid_free_var(&end.0, allowed) {
                return Some(v);
            }
            for (s, _) in &body.statements {
                if let Some(v) = find_invalid_free_var_in_stmt(s, allowed) {
                    return Some(v);
                }
            }
            None
        }
        Stmt::While {
            condition, body, ..
        } => {
            if let Some(v) = find_invalid_free_var(&condition.0, allowed) {
                return Some(v);
            }
            for (s, _) in &body.statements {
                if let Some(v) = find_invalid_free_var_in_stmt(s, allowed) {
                    return Some(v);
                }
            }
            None
        }
    }
}

/// Check if a postcondition can be proven given the current context
fn check_postcondition_provable<'src>(
    ctx: &TypingContext<'src>,
    postcond: &IProposition<'src>,
) -> bool {
    // Use the existing SMT oracle to check if the postcondition is provable
    crate::frontend::typechecker::smt::check_provable(ctx, postcond)
}
