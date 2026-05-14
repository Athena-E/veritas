// Statement and program type checking

use crate::common::ast::{Block, Expr, Function, Program, Stmt};
use crate::common::ownership::{OwnershipMode, ParameterKind};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TBlock, TExpr, TFunction, TFunctionBody, TParameter, TProgram, TStmt};
use crate::common::types::{FunctionSignature, IProposition, IType, IValue};
use crate::frontend::typechecker::array_props::{
    collect_array_modifications, extract_array_access, invalidate_array_props_selectively,
};
use crate::frontend::typechecker::ownership::{
    apply_call_argument_moves, apply_whole_value_move, explicit_transfer_ownership,
    expr_depends_on_region_local_array, infer_parameter_kinds,
};
use crate::frontend::typechecker::postconditions::{
    check_expr_satisfies_refined, check_postcondition_provable, find_invalid_free_var,
    postcondition_for_call,
};
pub(super) use crate::frontend::typechecker::substitution::substitute_var_with_literal;
use crate::frontend::typechecker::type_utils::{
    add_array_length_axioms, add_i64_range_props, add_u64_range_props,
    array_contains_reference_type, ast_type_to_itype, contains_array_type, has_symbolic_inner_dim,
    substitute_result_in_postcond,
};
use crate::frontend::typechecker::{TypeError, TypingContext, is_subtype, synth_expr};
use im::HashMap;
use std::sync::Arc;

fn borrow_binding_target_name<'src>(
    expr: &Expr<'src>,
) -> Option<(&'src str, crate::common::ownership::BorrowKind)> {
    match expr {
        Expr::Borrow { kind, expr } => match &expr.0 {
            Expr::Variable(name) => Some((name, *kind)),
            _ => None,
        },
        _ => None,
    }
}

fn reject_shadowing_borrowed_owner<'src>(
    ctx: &TypingContext<'src>,
    name: &str,
    span: Span,
) -> Result<(), TypeError<'src>> {
    if ctx.is_borrowed(name) && ctx.lookup_borrow_binding(name).is_none() {
        return Err(TypeError::BorrowConflict {
            name: name.to_string(),
            reason: "cannot shadow a binding while it is borrowed".to_string(),
            span,
        });
    }
    Ok(())
}

fn reject_mutating_borrowed_owner<'src>(
    ctx: &TypingContext<'src>,
    name: &str,
    span: Span,
) -> Result<(), TypeError<'src>> {
    if ctx.is_borrowed(name) {
        return Err(TypeError::BorrowConflict {
            name: name.to_string(),
            reason: "cannot mutate a binding while it is borrowed".to_string(),
            span,
        });
    }
    Ok(())
}

fn is_reference_type<'src>(ty: &IType<'src>) -> bool {
    matches!(ty, IType::Ref(_) | IType::RefMut(_))
}

fn array_base_type<'src>(ty: &IType<'src>) -> Option<IType<'src>> {
    match ty {
        IType::Array { .. } => Some(ty.clone()),
        IType::Ref(inner) | IType::RefMut(inner) => match inner.as_ref() {
            IType::Array { .. } => Some(inner.as_ref().clone()),
            _ => None,
        },
        _ => None,
    }
}

fn reject_region_borrow_escape<'src>(
    ctx: &TypingContext<'src>,
    binding_name: &str,
    binding_ty: &IType<'src>,
    value: &Expr<'src>,
    span: Span,
) -> Result<(), TypeError<'src>> {
    if ctx.bare_metal || !ctx.in_region_scope() || !is_reference_type(binding_ty) {
        return Ok(());
    }

    if !matches!(value, Expr::Borrow { .. }) {
        return Ok(());
    }

    if expr_depends_on_region_local_array(ctx, value) && !ctx.is_region_scoped_borrow(binding_name)
    {
        return Err(TypeError::UnsupportedFeature {
            feature:
                "borrowing region-local arrays into bindings that survive a hosted region block is not yet supported"
                    .to_string(),
            span,
        });
    }

    Ok(())
}

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
        Expr::Borrow {
            kind,
            expr: borrowed,
        } => {
            let (new_borrowed, resolved) = resolve_array_reads_in_expr(ctx, &borrowed.0);
            if resolved {
                (
                    Expr::Borrow {
                        kind: *kind,
                        expr: Box::new((new_borrowed, borrowed.1)),
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
            reject_shadowing_borrowed_owner(ctx, name, span)?;

            // Synthesize type of initializer
            let (tvalue, value_ty) = synth_expr(ctx, value)?;

            // Convert annotated type to semantic type
            let ann_ty = ast_type_to_itype(ty)?;

            if array_contains_reference_type(&ann_ty) {
                return Err(TypeError::UnsupportedFeature {
                    feature: "storing references inside arrays is not yet supported".to_string(),
                    span: ty.1,
                });
            }

            // Check value type is subtype of annotation
            if !is_subtype(ctx, &value_ty, &ann_ty) {
                return Err(TypeError::TypeMismatch {
                    expected: ann_ty,
                    found: value_ty,
                    span: value.1,
                });
            }

            let move_ctx = apply_whole_value_move(ctx, &value.0, &value_ty, value.1)?;

            // Add to context with the synthesized type (more precise)
            let mut new_ctx = move_ctx.with_immutable(name.to_string(), value_ty.clone());
            if matches!(ann_ty, IType::Ref(_) | IType::RefMut(_))
                && !matches!(value.0, Expr::Borrow { .. })
            {
                return Err(TypeError::UnsupportedFeature {
                    feature: "copying reference values is not yet supported".to_string(),
                    span: value.1,
                });
            }
            if let Some((owner_name, kind)) = borrow_binding_target_name(&value.0) {
                new_ctx = new_ctx.add_borrow_binding(name, owner_name, kind);
            }
            if !ctx.bare_metal && ctx.in_region_scope() && is_reference_type(&ann_ty) {
                new_ctx = new_ctx.mark_region_scoped_borrow(name);
            }
            reject_region_borrow_escape(&new_ctx, name, &ann_ty, &value.0, value.1)?;
            if !ctx.bare_metal && ctx.in_region_scope() && contains_array_type(&value_ty) {
                new_ctx = new_ctx.mark_region_scoped_array(name);
            }
            if !ctx.bare_metal
                && ctx.in_region_scope()
                && contains_array_type(&value_ty)
                && expr_depends_on_region_local_array(ctx, &value.0)
            {
                new_ctx = new_ctx.mark_region_local_array(name);
            }

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
                checked_ty: value_ty.clone(),
                ownership: explicit_transfer_ownership(ctx.bare_metal, &value.0, &value_ty),
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
            reject_shadowing_borrowed_owner(ctx, name, span)?;

            // Synthesize type of initializer
            let (tvalue, value_ty) = synth_expr(ctx, value)?;

            // Convert annotated type to semantic type
            let ann_ty = ast_type_to_itype(ty)?;

            if array_contains_reference_type(&ann_ty) {
                return Err(TypeError::UnsupportedFeature {
                    feature: "storing references inside arrays is not yet supported".to_string(),
                    span: ty.1,
                });
            }

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
            let move_ctx = apply_whole_value_move(ctx, &value.0, &value_ty, value.1)?;

            let mut new_ctx =
                move_ctx.with_mutable(name.to_string(), current_ty.clone(), master_ty);
            if matches!(ann_ty, IType::Ref(_) | IType::RefMut(_))
                && !matches!(value.0, Expr::Borrow { .. })
            {
                return Err(TypeError::UnsupportedFeature {
                    feature: "copying reference values is not yet supported".to_string(),
                    span: value.1,
                });
            }
            if let Some((owner_name, kind)) = borrow_binding_target_name(&value.0) {
                new_ctx = new_ctx.add_borrow_binding(name, owner_name, kind);
            }
            if !ctx.bare_metal && ctx.in_region_scope() && is_reference_type(&ann_ty) {
                new_ctx = new_ctx.mark_region_scoped_borrow(name);
            }
            reject_region_borrow_escape(&new_ctx, name, &ann_ty, &value.0, value.1)?;
            if !ctx.bare_metal && ctx.in_region_scope() && contains_array_type(&current_ty) {
                new_ctx = new_ctx.mark_region_scoped_array(name);
            }
            if !ctx.bare_metal
                && ctx.in_region_scope()
                && contains_array_type(&current_ty)
                && expr_depends_on_region_local_array(ctx, &value.0)
            {
                new_ctx = new_ctx.mark_region_local_array(name);
            }

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
                ownership: explicit_transfer_ownership(ctx.bare_metal, &value.0, &value_ty),
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
                    if ctx.lookup_borrow_binding(var_name).is_none() {
                        reject_mutating_borrowed_owner(ctx, var_name, span)?;
                    }

                    let rhs_depends_on_region_local = !ctx.bare_metal
                        && ctx.in_region_scope()
                        && contains_array_type(&rhs_ty)
                        && expr_depends_on_region_local_array(ctx, &rhs.0);

                    if !ctx.bare_metal
                        && ctx.in_region_scope()
                        && contains_array_type(&rhs_ty)
                        && rhs_depends_on_region_local
                        && !ctx.is_region_local_array(var_name)
                        && !ctx.is_region_scoped_array(var_name)
                    {
                        return Err(TypeError::UnsupportedFeature {
                            feature:
                                "assigning whole arrays inside a hosted region block is not yet supported"
                                    .to_string(),
                            span: rhs.1,
                        });
                    }

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

                    if !ctx.bare_metal
                        && ctx.in_region_scope()
                        && is_reference_type(master_base)
                        && !ctx.is_region_scoped_borrow(var_name)
                    {
                        return Err(TypeError::UnsupportedFeature {
                            feature:
                                "reassigning reference bindings that outlive a hosted region block is not yet supported"
                                    .to_string(),
                            span: rhs.1,
                        });
                    }

                    if !check_expr_satisfies_refined(ctx, &rhs.0, &rhs_ty, master_base) {
                        return Err(TypeError::TypeMismatch {
                            expected: master_base.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

                    // Update mutable variable's current type
                    let move_ctx = if matches!(&rhs.0, Expr::Variable(rhs_name) if *rhs_name == *var_name)
                    {
                        apply_call_argument_moves(ctx, &rhs.0)?
                    } else {
                        apply_whole_value_move(ctx, &rhs.0, &rhs_ty, rhs.1)?
                    };

                    let mut new_ctx = move_ctx
                        .with_mutable_update(var_name, rhs_ty.clone())
                        .map_err(|e| TypeError::InvalidAssignment {
                            variable: var_name.to_string(),
                            reason: e,
                            span,
                        })?;

                    new_ctx = new_ctx.release_borrow_binding(var_name);
                    reject_region_borrow_escape(ctx, var_name, master_base, &rhs.0, rhs.1)?;
                    if matches!(master_base, IType::Ref(_) | IType::RefMut(_))
                        && !matches!(rhs.0, Expr::Borrow { .. })
                    {
                        return Err(TypeError::UnsupportedFeature {
                            feature: "copying reference values is not yet supported".to_string(),
                            span: rhs.1,
                        });
                    }
                    if let Some((owner_name, kind)) = borrow_binding_target_name(&rhs.0) {
                        new_ctx = new_ctx.add_borrow_binding(var_name, owner_name, kind);
                    }

                    if !ctx.bare_metal && ctx.in_region_scope() && contains_array_type(&rhs_ty) {
                        if rhs_depends_on_region_local {
                            new_ctx = new_ctx.mark_region_local_array(var_name);
                        } else if ctx.is_region_scoped_array(var_name) {
                            new_ctx = new_ctx.clear_region_local_array(var_name);
                        }
                    }

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
                        ownership: if matches!(&rhs.0, Expr::Variable(rhs_name) if *rhs_name == *var_name)
                        {
                            OwnershipMode::Plain
                        } else {
                            explicit_transfer_ownership(ctx.bare_metal, &rhs.0, &rhs_ty)
                        },
                    };

                    Ok(((tstmt, span), new_ctx))
                }

                // Array indexing assignment
                crate::common::ast::Expr::Index { base, index } => {
                    if let Expr::Variable(arr_name) = &base.0 {
                        reject_mutating_borrowed_owner(ctx, arr_name, span)?;
                    }

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

                    // Check array/reference-to-array type and extract element type
                    let (elem_ty, array_size) = match &base_ty {
                        IType::Ref(_) => {
                            let name = if let Expr::Variable(name) = &base.0 {
                                name.to_string()
                            } else {
                                "<ref>".to_string()
                            };
                            return Err(TypeError::BorrowConflict {
                                name,
                                reason: "cannot assign through a shared reference".to_string(),
                                span: base.1,
                            });
                        }
                        _ => match array_base_type(&base_ty) {
                            Some(IType::Array { element_type, size }) => {
                                (element_type.as_ref().clone(), size.clone())
                            }
                            Some(other) => {
                                return Err(TypeError::NotAnArray {
                                    found: other,
                                    span: base.1,
                                });
                            }
                            None => {
                                return Err(TypeError::NotAnArray {
                                    found: base_ty,
                                    span: base.1,
                                });
                            }
                        },
                    };

                    // Check array bounds using the actual index expression
                    crate::frontend::typechecker::check_array_bounds_expr(
                        ctx,
                        &index.0,
                        &index_ty,
                        &array_size,
                        &base_ty,
                        index.1,
                    )?;

                    // Check value type matches element type
                    if !is_subtype(ctx, &rhs_ty, &elem_ty) {
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

                    let move_ctx = apply_whole_value_move(ctx, &rhs.0, &rhs_ty, rhs.1)?;

                    let tstmt = TStmt::Assignment {
                        lhs: (tlhs_expr, lhs.1),
                        rhs: trhs,
                        ownership: OwnershipMode::Plain,
                    };

                    // Add pointwise proposition: arr[i]...[k] == rhs.
                    // Supports nested index chains (2D+) by extracting the root
                    // array name and the full index tuple.
                    let mut new_ctx = move_ctx.clone();

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

                crate::common::ast::Expr::UnaryOp {
                    op: crate::common::ast::UnaryOp::Deref,
                    ..
                } => {
                    let (tlhs, _lhs_ty) = synth_expr(ctx, lhs)?;
                    let (owner_name, kind, pointee_ty, ref_name) = match &tlhs.0 {
                        TExpr::Deref {
                            owner, kind, ty, ..
                        } => {
                            let ref_name = match &lhs.0 {
                                crate::common::ast::Expr::UnaryOp { cond, .. } => match &cond.0 {
                                    crate::common::ast::Expr::Variable(name) => {
                                        Some((*name).to_string())
                                    }
                                    _ => None,
                                },
                                _ => None,
                            };
                            (owner.clone(), *kind, ty.clone(), ref_name)
                        }
                        _ => {
                            return Err(TypeError::UnsupportedFeature {
                                feature:
                                    "dereference assignment target must be a reference binding"
                                        .to_string(),
                                span: lhs.1,
                            });
                        }
                    };

                    if kind.is_shared() {
                        return Err(TypeError::BorrowConflict {
                            name: owner_name
                                .clone()
                                .or(ref_name)
                                .unwrap_or_else(|| "<ref>".to_string()),
                            reason: "cannot assign through a shared reference".to_string(),
                            span: lhs.1,
                        });
                    }
                    if !is_subtype(ctx, &rhs_ty, &pointee_ty) {
                        return Err(TypeError::TypeMismatch {
                            expected: pointee_ty.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

                    let move_ctx = apply_whole_value_move(ctx, &rhs.0, &rhs_ty, rhs.1)?;
                    let mut new_ctx = if let Some(owner_name) = owner_name.as_ref() {
                        move_ctx
                            .with_mutable_update(owner_name, rhs_ty.clone())
                            .map_err(|reason| TypeError::InvalidAssignment {
                                variable: owner_name.clone(),
                                reason,
                                span: lhs.1,
                            })?
                    } else {
                        move_ctx
                    };

                    let tlhs_expr = TExpr::Deref {
                        expr: match tlhs.0 {
                            TExpr::Deref { expr, .. } => expr,
                            _ => unreachable!(),
                        },
                        owner: owner_name.clone(),
                        kind,
                        ty: pointee_ty,
                    };

                    let tstmt = TStmt::Assignment {
                        lhs: (tlhs_expr, lhs.1),
                        rhs: trhs,
                        ownership: OwnershipMode::Plain,
                    };

                    if let Some(owner_name) = owner_name.as_ref()
                        && let Some(prop) = postcondition_for_call(&new_ctx, owner_name, &rhs.0)
                    {
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
            if !ctx.bare_metal && ctx.in_region_scope() {
                return Err(TypeError::UnsupportedFeature {
                    feature: "returning from inside a hosted region block is not yet supported"
                        .to_string(),
                    span: expr.1,
                });
            }

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

            let move_ctx = apply_whole_value_move(ctx, &expr.0, &ret_ty, expr.1)?;

            let tstmt = TStmt::Return {
                expr: Box::new(texpr),
                ownership: explicit_transfer_ownership(ctx.bare_metal, &expr.0, &ret_ty),
            };

            Ok(((tstmt, span), move_ctx))
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
                crate::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            // Check body statements with invariant in context
            let scoped_loop_ctx = loop_ctx.enter_borrow_scope();
            let (tbody, body_ctx) = check_stmts(&scoped_loop_ctx, &body.statements)?;
            let body_ctx = body_ctx.exit_borrow_scope();

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
                crate::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            // Step 3: Check body statements
            let scoped_loop_ctx = loop_ctx.enter_borrow_scope();
            let (tbody, body_ctx) = check_stmts(&scoped_loop_ctx, &body.statements)?;
            let body_ctx = body_ctx.exit_borrow_scope();

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

        Stmt::Region { body } => {
            let region_ctx = ctx.enter_region_scope();
            let (tbody, final_region_ctx) = check_block_as_stmt(&region_ctx, body)?;
            let tstmt = TStmt::Region { body: tbody };
            Ok(((tstmt, span), ctx.merge_region_exit(&final_region_ctx)))
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
                    Ok(((tstmt, span), apply_call_argument_moves(ctx, &expr.0)?))
                }
            }
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
    let scoped_ctx = ctx.enter_borrow_scope();
    let (typed_stmts, stmts_ctx) = check_stmts(&scoped_ctx, &block.statements)?;

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
                    final_ctx.exit_borrow_scope(),
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
                    stmts_ctx.exit_borrow_scope(),
                ))
            }
        }
    } else {
        Ok((
            TBlock {
                statements: typed_stmts,
                trailing_expr: None,
            },
            stmts_ctx.exit_borrow_scope(),
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

    if matches!(return_type, IType::Ref(_) | IType::RefMut(_)) {
        return Err(TypeError::UnsupportedFeature {
            feature: "returning references is not yet supported".to_string(),
            span: func_inner.return_type.1,
        });
    }

    // Create context with parameters and expected return type
    let mut func_ctx = global_ctx.clone();
    for (spanned_param, ty) in func_inner.parameters.iter().zip(param_types.iter()) {
        let param = &spanned_param.0;

        if array_contains_reference_type(ty) {
            return Err(TypeError::UnsupportedFeature {
                feature: "storing references inside arrays is not yet supported".to_string(),
                span: spanned_param.1,
            });
        }

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

    let scoped_func_ctx = func_ctx.enter_borrow_scope();
    let (tbody, final_ctx) = check_stmts(&scoped_func_ctx, &func_inner.body.statements)?;

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
        parameter_kinds: global_ctx
            .lookup_function(func_inner.name)
            .map(|sig| sig.parameter_kinds.clone())
            .unwrap_or_else(|| vec![ParameterKind::PlainValue; func_inner.parameters.len()]),
        return_type,
        returns_owned: global_ctx
            .lookup_function(func_inner.name)
            .is_some_and(|sig| sig.returns_owned),
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
    check_program_with_options(program, false)
}

pub fn check_program_bare_metal<'src>(
    program: &Program<'src>,
) -> Result<TProgram<'src>, TypeError<'src>> {
    check_program_with_options(program, true)
}

fn check_program_with_options<'src>(
    program: &Program<'src>,
    bare_metal: bool,
) -> Result<TProgram<'src>, TypeError<'src>> {
    let mut signatures = HashMap::new();

    for spanned_func in &program.functions {
        let (func, func_span) = spanned_func;

        // Convert parameter types
        let mut parameters = Vec::new();
        for spanned_param in &func.parameters {
            let param = &spanned_param.0;
            let ty = ast_type_to_itype(&param.ty)?;
            if array_contains_reference_type(&ty) {
                return Err(TypeError::UnsupportedFeature {
                    feature: "storing references inside arrays is not yet supported".to_string(),
                    span: spanned_param.1,
                });
            }
            parameters.push((param.name.to_string(), ty));
        }

        // Convert return type
        let return_type = ast_type_to_itype(&func.return_type)?;
        if matches!(return_type, IType::Ref(_) | IType::RefMut(_)) {
            return Err(TypeError::UnsupportedFeature {
                feature: "returning references is not yet supported".to_string(),
                span: func.return_type.1,
            });
        }
        if array_contains_reference_type(&return_type) {
            return Err(TypeError::UnsupportedFeature {
                feature: "storing references inside arrays is not yet supported".to_string(),
                span: func.return_type.1,
            });
        }
        let returns_owned = !bare_metal && contains_array_type(&return_type) && func.name != "main";
        let return_ownership = if returns_owned {
            OwnershipMode::FreshOwned
        } else {
            OwnershipMode::Plain
        };
        if !bare_metal && contains_array_type(&return_type) && func.name == "main" {
            return Err(TypeError::UnsupportedFeature {
                feature:
                    "returning arrays from hosted main is not yet supported with function-local region allocation".to_string(),
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

        let parameter_kinds = infer_parameter_kinds(func);

        let sig = FunctionSignature {
            name: func.name.to_string(),
            parameters,
            parameter_kinds,
            return_type,
            return_ownership,
            returns_owned,
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
                parameter_kinds: vec![ParameterKind::PlainValue],
                return_type: IType::Unit,
                return_ownership: OwnershipMode::Plain,
                returns_owned: false,
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
                parameter_kinds: vec![ParameterKind::PlainValue],
                return_type: IType::Unit,
                return_ownership: OwnershipMode::Plain,
                returns_owned: false,
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
                parameter_kinds: vec![],
                return_type: IType::Int,
                return_ownership: OwnershipMode::Plain,
                returns_owned: false,
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
            parameter_kinds: vec![ParameterKind::PlainValue],
            return_type: IType::Int,
            return_ownership: OwnershipMode::Plain,
            returns_owned: false,
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
            parameter_kinds: vec![ParameterKind::PlainValue, ParameterKind::PlainValue],
            return_type: IType::Unit,
            return_ownership: OwnershipMode::Plain,
            returns_owned: false,
            precondition: None,
            postcondition: None,
            span: dummy_span,
        },
    );

    let mut global_ctx = TypingContext::with_functions(signatures);
    global_ctx.bare_metal = bare_metal;

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
