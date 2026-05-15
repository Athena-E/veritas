use crate::common::ast::{Block, Expr, Function, Program, Stmt};
use crate::common::ownership::{LifetimeId, OwnershipMode, ParameterKind};
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
use crate::frontend::typechecker::{TypeError, TypingContext, VarBinding, is_subtype, synth_expr};
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

fn expr_mentions_binding<'src>(expr: &Expr<'src>, name: &str) -> bool {
    match expr {
        Expr::Variable(var) => *var == name,
        Expr::Literal(_) | Expr::Error => false,
        Expr::BinOp { lhs, rhs, .. } => {
            expr_mentions_binding(&lhs.0, name) || expr_mentions_binding(&rhs.0, name)
        }
        Expr::UnaryOp { cond, .. } => expr_mentions_binding(&cond.0, name),
        Expr::Borrow { expr, .. } => expr_mentions_binding(&expr.0, name),
        Expr::Index { base, index } => {
            expr_mentions_binding(&base.0, name) || expr_mentions_binding(&index.0, name)
        }
        Expr::Call { args, .. } => args.0.iter().any(|arg| expr_mentions_binding(&arg.0, name)),
        Expr::ArrayInit { value, length } => {
            expr_mentions_binding(&value.0, name) || expr_mentions_binding(&length.0, name)
        }
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            expr_mentions_binding(&cond.0, name)
                || block_mentions_binding(then_block, name)
                || else_block
                    .as_ref()
                    .is_some_and(|block| block_mentions_binding(block, name))
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
            expr_mentions_binding(&start.0, name)
                || expr_mentions_binding(&end.0, name)
                || (*var != name && expr_mentions_binding(&body.0, name))
        }
    }
}

fn stmt_mentions_binding<'src>(stmt: &Stmt<'src>, name: &str) -> bool {
    match stmt {
        Stmt::Let { value, .. } => expr_mentions_binding(&value.0, name),
        Stmt::Assignment { lhs, rhs } => {
            expr_mentions_binding(&lhs.0, name) || expr_mentions_binding(&rhs.0, name)
        }
        Stmt::Return { expr } => expr_mentions_binding(&expr.0, name),
        Stmt::Expr(expr) => expr_mentions_binding(&expr.0, name),
        Stmt::For {
            var,
            start,
            end,
            body,
            ..
        } => {
            expr_mentions_binding(&start.0, name)
                || expr_mentions_binding(&end.0, name)
                || (*var != name && block_mentions_binding(body, name))
        }
        Stmt::While {
            condition, body, ..
        } => expr_mentions_binding(&condition.0, name) || block_mentions_binding(body, name),
        Stmt::Region { body } => block_mentions_binding(body, name),
    }
}

fn block_mentions_binding<'src>(block: &Block<'src>, name: &str) -> bool {
    block
        .statements
        .iter()
        .any(|stmt| stmt_mentions_binding(&stmt.0, name))
        || block
            .trailing_expr
            .as_ref()
            .is_some_and(|expr| expr_mentions_binding(&expr.0, name))
}

fn remaining_mentions_binding<'src>(
    remaining_stmts: &[Spanned<Stmt<'src>>],
    trailing_expr: Option<&Spanned<Expr<'src>>>,
    name: &str,
) -> bool {
    remaining_stmts
        .iter()
        .any(|stmt| stmt_mentions_binding(&stmt.0, name))
        || trailing_expr.is_some_and(|expr| expr_mentions_binding(&expr.0, name))
}

fn release_dead_borrow_bindings<'src>(
    ctx: TypingContext<'src>,
    remaining_stmts: &[Spanned<Stmt<'src>>],
    trailing_expr: Option<&Spanned<Expr<'src>>>,
    typed_stmts: &mut Vec<Spanned<TStmt<'src>>>,
    span: Span,
) -> TypingContext<'src> {
    let mut current_ctx = ctx;
    for name in current_ctx.live_borrow_binding_names() {
        if remaining_mentions_binding(remaining_stmts, trailing_expr, &name) {
            continue;
        }
        let Some(binding) = current_ctx.lookup_var(&name) else {
            current_ctx = current_ctx.release_borrow_binding(&name);
            continue;
        };
        let Some(lifetime) = current_ctx.lookup_borrow_lifetime(&name) else {
            current_ctx = current_ctx.release_borrow_binding(&name);
            continue;
        };
        let ty = match binding {
            VarBinding::Immutable(ty) => ty,
            VarBinding::Mutable(binding) => binding.current_type,
        };
        typed_stmts.push((
            TStmt::BorrowEnd {
                name: name.clone(),
                lifetime,
                ty,
            },
            span,
        ));
        current_ctx = current_ctx.release_borrow_binding(&name);
    }
    current_ctx
}

fn with_borrow_lifetime<'src>(
    mut expr: Spanned<TExpr<'src>>,
    lifetime: LifetimeId,
) -> Spanned<TExpr<'src>> {
    if let TExpr::Borrow {
        lifetime: borrow_lifetime,
        ..
    } = &mut expr.0
    {
        *borrow_lifetime = Some(lifetime);
    }
    expr
}

fn ast_block_has_return<'src>(block: &Block<'src>) -> bool {
    block
        .statements
        .iter()
        .any(|stmt| ast_stmt_has_return(&stmt.0))
}

fn ast_stmt_has_return<'src>(stmt: &Stmt<'src>) -> bool {
    match stmt {
        Stmt::Return { .. } => true,
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Region { body } => {
            ast_block_has_return(body)
        }
        Stmt::Expr(expr) => match &expr.0 {
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                ast_block_has_return(then_block)
                    || else_block
                        .as_ref()
                        .is_some_and(|block| ast_block_has_return(block))
            }
            _ => false,
        },
        _ => false,
    }
}

fn validate_reference_return<'src>(
    func: &Function<'src>,
    param_types: &[IType<'src>],
    return_type: &IType<'src>,
) -> Result<(), TypeError<'src>> {
    if !is_reference_type(return_type) {
        return Ok(());
    }

    if ast_block_has_return(&func.body) {
        return Err(TypeError::UnsupportedFeature {
            feature: "return statements with reference return types are not yet supported"
                .to_string(),
            span: func.return_type.1,
        });
    }

    let Some(trailing) = &func.body.trailing_expr else {
        return Err(TypeError::UnsupportedFeature {
            feature: "reference-returning functions must return a reference parameter".to_string(),
            span: func.return_type.1,
        });
    };

    let Expr::Variable(returned_name) = &trailing.0 else {
        return Err(TypeError::UnsupportedFeature {
            feature: "returning references is limited to returning a reference parameter"
                .to_string(),
            span: trailing.1,
        });
    };

    let Some((_, param_ty)) = func
        .parameters
        .iter()
        .zip(param_types.iter())
        .find(|(param, _)| param.0.name == *returned_name)
    else {
        return Err(TypeError::UnsupportedFeature {
            feature: "cannot return references derived from local bindings".to_string(),
            span: trailing.1,
        });
    };

    if !is_subtype(&TypingContext::new(), param_ty, return_type) {
        return Err(TypeError::TypeMismatch {
            expected: return_type.clone(),
            found: param_ty.clone(),
            span: trailing.1,
        });
    }

    Ok(())
}

pub fn check_stmt<'src>(
    ctx: &TypingContext<'src>,
    stmt: &Spanned<Stmt<'src>>,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    let span = stmt.1;

    match &stmt.0 {
        Stmt::Let {
            name,
            ty,
            value,
            is_mut: false,
        } => {
            reject_shadowing_borrowed_owner(ctx, name, span)?;

            let (mut tvalue, value_ty) = synth_expr(ctx, value)?;

            let ann_ty = ast_type_to_itype(ty)?;

            if array_contains_reference_type(&ann_ty) {
                return Err(TypeError::UnsupportedFeature {
                    feature: "storing references inside arrays is not yet supported".to_string(),
                    span: ty.1,
                });
            }

            if !is_subtype(ctx, &value_ty, &ann_ty) {
                return Err(TypeError::TypeMismatch {
                    expected: ann_ty,
                    found: value_ty,
                    span: value.1,
                });
            }

            let move_ctx = apply_whole_value_move(ctx, &value.0, &value_ty, value.1)?;

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
                if let Some(lifetime) = new_ctx.lookup_borrow_lifetime(name) {
                    tvalue = with_borrow_lifetime(tvalue, lifetime);
                }
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

        Stmt::Let {
            name,
            ty,
            value,
            is_mut: true,
        } => {
            reject_shadowing_borrowed_owner(ctx, name, span)?;

            let (mut tvalue, value_ty) = synth_expr(ctx, value)?;

            let ann_ty = ast_type_to_itype(ty)?;

            if array_contains_reference_type(&ann_ty) {
                return Err(TypeError::UnsupportedFeature {
                    feature: "storing references inside arrays is not yet supported".to_string(),
                    span: ty.1,
                });
            }

            if !is_subtype(ctx, &value_ty, &ann_ty) {
                return Err(TypeError::TypeMismatch {
                    expected: ann_ty.clone(),
                    found: value_ty,
                    span: value.1,
                });
            }

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
                if let Some(lifetime) = new_ctx.lookup_borrow_lifetime(name) {
                    tvalue = with_borrow_lifetime(tvalue, lifetime);
                }
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

        Stmt::Assignment { lhs, rhs } => {
            let (mut trhs, rhs_ty) = synth_expr(ctx, rhs)?;

            match &lhs.0 {
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

                    let binding =
                        ctx.lookup_mutable(var_name)
                            .ok_or_else(|| TypeError::NotMutable {
                                name: var_name.to_string(),
                                span,
                            })?;

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
                        if let Some(lifetime) = new_ctx.lookup_borrow_lifetime(var_name) {
                            trhs = with_borrow_lifetime(trhs, lifetime);
                        }
                    }

                    if !ctx.bare_metal && ctx.in_region_scope() && contains_array_type(&rhs_ty) {
                        if rhs_depends_on_region_local {
                            new_ctx = new_ctx.mark_region_local_array(var_name);
                        } else if ctx.is_region_scoped_array(var_name) {
                            new_ctx = new_ctx.clear_region_local_array(var_name);
                        }
                    }

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

                crate::common::ast::Expr::Index { base, index } => {
                    if let Expr::Variable(arr_name) = &base.0 {
                        reject_mutating_borrowed_owner(ctx, arr_name, span)?;
                    }

                    let (tbase, base_ty) = synth_expr(ctx, base)?;
                    let (tindex, index_ty) = synth_expr(ctx, index)?;

                    if !is_subtype(ctx, &index_ty, &IType::Int) {
                        return Err(TypeError::TypeMismatch {
                            expected: IType::Int,
                            found: index_ty,
                            span: index.1,
                        });
                    }

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

                    crate::frontend::typechecker::check_array_bounds_expr(
                        ctx,
                        &index.0,
                        &index_ty,
                        &array_size,
                        &base_ty,
                        index.1,
                    )?;

                    if !is_subtype(ctx, &rhs_ty, &elem_ty) {
                        return Err(TypeError::TypeMismatch {
                            expected: elem_ty.clone(),
                            found: rhs_ty,
                            span: rhs.1,
                        });
                    }

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

                    let mut new_ctx = move_ctx.clone();

                    if let Some((arr_name, indices)) = extract_array_access(&lhs.0) {
                        let dummy_span = chumsky::span::SimpleSpan::new(0, 0);

                        let (resolved, any_resolved) = resolve_array_reads_in_expr(ctx, &rhs.0);
                        let snapshot_rhs = if any_resolved {
                            resolved
                        } else {
                            rhs.0.clone()
                        };

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

        Stmt::Return { expr } => {
            if !ctx.bare_metal && ctx.in_region_scope() {
                return Err(TypeError::UnsupportedFeature {
                    feature: "returning from inside a hosted region block is not yet supported"
                        .to_string(),
                    span: expr.1,
                });
            }

            let (texpr, ret_ty) = synth_expr(ctx, expr)?;

            if let Some(expected) = ctx.get_expected_return()
                && !is_subtype(ctx, &ret_ty, expected)
            {
                return Err(TypeError::ReturnTypeMismatch {
                    expected: expected.clone(),
                    found: ret_ty,
                    span: expr.1,
                });
            }

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

        Stmt::For {
            var,
            start,
            end,
            invariant,
            body,
        } => {
            let (tstart, start_ty) = synth_expr(ctx, start)?;
            let (tend, end_ty) = synth_expr(ctx, end)?;

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

            let mut loop_ctx = ctx.with_immutable(var.to_string(), loop_var_ty.clone());

            let lower_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((lower_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(lower_bound_prop);

            let upper_bound_prop = crate::common::types::IProposition {
                var: var.to_string(),
                predicate: Arc::new((upper_bound_expr, dummy_span)),
            };
            loop_ctx = loop_ctx.with_proposition(upper_bound_prop);

            let tinvariant = if let Some(inv_expr) = invariant {
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

                let inv_prop = IProposition {
                    var: var.to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                loop_ctx = loop_ctx.with_proposition(inv_prop);

                crate::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            let scoped_loop_ctx = loop_ctx.enter_borrow_scope();
            let (tbody, body_ctx) = check_stmts(&scoped_loop_ctx, &body.statements)?;
            let body_ctx = body_ctx.exit_borrow_scope();

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

            let post_ctx = if let Some(inv_expr) = invariant {
                use crate::frontend::typechecker::helpers::substitute_expr_for_var;
                let mut post = ctx.clone();

                let modifications = collect_array_modifications(&body.statements);
                for (arr_name, indices) in &modifications {
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

        Stmt::While {
            condition,
            invariant,
            body,
        } => {
            let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);

            let (tcond, cond_ty) = synth_expr(ctx, condition)?;
            if !is_subtype(ctx, &cond_ty, &IType::Bool) {
                return Err(TypeError::TypeMismatch {
                    expected: IType::Bool,
                    found: cond_ty,
                    span: condition.1,
                });
            }

            let cond_prop = IProposition {
                var: "_cond".to_string(),
                predicate: Arc::new(*condition.clone()),
            };
            let mut loop_ctx = ctx.with_proposition(cond_prop);

            let tinvariant = if let Some(inv_expr) = invariant {
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

                let inv_prop = IProposition {
                    var: "_inv".to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                if !crate::frontend::typechecker::check_provable(ctx, &inv_prop) {
                    return Err(TypeError::InvariantNotEstablished {
                        invariant_span: inv_expr.1,
                    });
                }

                loop_ctx = loop_ctx.with_proposition(inv_prop);

                crate::dtal::convert::expr_to_constraint(&inv_expr.0)
            } else {
                None
            };

            let scoped_loop_ctx = loop_ctx.enter_borrow_scope();
            let (tbody, body_ctx) = check_stmts(&scoped_loop_ctx, &body.statements)?;
            let body_ctx = body_ctx.exit_borrow_scope();

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

            let mut post_ctx = ctx.clone();

            let modifications = collect_array_modifications(&body.statements);
            for (arr_name, indices) in &modifications {
                post_ctx = invalidate_array_props_selectively(&post_ctx, arr_name, indices);
            }

            if let Some(inv_expr) = invariant {
                let inv_prop = IProposition {
                    var: "_inv".to_string(),
                    predicate: Arc::new(inv_expr.clone()),
                };
                post_ctx = post_ctx.with_proposition(inv_prop);
            }

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

        Stmt::Expr(expr) => match &expr.0 {
            crate::common::ast::Expr::If {
                cond,
                then_block,
                else_block,
            } => check_if_stmt(ctx, cond, then_block, else_block.as_ref(), span),
            _ => {
                let (texpr, _) = synth_expr(ctx, expr)?;
                let tstmt = TStmt::Expr(texpr);
                Ok(((tstmt, span), apply_call_argument_moves(ctx, &expr.0)?))
            }
        },
    }
}

fn check_if_stmt<'src>(
    ctx: &TypingContext<'src>,
    cond: &Spanned<crate::common::ast::Expr<'src>>,
    then_block: &Block<'src>,
    else_block: Option<&Block<'src>>,
    span: Span,
) -> Result<(Spanned<TStmt<'src>>, TypingContext<'src>), TypeError<'src>> {
    use crate::frontend::typechecker::{extract_proposition, negate_proposition};

    let (tcond, cond_ty) = synth_expr(ctx, cond)?;

    if !is_subtype(ctx, &cond_ty, &IType::Bool) {
        return Err(TypeError::TypeMismatch {
            expected: IType::Bool,
            found: cond_ty,
            span: cond.1,
        });
    }

    let mut then_ctx = ctx.clone();
    if let Some(prop) = extract_proposition(&cond.0) {
        then_ctx = then_ctx.with_proposition(prop);
    }

    let (tthen_block, then_final_ctx) = check_block_as_stmt(&then_ctx, then_block)?;

    let (telse_block, else_final_ctx) = if let Some(else_blk) = else_block {
        let mut else_ctx = ctx.clone();
        if let Some(prop) = extract_proposition(&cond.0) {
            let neg_prop = negate_proposition(&prop);
            else_ctx = else_ctx.with_proposition(neg_prop);
        }

        let (typed_else, else_ctx_final) = check_block_as_stmt(&else_ctx, else_blk)?;
        (Some(typed_else), else_ctx_final)
    } else {
        (None, ctx.clone())
    };

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

fn check_block_as_stmt<'src>(
    ctx: &TypingContext<'src>,
    block: &Block<'src>,
) -> Result<(TBlock<'src>, TypingContext<'src>), TypeError<'src>> {
    let scoped_ctx = ctx.enter_borrow_scope();
    let (typed_stmts, stmts_ctx) = check_stmts_with_trailing(
        &scoped_ctx,
        &block.statements,
        block.trailing_expr.as_deref(),
    )?;

    if let Some(trailing) = &block.trailing_expr {
        match &trailing.0 {
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

pub fn check_stmts<'src>(
    ctx: &TypingContext<'src>,
    stmts: &[Spanned<Stmt<'src>>],
) -> Result<(Vec<Spanned<TStmt<'src>>>, TypingContext<'src>), TypeError<'src>> {
    check_stmts_with_trailing(ctx, stmts, None)
}

fn check_stmts_with_trailing<'src>(
    ctx: &TypingContext<'src>,
    stmts: &[Spanned<Stmt<'src>>],
    trailing_expr: Option<&Spanned<Expr<'src>>>,
) -> Result<(Vec<Spanned<TStmt<'src>>>, TypingContext<'src>), TypeError<'src>> {
    let mut current_ctx = ctx.clone();
    let mut typed_stmts = Vec::new();

    for (idx, stmt) in stmts.iter().enumerate() {
        let (tstmt, new_ctx) = check_stmt(&current_ctx, stmt)?;
        typed_stmts.push(tstmt);
        current_ctx = release_dead_borrow_bindings(
            new_ctx,
            &stmts[idx + 1..],
            trailing_expr,
            &mut typed_stmts,
            stmt.1,
        );
    }

    Ok((typed_stmts, current_ctx))
}

pub fn check_function<'src>(
    global_ctx: &TypingContext<'src>,
    func: &Spanned<Function<'src>>,
) -> Result<TFunction<'src>, TypeError<'src>> {
    let (func_inner, func_span) = func;

    let mut param_types = Vec::new();
    for spanned_param in &func_inner.parameters {
        let param = &spanned_param.0;
        param_types.push(ast_type_to_itype(&param.ty)?);
    }

    let return_type = ast_type_to_itype(&func_inner.return_type)?;

    validate_reference_return(func_inner, &param_types, &return_type)?;

    let mut func_ctx = global_ctx.clone();
    for (spanned_param, ty) in func_inner.parameters.iter().zip(param_types.iter()) {
        let param = &spanned_param.0;

        if array_contains_reference_type(ty) {
            return Err(TypeError::UnsupportedFeature {
                feature: "storing references inside arrays is not yet supported".to_string(),
                span: spanned_param.1,
            });
        }

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

        if is_subtype(&func_ctx, ty, &IType::I64) {
            func_ctx = add_i64_range_props(func_ctx, param.name);
        }
        if is_subtype(&func_ctx, ty, &IType::U64) {
            func_ctx = add_u64_range_props(func_ctx, param.name);
        }

        if let IType::Array {
            size: IValue::Symbolic(size_var),
            ..
        } = ty
        {
            func_ctx = add_array_length_axioms(func_ctx, size_var);
        }
    }
    func_ctx = func_ctx.with_expected_return(return_type.clone());

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

    let postcondition = global_ctx
        .lookup_function(func_inner.name)
        .and_then(|sig| sig.postcondition.clone());

    if let Some(ref pc) = postcondition {
        func_ctx = func_ctx.with_postcondition(pc.clone());
    }
    func_ctx = func_ctx.with_current_function(func_inner.name.to_string());

    let scoped_func_ctx = func_ctx.enter_borrow_scope();
    let (tbody, final_ctx) = check_stmts_with_trailing(
        &scoped_func_ctx,
        &func_inner.body.statements,
        func_inner.body.trailing_expr.as_deref(),
    )?;

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

    let treturn = if let Some(ret_expr) = &func_inner.body.trailing_expr {
        let (texpr, ret_ty) = synth_expr(&final_ctx, ret_expr)?;

        if !check_expr_satisfies_refined(&final_ctx, &ret_expr.0, &ret_ty, &return_type) {
            return Err(TypeError::TypeMismatch {
                expected: return_type.clone(),
                found: ret_ty,
                span: ret_expr.1,
            });
        }

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

        let return_type = ast_type_to_itype(&func.return_type)?;
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

        let precondition = func.precondition.as_ref().map(|precond_expr| {
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

        let postcondition =
            func.postcondition
                .as_ref()
                .map(|postcond_expr| crate::common::types::IProposition {
                    var: "result".to_string(),
                    predicate: Arc::new(postcond_expr.clone()),
                });

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

    let dummy_span = chumsky::prelude::SimpleSpan::new(0, 0);

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
    }

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

    for (constant, _span) in &program.constants {
        let ty = ast_type_to_itype(&constant.ty)?;
        let (_, value_ty) = synth_expr(&global_ctx, &constant.value)?;
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
