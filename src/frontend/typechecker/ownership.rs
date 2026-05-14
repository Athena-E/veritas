use crate::common::ast::{Block, Expr, Function, Stmt, Type as AstType};
use crate::common::ownership::{OwnershipMode, ParameterKind};
use crate::common::span::Span;
use crate::common::types::IType;
use crate::frontend::typechecker::type_utils::{contains_array_type, source_type_contains_array};
use crate::frontend::typechecker::{TypeError, TypingContext};

pub(super) fn infer_parameter_kinds<'src>(func: &Function<'src>) -> Vec<ParameterKind> {
    func.parameters
        .iter()
        .map(|param| {
            match &param.0.ty.0 {
                AstType::Ref(_) => return ParameterKind::SharedBorrow,
                AstType::RefMut(_) => return ParameterKind::MutableBorrow,
                _ => {}
            }
            if !source_type_contains_array(&param.0.ty.0) {
                return ParameterKind::PlainValue;
            }
            if block_consumes_variable(&func.body, param.0.name)
                || func
                    .body
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_is_whole_value_variable(&expr.0, param.0.name))
            {
                ParameterKind::OwnedValue
            } else {
                ParameterKind::SharedBorrow
            }
        })
        .collect()
}

fn block_consumes_variable<'src>(block: &Block<'src>, name: &str) -> bool {
    block
        .statements
        .iter()
        .any(|stmt| stmt_consumes_variable(&stmt.0, name))
        || block
            .trailing_expr
            .as_ref()
            .is_some_and(|expr| expr_consumes_variable(&expr.0, name))
}

fn stmt_consumes_variable<'src>(stmt: &Stmt<'src>, name: &str) -> bool {
    match stmt {
        Stmt::Let { value, .. } => expr_is_whole_value_variable(&value.0, name),
        Stmt::Assignment { rhs, .. } => expr_is_whole_value_variable(&rhs.0, name),
        Stmt::Return { expr } => expr_is_whole_value_variable(&expr.0, name),
        Stmt::Expr(expr) => expr_consumes_variable(&expr.0, name),
        Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::Region { body } => {
            block_consumes_variable(body, name)
        }
    }
}

fn expr_consumes_variable<'src>(expr: &Expr<'src>, name: &str) -> bool {
    match expr {
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            block_consumes_variable(then_block, name)
                || else_block
                    .as_ref()
                    .is_some_and(|block| block_consumes_variable(block, name))
        }
        _ => expr_is_whole_value_variable(expr, name),
    }
}

fn expr_is_whole_value_variable<'src>(expr: &Expr<'src>, name: &str) -> bool {
    matches!(expr, Expr::Variable(var_name) if *var_name == name)
}

fn expr_mentions_var<'src>(expr: &Expr<'src>, name: &str) -> bool {
    match expr {
        Expr::Variable(var) => *var == name,
        Expr::Literal(_) | Expr::Error => false,
        Expr::BinOp { lhs, rhs, .. } => {
            expr_mentions_var(&lhs.0, name) || expr_mentions_var(&rhs.0, name)
        }
        Expr::UnaryOp { cond, .. } => expr_mentions_var(&cond.0, name),
        Expr::Borrow { expr, .. } => expr_mentions_var(&expr.0, name),
        Expr::Index { base, index } => {
            expr_mentions_var(&base.0, name) || expr_mentions_var(&index.0, name)
        }
        Expr::Call { args, .. } => args.0.iter().any(|arg| expr_mentions_var(&arg.0, name)),
        Expr::ArrayInit { value, length } => {
            expr_mentions_var(&value.0, name) || expr_mentions_var(&length.0, name)
        }
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            expr_mentions_var(&cond.0, name)
                || then_block
                    .statements
                    .iter()
                    .any(|stmt| stmt_mentions_var(&stmt.0, name))
                || then_block
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_mentions_var(&expr.0, name))
                || else_block.as_ref().is_some_and(|block| {
                    block
                        .statements
                        .iter()
                        .any(|stmt| stmt_mentions_var(&stmt.0, name))
                        || block
                            .trailing_expr
                            .as_ref()
                            .is_some_and(|expr| expr_mentions_var(&expr.0, name))
                })
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
            expr_mentions_var(&start.0, name)
                || expr_mentions_var(&end.0, name)
                || (*var != name && expr_mentions_var(&body.0, name))
        }
    }
}

fn stmt_mentions_var<'src>(stmt: &Stmt<'src>, name: &str) -> bool {
    match stmt {
        Stmt::Let { value, .. } => expr_mentions_var(&value.0, name),
        Stmt::Assignment { lhs, rhs } => {
            expr_mentions_var(&lhs.0, name) || expr_mentions_var(&rhs.0, name)
        }
        Stmt::Return { expr } => expr_mentions_var(&expr.0, name),
        Stmt::Expr(expr) => expr_mentions_var(&expr.0, name),
        Stmt::For {
            var,
            start,
            end,
            body,
            ..
        } => {
            expr_mentions_var(&start.0, name)
                || expr_mentions_var(&end.0, name)
                || (*var != name
                    && body
                        .statements
                        .iter()
                        .any(|stmt| stmt_mentions_var(&stmt.0, name)))
        }
        Stmt::While {
            condition, body, ..
        } => {
            expr_mentions_var(&condition.0, name)
                || body
                    .statements
                    .iter()
                    .any(|stmt| stmt_mentions_var(&stmt.0, name))
        }
        Stmt::Region { body } => body
            .statements
            .iter()
            .any(|stmt| stmt_mentions_var(&stmt.0, name)),
    }
}

fn invalidate_moved_binding_props<'src>(
    ctx: &TypingContext<'src>,
    name: &str,
) -> TypingContext<'src> {
    ctx.retain_propositions(|prop| prop.var != name && !expr_mentions_var(&prop.predicate.0, name))
}

fn consume_owned_variable<'src>(ctx: &TypingContext<'src>, name: &str) -> TypingContext<'src> {
    invalidate_moved_binding_props(ctx, name).mark_moved(name)
}

pub(super) fn apply_call_argument_moves<'src>(
    ctx: &TypingContext<'src>,
    expr: &Expr<'src>,
) -> Result<TypingContext<'src>, TypeError<'src>> {
    match expr {
        Expr::Call { func_name, args } => {
            let mut new_ctx = ctx.clone();
            if let Some(sig) = ctx.lookup_function(func_name) {
                for (((arg_expr, _), (_param_name, param_ty)), arg_kind) in args
                    .0
                    .iter()
                    .zip(sig.parameters.iter())
                    .zip(sig.parameter_kinds.iter())
                {
                    new_ctx = apply_call_argument_moves(&new_ctx, arg_expr)?;
                    if arg_kind.is_owned_value()
                        && !new_ctx.bare_metal
                        && contains_array_type(param_ty)
                        && let Expr::Variable(name) = arg_expr
                    {
                        if new_ctx.is_borrowed(name) {
                            return Err(TypeError::BorrowConflict {
                                name: name.to_string(),
                                reason: "cannot move a binding while it is borrowed".to_string(),
                                span: args.1,
                            });
                        }
                        new_ctx = consume_owned_variable(&new_ctx, name);
                    }
                }
            }
            Ok(new_ctx)
        }
        Expr::BinOp { lhs, rhs, .. } => {
            let after_lhs = apply_call_argument_moves(ctx, &lhs.0)?;
            apply_call_argument_moves(&after_lhs, &rhs.0)
        }
        Expr::UnaryOp { cond, .. } => apply_call_argument_moves(ctx, &cond.0),
        Expr::Borrow { expr, .. } => apply_call_argument_moves(ctx, &expr.0),
        Expr::Index { base, index } => {
            let after_base = apply_call_argument_moves(ctx, &base.0)?;
            apply_call_argument_moves(&after_base, &index.0)
        }
        Expr::ArrayInit { value, length } => {
            let after_value = apply_call_argument_moves(ctx, &value.0)?;
            apply_call_argument_moves(&after_value, &length.0)
        }
        _ => Ok(ctx.clone()),
    }
}

pub(super) fn apply_whole_value_move<'src>(
    ctx: &TypingContext<'src>,
    expr: &Expr<'src>,
    ty: &IType<'src>,
    span: Span,
) -> Result<TypingContext<'src>, TypeError<'src>> {
    let new_ctx = apply_call_argument_moves(ctx, expr)?;
    if !new_ctx.bare_metal
        && contains_array_type(ty)
        && let Expr::Variable(name) = expr
    {
        if new_ctx.is_borrowed(name) {
            return Err(TypeError::BorrowConflict {
                name: name.to_string(),
                reason: "cannot move a binding while it is borrowed".to_string(),
                span,
            });
        }
        return Ok(consume_owned_variable(&new_ctx, name));
    }
    Ok(new_ctx)
}

pub(super) fn explicit_transfer_ownership<'src>(
    bare_metal: bool,
    expr: &Expr<'src>,
    ty: &IType<'src>,
) -> OwnershipMode {
    if bare_metal || !contains_array_type(ty) {
        return OwnershipMode::Plain;
    }

    match expr {
        Expr::Variable(_) => OwnershipMode::Consume,
        Expr::ArrayInit { .. } => OwnershipMode::FreshOwned,
        _ => OwnershipMode::Plain,
    }
}

pub(super) fn expr_depends_on_region_local_array<'src>(
    ctx: &TypingContext<'src>,
    expr: &Expr<'src>,
) -> bool {
    match expr {
        Expr::Variable(name) => ctx.is_region_local_array(name),
        Expr::ArrayInit { .. } => ctx.in_region_scope() && !ctx.bare_metal,
        Expr::BinOp { lhs, rhs, .. } => {
            expr_depends_on_region_local_array(ctx, &lhs.0)
                || expr_depends_on_region_local_array(ctx, &rhs.0)
        }
        Expr::UnaryOp { cond, .. } => expr_depends_on_region_local_array(ctx, &cond.0),
        Expr::Borrow { expr, .. } => expr_depends_on_region_local_array(ctx, &expr.0),
        Expr::Call { args, .. } => args
            .0
            .iter()
            .any(|arg| expr_depends_on_region_local_array(ctx, &arg.0)),
        Expr::Index { base, index } => {
            expr_depends_on_region_local_array(ctx, &base.0)
                || expr_depends_on_region_local_array(ctx, &index.0)
        }
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            expr_depends_on_region_local_array(ctx, &cond.0)
                || then_block
                    .statements
                    .iter()
                    .any(|stmt| stmt_depends_on_region_local_array(ctx, &stmt.0))
                || then_block
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_depends_on_region_local_array(ctx, &expr.0))
                || else_block.as_ref().is_some_and(|block| {
                    block
                        .statements
                        .iter()
                        .any(|stmt| stmt_depends_on_region_local_array(ctx, &stmt.0))
                        || block
                            .trailing_expr
                            .as_ref()
                            .is_some_and(|expr| expr_depends_on_region_local_array(ctx, &expr.0))
                })
        }
        Expr::Forall {
            start, end, body, ..
        }
        | Expr::Exists {
            start, end, body, ..
        } => {
            expr_depends_on_region_local_array(ctx, &start.0)
                || expr_depends_on_region_local_array(ctx, &end.0)
                || expr_depends_on_region_local_array(ctx, &body.0)
        }
        Expr::Literal(_) | Expr::Error => false,
    }
}

fn stmt_depends_on_region_local_array<'src>(ctx: &TypingContext<'src>, stmt: &Stmt<'src>) -> bool {
    match stmt {
        Stmt::Let { value, .. } => expr_depends_on_region_local_array(ctx, &value.0),
        Stmt::Assignment { lhs, rhs } => {
            expr_depends_on_region_local_array(ctx, &lhs.0)
                || expr_depends_on_region_local_array(ctx, &rhs.0)
        }
        Stmt::Return { expr } => expr_depends_on_region_local_array(ctx, &expr.0),
        Stmt::Expr(expr) => expr_depends_on_region_local_array(ctx, &expr.0),
        Stmt::For {
            start, end, body, ..
        } => {
            expr_depends_on_region_local_array(ctx, &start.0)
                || expr_depends_on_region_local_array(ctx, &end.0)
                || body
                    .statements
                    .iter()
                    .any(|stmt| stmt_depends_on_region_local_array(ctx, &stmt.0))
                || body
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_depends_on_region_local_array(ctx, &expr.0))
        }
        Stmt::While {
            condition, body, ..
        } => {
            expr_depends_on_region_local_array(ctx, &condition.0)
                || body
                    .statements
                    .iter()
                    .any(|stmt| stmt_depends_on_region_local_array(ctx, &stmt.0))
                || body
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_depends_on_region_local_array(ctx, &expr.0))
        }
        Stmt::Region { body } => {
            body.statements
                .iter()
                .any(|stmt| stmt_depends_on_region_local_array(ctx, &stmt.0))
                || body
                    .trailing_expr
                    .as_ref()
                    .is_some_and(|expr| expr_depends_on_region_local_array(ctx, &expr.0))
        }
    }
}
