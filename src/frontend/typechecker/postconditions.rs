use crate::common::ast::Expr;
use crate::common::types::{IProposition, IType};
use crate::frontend::typechecker::{TypingContext, is_subtype, synth_expr};
use std::sync::Arc;

/// Check if an expression of type `expr_ty` can satisfy a refined target type
/// by substituting the expression into the refinement predicate and checking provability.
/// Falls back when `is_subtype` alone cannot prove `int <: {v: int | P}`.
pub(super) fn check_expr_satisfies_refined<'src>(
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

/// If the value expression is a function call with a postcondition,
/// produce a proposition with `result` renamed to the binding variable
/// and parameter names substituted with the actual argument values.
pub(super) fn postcondition_for_call<'src>(
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
pub(super) fn find_invalid_free_var<'src>(
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
        Expr::Borrow { expr, .. } => find_invalid_free_var(&expr.0, allowed),
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
        Stmt::Region { body } => {
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
pub(super) fn check_postcondition_provable<'src>(
    ctx: &TypingContext<'src>,
    postcond: &IProposition<'src>,
) -> bool {
    // Use the existing SMT oracle to check if the postcondition is provable
    crate::frontend::typechecker::smt::check_provable(ctx, postcond)
}
