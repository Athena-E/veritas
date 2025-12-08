// Subtyping relation for refinement types
// - Reflexivity: T <: T
// - Refinement weakening
// - Singleton to base: int(n) <: int
// - Structural: &T <: &T, [T; n] <: [T; n] (invariant)

use crate::common::ast::{Expr, Literal};
use crate::common::types::{IProposition, IType, IValue};
use crate::frontend::typechecker::{TypingContext, check_provable};
use std::sync::Arc;

/// Check if sub is a subtype of sup in the given typing context
pub fn is_subtype<'src>(ctx: &TypingContext<'src>, sub: &IType<'src>, sup: &IType<'src>) -> bool {
    match (sub, sup) {
        // Reflexivity: T <: T
        (IType::Unit, IType::Unit) => true,
        (IType::Int, IType::Int) => true,
        (IType::Bool, IType::Bool) => true,

        // Singleton to base: int(n) <: int
        (IType::SingletonInt(_), IType::Int) => true,

        // Singleton reflexivity: int(n) <: int(n)
        (IType::SingletonInt(v1), IType::SingletonInt(v2)) => v1 == v2,

        // Singleton to refined: int(n) <: {x: int | P} if P[n/x]
        (IType::SingletonInt(n), IType::RefinedInt { prop, .. }) => {
            // Create proposition P[n/x] - substitute n for x in predicate
            let substituted_prop = substitute_value_in_prop(prop, n);
            check_provable(ctx, &substituted_prop)
        }

        // Refined to base: {x: int | P} <: int (always true)
        (IType::RefinedInt { .. }, IType::Int) => true,

        // Refined to refined: {x: int | P} <: {x: int | Q} if Phi /\ P |- Q
        (
            IType::RefinedInt { base: base1, prop: prop1 },
            IType::RefinedInt { base: base2, prop: prop2 },
        ) => {
            // First check base types are compatible
            if !is_subtype(ctx, base1, base2) {
                return false;
            }

            // Check if P implies Q: add P to context, check if Q is provable
            let ctx_with_p = ctx.with_proposition(prop1.clone());

            // Rename the variable in Q to match P if needed
            let renamed_q = if prop1.var != prop2.var {
                rename_prop_var(prop2, &prop1.var)
            } else {
                prop2.clone()
            };

            check_provable(&ctx_with_p, &renamed_q)
        }

        // Array subtyping: [T1; n] <: [T2; m] if T1 <: T2 and n = m
        // allow covariance for immutable arrays since elements are read-only
        (
            IType::Array { element_type: elem1, size: size1 },
            IType::Array { element_type: elem2, size: size2 },
        ) => {
            // Sizes must match, element type is covariant for immutable arrays
            size1 == size2 && is_subtype(ctx, elem1, elem2)
        }

        // Reference subtyping: &T1 <: &T2 if T1 = T2 (invariant for shared refs)
        (IType::Ref(t1), IType::Ref(t2)) => types_equal(t1, t2),

        // Mutable reference subtyping: &mut T1 <: &mut T2 if T1 = T2 (invariant)
        (IType::RefMut(t1), IType::RefMut(t2)) => types_equal(t1, t2),

        // Master type unwrapping: M(T) <: T (can use master type as base)
        (IType::Master(t), sup) => is_subtype(ctx, t, sup),

        // Different type constructors are incompatible
        _ => false,
    }
}

/// Check if two types are structurally equal
fn types_equal(t1: &IType, t2: &IType) -> bool {
    match (t1, t2) {
        (IType::Unit, IType::Unit) => true,
        (IType::Int, IType::Int) => true,
        (IType::Bool, IType::Bool) => true,

        (IType::SingletonInt(v1), IType::SingletonInt(v2)) => v1 == v2,

        (
            IType::Array { element_type: e1, size: s1 },
            IType::Array { element_type: e2, size: s2 },
        ) => s1 == s2 && types_equal(e1, e2),

        (IType::Ref(t1), IType::Ref(t2)) => types_equal(t1, t2),
        (IType::RefMut(t1), IType::RefMut(t2)) => types_equal(t1, t2),
        (IType::Master(t1), IType::Master(t2)) => types_equal(t1, t2),

        // TODO: need SMT for refined types
        // Temp fix: require exact structural match
        (
            IType::RefinedInt { base: b1, prop: p1 },
            IType::RefinedInt { base: b2, prop: p2 },
        ) => {
            types_equal(b1, b2) && p1.var == p2.var
        }

        _ => false,
    }
}

/// Substitute a value for the bound variable in a proposition
fn substitute_value_in_prop<'src>(prop: &IProposition<'src>, value: &'src IValue) -> IProposition<'src> {
    let substituted_expr = substitute_value_in_expr(&prop.predicate.0, &prop.var, value);

    IProposition {
        var: "_".to_string(), 
        predicate: Arc::new((substituted_expr, prop.predicate.1)),
    }
}

/// Substitute a value for a variable in an expression
fn substitute_value_in_expr<'src>(
    expr: &Expr<'src>,
    var: &str,
    value: &'src IValue,
) -> Expr<'src> {
    match expr {
        Expr::Variable(name) if *name == var => {
            // Replace variable with value
            match value {
                IValue::Int(n) => Expr::Literal(Literal::Int(*n)),
                IValue::Bool(b) => Expr::Literal(Literal::Bool(*b)),
                IValue::Symbolic(s) => Expr::Variable(s.as_str()),
            }
        }

        Expr::Variable(_) | Expr::Literal(_) => expr.clone(),

        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((
                substitute_value_in_expr(&lhs.0, var, value),
                lhs.1,
            )),
            rhs: Box::new((
                substitute_value_in_expr(&rhs.0, var, value),
                rhs.1,
            )),
        },

        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((
                substitute_value_in_expr(&cond.0, var, value),
                cond.1,
            )),
        },

        _ => expr.clone(),
    }
}

/// Rename the bound variable in a proposition
fn rename_prop_var<'src>(prop: &IProposition<'src>, new_var: &'src str) -> IProposition<'src> {
    let renamed_expr = rename_var_in_expr(&prop.predicate.0, &prop.var, new_var);

    IProposition {
        var: new_var.to_string(),
        predicate: Arc::new((renamed_expr, prop.predicate.1)),
    }
}

/// Rename a variable in an expression
fn rename_var_in_expr<'src>(
    expr: &Expr<'src>,
    old_var: &str,
    new_var: &'src str,
) -> Expr<'src> {
    match expr {
        Expr::Variable(name) if *name == old_var => Expr::Variable(new_var),

        Expr::Variable(_) | Expr::Literal(_) => expr.clone(),

        Expr::BinOp { op, lhs, rhs } => Expr::BinOp {
            op: *op,
            lhs: Box::new((
                rename_var_in_expr(&lhs.0, old_var, new_var),
                lhs.1,
            )),
            rhs: Box::new((
                rename_var_in_expr(&rhs.0, old_var, new_var),
                rhs.1,
            )),
        },

        Expr::UnaryOp { op, cond } => Expr::UnaryOp {
            op: *op,
            cond: Box::new((
                rename_var_in_expr(&cond.0, old_var, new_var),
                cond.1,
            )),
        },

        _ => expr.clone(),
    }
}
