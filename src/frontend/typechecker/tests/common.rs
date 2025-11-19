// Common test utilities for type checker tests

use crate::common::ast::{BinOp, Expr, Literal};
use crate::common::types::{IProposition, IType};
use crate::frontend::typechecker::{TypingContext, check_provable};
use chumsky::prelude::SimpleSpan;
use std::sync::Arc;

/// Create a simple integer proposition like {x: int | x > 5}
pub fn make_int_prop<'src>(var: &str, predicate_expr: Expr<'src>) -> IProposition<'src> {
    IProposition {
        var: var.to_string(),
        predicate: Arc::new((predicate_expr, SimpleSpan::new(0, 0))),
    }
}

/// Create a refined integer type like {x: int | x > 0}
pub fn make_refined_int<'src>(var: &str, predicate_expr: Expr<'src>) -> IType<'src> {
    IType::RefinedInt {
        base: Arc::new(IType::Int),
        prop: make_int_prop(var, predicate_expr),
    }
}

/// Create a comparison expression: var op value
/// Example: x > 5
pub fn make_comparison<'src>(var: &'src str, op: BinOp, value: i64) -> Expr<'src> {
    Expr::BinOp {
        op,
        lhs: Box::new((Expr::Variable(var), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(value)), SimpleSpan::new(0, 0))),
    }
}

/// Create an equality expression: var == other_var
pub fn make_var_equality<'src>(var1: &'src str, var2: &'src str) -> Expr<'src> {
    Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((Expr::Variable(var1), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Variable(var2), SimpleSpan::new(0, 0))),
    }
}

/// Create an arithmetic expression: var + offset
pub fn make_add_expr<'src>(var: &'src str, offset: i64) -> Expr<'src> {
    Expr::BinOp {
        op: BinOp::Add,
        lhs: Box::new((Expr::Variable(var), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(offset)), SimpleSpan::new(0, 0))),
    }
}

/// Assert that a proposition is provable in the given context
#[track_caller]
pub fn assert_provable(ctx: &TypingContext, prop: &IProposition) {
    assert!(
        check_provable(ctx, prop),
        "Expected proposition to be provable: {}, but it was not",
        prop
    );
}

/// Assert that a proposition is not provable in the given context
#[track_caller]
pub fn assert_unprovable(ctx: &TypingContext, prop: &IProposition) {
    assert!(
        !check_provable(ctx, prop),
        "Expected proposition to be unprovable: {}, but it was provable",
        prop
    );
}

/// Create a typing context with some immutable variables
pub fn make_context_with_vars<'src>(vars: Vec<(&str, IType<'src>)>) -> TypingContext<'src> {
    let mut ctx = TypingContext::new();
    for (name, ty) in vars {
        ctx = ctx.with_immutable(name.to_string(), ty);
    }
    ctx
}

/// Create a typing context with propositions
pub fn make_context_with_props<'src>(props: Vec<IProposition<'src>>) -> TypingContext<'src> {
    let mut ctx = TypingContext::new();
    for prop in props {
        ctx = ctx.with_proposition(prop);
    }
    ctx
}
