use super::common::*;
use crate::common::ast::{BinOp, Expr, Literal};
use crate::frontend::typechecker::TypingContext;
use chumsky::prelude::SimpleSpan;
#[test]

fn test_smt_simple_implication() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Gt, 0)));

    let goal = make_int_prop("x", make_comparison("x", BinOp::Gte, 0));
    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_transitivity() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Gt, 5)));

    let goal = make_int_prop("x", make_comparison("x", BinOp::Gt, 3));
    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_arithmetic_evaluation() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Eq, 10)));

    let lhs = Expr::BinOp {
        op: BinOp::Add,
        lhs: Box::new((Expr::Variable("x"), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(5)), SimpleSpan::new(0, 0))),
    };
    let goal_expr = Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((lhs, SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(15)), SimpleSpan::new(0, 0))),
    };
    let goal = make_int_prop("_", goal_expr);

    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_unprovable_weakening() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Gt, 0)));

    let goal = make_int_prop("x", make_comparison("x", BinOp::Gt, 10));
    assert_unprovable(&ctx, &goal);
}
#[test]

fn test_smt_unprovable_no_context() {
    let ctx = TypingContext::new();

    let goal = make_int_prop("x", make_comparison("x", BinOp::Gt, 0));
    assert_unprovable(&ctx, &goal);
}
#[test]

fn test_smt_boolean_and() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Gt, 0)));
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Lt, 10)));

    let lhs = make_comparison("x", BinOp::Gt, 0);
    let rhs = make_comparison("x", BinOp::Lt, 10);
    let goal_expr = Expr::BinOp {
        op: BinOp::And,
        lhs: Box::new((lhs, SimpleSpan::new(0, 0))),
        rhs: Box::new((rhs, SimpleSpan::new(0, 0))),
    };
    let goal = make_int_prop("_", goal_expr);

    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_boolean_or() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_proposition(make_int_prop("x", make_comparison("x", BinOp::Gt, 10)));

    let lhs = make_comparison("x", BinOp::Gt, 10);
    let rhs = make_comparison("x", BinOp::Lt, 0);
    let goal_expr = Expr::BinOp {
        op: BinOp::Or,
        lhs: Box::new((lhs, SimpleSpan::new(0, 0))),
        rhs: Box::new((rhs, SimpleSpan::new(0, 0))),
    };
    let goal = make_int_prop("_", goal_expr);

    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_two_variable_inequality() {
    let ctx = TypingContext::new();

    let x_gt_y = Expr::BinOp {
        op: BinOp::Gt,
        lhs: Box::new((Expr::Variable("x"), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Variable("y"), SimpleSpan::new(0, 0))),
    };
    let ctx = ctx.with_proposition(make_int_prop("_", x_gt_y));
    let ctx = ctx.with_proposition(make_int_prop("y", make_comparison("y", BinOp::Gt, 5)));

    let goal = make_int_prop("x", make_comparison("x", BinOp::Gt, 5));
    assert_provable(&ctx, &goal);
}
#[test]

fn test_smt_tautology() {
    let ctx = TypingContext::new();

    let goal_expr = Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((Expr::Literal(Literal::Int(5)), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(5)), SimpleSpan::new(0, 0))),
    };
    let goal = make_int_prop("_", goal_expr);

    assert_provable(&ctx, &goal);
}
