// Unit tests for checked constant folding (Phase 2 of overflow verification)

use crate::common::ast::BinOp;
use crate::common::types::{IType, IValue};
use crate::frontend::typechecker::TypingContext;
use crate::frontend::typechecker::error::TypeError;
use crate::frontend::typechecker::helpers::{check_const_fold_overflow, checked_fold, join_op};
use chumsky::prelude::SimpleSpan;

fn sing(n: i64) -> IType<'static> {
    IType::SingletonInt(IValue::Int(n))
}

fn span() -> SimpleSpan {
    SimpleSpan::new(0, 0)
}

// ------- checked_fold ----------------------------------------------------

#[test]
fn checked_fold_add_no_overflow() {
    assert_eq!(checked_fold(BinOp::Add, 1, 2), Some(3));
}

#[test]
fn checked_fold_add_overflow() {
    assert_eq!(checked_fold(BinOp::Add, i64::MAX, 1), None);
}

#[test]
fn checked_fold_sub_overflow() {
    assert_eq!(checked_fold(BinOp::Sub, i64::MIN, 1), None);
}

#[test]
fn checked_fold_mul_overflow() {
    assert_eq!(checked_fold(BinOp::Mul, i64::MIN, -1), None);
    assert_eq!(checked_fold(BinOp::Mul, i64::MAX, 2), None);
}

#[test]
fn checked_fold_div_by_zero() {
    assert_eq!(checked_fold(BinOp::Div, 1, 0), None);
}

#[test]
fn checked_fold_div_int_min_neg_one() {
    // INT_MIN / -1 overflows because the result (+INT_MIN+1) doesn't fit
    assert_eq!(checked_fold(BinOp::Div, i64::MIN, -1), None);
}

#[test]
fn checked_fold_shl_in_range() {
    assert_eq!(checked_fold(BinOp::Shl, 1, 4), Some(16));
}

#[test]
fn checked_fold_shl_count_out_of_range() {
    assert_eq!(checked_fold(BinOp::Shl, 1, 64), None);
    assert_eq!(checked_fold(BinOp::Shl, 1, -1), None);
}

#[test]
fn checked_fold_bitwise_always_safe() {
    assert_eq!(checked_fold(BinOp::BitAnd, i64::MAX, i64::MIN), Some(0));
    assert_eq!(checked_fold(BinOp::BitOr, i64::MAX, 0), Some(i64::MAX));
    assert_eq!(checked_fold(BinOp::BitXor, -1, -1), Some(0));
}

// ------- join_op -----------------------------------------------------------

#[test]
fn join_op_add_overflow_widens_to_int() {
    // Previously this would wrap silently to a SingletonInt(INT_MIN).
    // Now it must fall back to IType::Int so the refinement path takes over.
    let ty = join_op(BinOp::Add, &sing(i64::MAX), &sing(1));
    assert!(
        matches!(ty, IType::Int),
        "expected IType::Int on overflow, got {:?}",
        ty
    );
}

#[test]
fn join_op_mul_overflow_widens_to_int() {
    let ty = join_op(BinOp::Mul, &sing(i64::MAX), &sing(2));
    assert!(matches!(ty, IType::Int));
}

#[test]
fn join_op_add_in_range_still_folds() {
    let ty = join_op(BinOp::Add, &sing(5), &sing(7));
    assert!(matches!(ty, IType::SingletonInt(IValue::Int(12))));
}

#[test]
fn join_op_shl_out_of_range_widens_to_int() {
    // Previously would panic in debug mode; must now widen.
    let ty = join_op(BinOp::Shl, &sing(1), &sing(64));
    assert!(matches!(ty, IType::Int));
}

// ------- check_const_fold_overflow -----------------------------------------

#[test]
fn overflow_check_disabled_is_noop() {
    let ctx = TypingContext::new();
    assert!(!ctx.check_overflow);
    // Even a clearly overflowing fold should not raise when flag is off.
    let res = check_const_fold_overflow(&ctx, BinOp::Add, &sing(i64::MAX), &sing(1), span());
    assert!(res.is_ok(), "Phase 2 must be opt-in");
}

#[test]
fn overflow_check_enabled_rejects_add_max_plus_one() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Add, &sing(i64::MAX), &sing(1), span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "+"),
        other => panic!("expected IntegerOverflow(+), got {:?}", other),
    }
}

#[test]
fn overflow_check_enabled_rejects_sub_min_minus_one() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Sub, &sing(i64::MIN), &sing(1), span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "-"),
        other => panic!("expected IntegerOverflow(-), got {:?}", other),
    }
}

#[test]
fn overflow_check_enabled_rejects_mul_min_times_neg_one() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Mul, &sing(i64::MIN), &sing(-1), span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "*"),
        other => panic!("expected IntegerOverflow(*), got {:?}", other),
    }
}

#[test]
fn overflow_check_enabled_rejects_div_min_by_neg_one() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Div, &sing(i64::MIN), &sing(-1), span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "/"),
        other => panic!("expected IntegerOverflow(/), got {:?}", other),
    }
}

#[test]
fn overflow_check_enabled_rejects_shl_out_of_range() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Shl, &sing(1), &sing(64), span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "<<"),
        other => panic!("expected IntegerOverflow(<<), got {:?}", other),
    }
}

#[test]
fn overflow_check_leaves_in_range_alone() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    assert!(
        check_const_fold_overflow(&ctx, BinOp::Add, &sing(3), &sing(4), span()).is_ok()
    );
    assert!(
        check_const_fold_overflow(&ctx, BinOp::Mul, &sing(1000), &sing(1000), span()).is_ok()
    );
}

#[test]
fn overflow_check_ignores_bitwise_and_or_xor() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    // These never overflow on i64 — helper must short-circuit.
    assert!(
        check_const_fold_overflow(&ctx, BinOp::BitAnd, &sing(i64::MAX), &sing(i64::MIN), span())
            .is_ok()
    );
    assert!(
        check_const_fold_overflow(&ctx, BinOp::BitOr, &sing(i64::MAX), &sing(i64::MAX), span())
            .is_ok()
    );
    assert!(
        check_const_fold_overflow(&ctx, BinOp::BitXor, &sing(i64::MAX), &sing(i64::MIN), span())
            .is_ok()
    );
}

#[test]
fn overflow_check_ignores_nonsingleton_operands() {
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    // Phase 2 only sees concrete folds; symbolic operands are Phase 3's job.
    assert!(check_const_fold_overflow(&ctx, BinOp::Add, &IType::Int, &sing(1), span()).is_ok());
}

#[test]
fn overflow_check_divides_by_zero_passes_through() {
    // Divide-by-zero is a separate error class (DivisionByZero); the overflow
    // helper must not mask it.
    let mut ctx = TypingContext::new();
    ctx.check_overflow = true;
    let res = check_const_fold_overflow(&ctx, BinOp::Div, &sing(5), &sing(0), span());
    assert!(res.is_ok(), "divide-by-zero must not be reported as IntegerOverflow");
}
