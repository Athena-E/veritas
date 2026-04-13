// Unit tests for checked constant folding (Phase 2 of overflow verification)

use crate::common::ast::BinOp;
use crate::common::types::{IType, IValue};
use crate::frontend::typechecker::error::TypeError;
use crate::frontend::typechecker::helpers::{
    check_const_fold_overflow, checked_fold, checked_fold_in_range, join_op,
};
use chumsky::prelude::SimpleSpan;

const I64_LO: i128 = i64::MIN as i128;
const I64_HI: i128 = i64::MAX as i128;

fn sing(n: i128) -> IType<'static> {
    IType::SingletonInt(IValue::Int(n))
}

fn span() -> SimpleSpan {
    SimpleSpan::new(0, 0)
}

// ------- checked_fold (i128-level) -------------------------------------------

#[test]
fn checked_fold_add_no_overflow() {
    assert_eq!(checked_fold(BinOp::Add, 1, 2), Some(3));
}

#[test]
fn checked_fold_div_by_zero() {
    assert_eq!(checked_fold(BinOp::Div, 1, 0), None);
}

#[test]
fn checked_fold_shl_in_range() {
    assert_eq!(checked_fold(BinOp::Shl, 1, 4), Some(16));
}

#[test]
fn checked_fold_bitwise_always_safe() {
    assert_eq!(checked_fold(BinOp::BitAnd, I64_HI, I64_LO), Some(0));
    assert_eq!(checked_fold(BinOp::BitOr, I64_HI, 0), Some(I64_HI));
    assert_eq!(checked_fold(BinOp::BitXor, -1, -1), Some(0));
}

// ------- checked_fold_in_range (i64-bounded) ---------------------------------

#[test]
fn checked_fold_in_range_add_overflow() {
    assert_eq!(
        checked_fold_in_range(BinOp::Add, I64_HI, 1, I64_LO, I64_HI),
        None
    );
}

#[test]
fn checked_fold_in_range_sub_overflow() {
    assert_eq!(
        checked_fold_in_range(BinOp::Sub, I64_LO, 1, I64_LO, I64_HI),
        None
    );
}

#[test]
fn checked_fold_in_range_mul_overflow() {
    assert_eq!(
        checked_fold_in_range(BinOp::Mul, I64_LO, -1, I64_LO, I64_HI),
        None
    );
    assert_eq!(
        checked_fold_in_range(BinOp::Mul, I64_HI, 2, I64_LO, I64_HI),
        None
    );
}

#[test]
fn checked_fold_in_range_div_int_min_neg_one() {
    assert_eq!(
        checked_fold_in_range(BinOp::Div, I64_LO, -1, I64_LO, I64_HI),
        None
    );
}

#[test]
fn checked_fold_in_range_shl_count_out_of_range() {
    assert_eq!(
        checked_fold_in_range(BinOp::Shl, 1, 64, I64_LO, I64_HI),
        None
    );
    assert_eq!(
        checked_fold_in_range(BinOp::Shl, 1, -1, I64_LO, I64_HI),
        None
    );
}

#[test]
fn checked_fold_in_range_add_safe() {
    assert_eq!(
        checked_fold_in_range(BinOp::Add, 3, 4, I64_LO, I64_HI),
        Some(7)
    );
}

// ------- join_op -----------------------------------------------------------

#[test]
fn join_op_add_in_range_still_folds() {
    let ty = join_op(BinOp::Add, &sing(5), &sing(7));
    assert!(matches!(ty, IType::SingletonInt(IValue::Int(12))));
}

#[test]
fn join_op_shl_out_of_range_widens_to_int() {
    // Shift count >= 128 causes i128 checked_shl to fail → widens
    let ty = join_op(BinOp::Shl, &sing(1), &sing(128));
    assert!(matches!(ty, IType::Int));
}

// ------- check_const_fold_overflow (i64 bounds) ------------------------------

#[test]
fn overflow_check_rejects_add_max_plus_one() {
    let res =
        check_const_fold_overflow(BinOp::Add, &sing(I64_HI), &sing(1), I64_LO, I64_HI, span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "+"),
        other => panic!("expected IntegerOverflow(+), got {:?}", other),
    }
}

#[test]
fn overflow_check_rejects_sub_min_minus_one() {
    let res =
        check_const_fold_overflow(BinOp::Sub, &sing(I64_LO), &sing(1), I64_LO, I64_HI, span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "-"),
        other => panic!("expected IntegerOverflow(-), got {:?}", other),
    }
}

#[test]
fn overflow_check_rejects_mul_min_times_neg_one() {
    let res =
        check_const_fold_overflow(BinOp::Mul, &sing(I64_LO), &sing(-1), I64_LO, I64_HI, span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "*"),
        other => panic!("expected IntegerOverflow(*), got {:?}", other),
    }
}

#[test]
fn overflow_check_rejects_div_min_by_neg_one() {
    let res =
        check_const_fold_overflow(BinOp::Div, &sing(I64_LO), &sing(-1), I64_LO, I64_HI, span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "/"),
        other => panic!("expected IntegerOverflow(/), got {:?}", other),
    }
}

#[test]
fn overflow_check_rejects_shl_out_of_range() {
    let res = check_const_fold_overflow(BinOp::Shl, &sing(1), &sing(64), I64_LO, I64_HI, span());
    match res {
        Err(TypeError::IntegerOverflow { op, .. }) => assert_eq!(op, "<<"),
        other => panic!("expected IntegerOverflow(<<), got {:?}", other),
    }
}

#[test]
fn overflow_check_leaves_in_range_alone() {
    assert!(
        check_const_fold_overflow(BinOp::Add, &sing(3), &sing(4), I64_LO, I64_HI, span()).is_ok()
    );
    assert!(
        check_const_fold_overflow(BinOp::Mul, &sing(1000), &sing(1000), I64_LO, I64_HI, span())
            .is_ok()
    );
}

#[test]
fn overflow_check_ignores_bitwise() {
    assert!(
        check_const_fold_overflow(
            BinOp::BitAnd,
            &sing(I64_HI),
            &sing(I64_LO),
            I64_LO,
            I64_HI,
            span()
        )
        .is_ok()
    );
    assert!(
        check_const_fold_overflow(
            BinOp::BitOr,
            &sing(I64_HI),
            &sing(I64_HI),
            I64_LO,
            I64_HI,
            span()
        )
        .is_ok()
    );
    assert!(
        check_const_fold_overflow(
            BinOp::BitXor,
            &sing(I64_HI),
            &sing(I64_LO),
            I64_LO,
            I64_HI,
            span()
        )
        .is_ok()
    );
}

#[test]
fn overflow_check_ignores_nonsingleton_operands() {
    assert!(
        check_const_fold_overflow(BinOp::Add, &IType::Int, &sing(1), I64_LO, I64_HI, span())
            .is_ok()
    );
}

#[test]
fn overflow_check_divides_by_zero_passes_through() {
    let res = check_const_fold_overflow(BinOp::Div, &sing(5), &sing(0), I64_LO, I64_HI, span());
    assert!(
        res.is_ok(),
        "divide-by-zero must not be reported as IntegerOverflow"
    );
}
