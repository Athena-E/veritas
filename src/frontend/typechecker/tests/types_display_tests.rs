// Tests for Display trait implementations on semantic types

use crate::common::types::{FunctionSignature, IType, IValue};
use super::common::*;
use crate::common::ast::BinOp;
use std::sync::Arc;

#[test]
fn test_display_basic_types() {
    assert_eq!(format!("{}", IType::Int), "int");
    assert_eq!(format!("{}", IType::Bool), "bool");
    assert_eq!(format!("{}", IType::Unit), "unit");
}

#[test]
fn test_display_array() {
    let ty = IType::Array {
        element_type: Arc::new(IType::Int),
        size: IValue::Int(10),
    };
    assert_eq!(format!("{}", ty), "int[10]");

    let ty_symbolic = IType::Array {
        element_type: Arc::new(IType::Bool),
        size: IValue::Symbolic("n".to_string()),
    };
    assert_eq!(format!("{}", ty_symbolic), "bool[n]");
}

#[test]
fn test_display_references() {
    let ref_ty = IType::Ref(Arc::new(IType::Int));
    assert_eq!(format!("{}", ref_ty), "&int");

    let refmut_ty = IType::RefMut(Arc::new(IType::Bool));
    assert_eq!(format!("{}", refmut_ty), "&mut bool");
}

#[test]
fn test_display_refined_int() {
    let ty = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    assert_eq!(format!("{}", ty), "{x: int | x > 0}");

    let ty_eq = make_refined_int("n", make_comparison("n", BinOp::Eq, 42));
    assert_eq!(format!("{}", ty_eq), "{n: int | n == 42}");
}

#[test]
fn test_display_master_type() {
    let ty = IType::Master(Arc::new(IType::Int));
    assert_eq!(format!("{}", ty), "M(int)");

    let refined = make_refined_int("x", make_comparison("x", BinOp::Gte, 0));
    let master_refined = IType::Master(Arc::new(refined));
    assert_eq!(format!("{}", master_refined), "M({x: int | x >= 0})");
}

#[test]
fn test_display_function_signature() {
    let sig = FunctionSignature {
        params: vec![IType::Int],
        return_type: IType::Bool,
    };
    assert_eq!(format!("{}", sig), "(int) -> bool");

    let sig_multiple = FunctionSignature {
        params: vec![IType::Int, IType::Bool, IType::Unit],
        return_type: IType::Int,
    };
    assert_eq!(format!("{}", sig_multiple), "(int, bool, unit) -> int");

    let sig_no_params = FunctionSignature {
        params: vec![],
        return_type: IType::Bool,
    };
    assert_eq!(format!("{}", sig_no_params), "() -> bool");
}

#[test]
fn test_display_proposition() {
    let prop = make_int_prop("x", make_comparison("x", BinOp::Gt, 5));
    assert_eq!(format!("{}", prop), "x > 5");

    let prop_eq = make_int_prop("z", make_comparison("z", BinOp::Eq, 0));
    assert_eq!(format!("{}", prop_eq), "z == 0");
}

#[test]
fn test_display_nested_types() {
    // &mut {x: int | x > 0}
    let refined = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    let refmut = IType::RefMut(Arc::new(refined));
    assert_eq!(format!("{}", refmut), "&mut {x: int | x > 0}");

    // int[n][] - nested array
    let inner = IType::Array {
        element_type: Arc::new(IType::Int),
        size: IValue::Symbolic("n".to_string()),
    };
    let outer = IType::Array {
        element_type: Arc::new(inner),
        size: IValue::Int(5),
    };
    assert_eq!(format!("{}", outer), "int[n][5]");
}
