// Tests for TypingContext

use super::common::*;
use crate::common::ast::BinOp;
use crate::common::types::{FunctionSignature, IType};
use crate::frontend::typechecker::{TypingContext, VarBinding};

#[test]
fn test_context_new() {
    let ctx = TypingContext::new();
    assert!(ctx.get_propositions().is_empty());
    assert!(ctx.lookup_immutable("x").is_none());
    assert!(ctx.lookup_mutable("x").is_none());
    assert!(ctx.lookup_function("foo").is_none());
}

#[test]
fn test_context_with_immutable() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_immutable("x".to_string(), IType::Int);

    assert!(ctx.lookup_immutable("x").is_some());
    assert_eq!(format!("{}", ctx.lookup_immutable("x").unwrap()), "int");
    assert!(ctx.lookup_immutable("y").is_none());
}

#[test]
fn test_context_with_mutable() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_mutable("x".to_string(), IType::Int, IType::Int);

    let binding = ctx.lookup_mutable("x");
    assert!(binding.is_some());
    let binding = binding.unwrap();
    assert_eq!(format!("{}", binding.current_type), "int");
    assert_eq!(format!("{}", binding.master_type), "int");
}

#[test]
fn test_context_update_mutable() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_mutable("x".to_string(), IType::Int, IType::Int);

    // Update to a refined type
    let refined = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    let ctx = ctx.with_mutable_update("x", refined.clone()).unwrap();

    let binding = ctx.lookup_mutable("x").unwrap();
    assert_eq!(format!("{}", binding.current_type), "{x: int | x > 0}");
    assert_eq!(format!("{}", binding.master_type), "int"); // Master unchanged
}

#[test]
fn test_context_lookup_variable() {
    let ctx = TypingContext::new();
    let ctx = ctx.with_immutable("x".to_string(), IType::Int);
    let ctx = ctx.with_mutable("y".to_string(), IType::Bool, IType::Bool);

    match ctx.lookup_var("x") {
        Some(VarBinding::Immutable(ty)) => {
            assert_eq!(format!("{}", ty), "int");
        }
        _ => panic!("Expected immutable variable binding"),
    }

    match ctx.lookup_var("y") {
        Some(VarBinding::Mutable(binding)) => {
            assert_eq!(format!("{}", binding.current_type), "bool");
        }
        _ => panic!("Expected mutable variable binding"),
    }
}

#[test]
fn test_context_with_proposition() {
    let ctx = TypingContext::new();
    let prop = make_int_prop("x", make_comparison("x", BinOp::Gt, 0));
    let ctx = ctx.with_proposition(prop);

    assert_eq!(ctx.get_propositions().len(), 1);
    assert_eq!(format!("{}", &ctx.get_propositions()[0]), "x > 0");
}

#[test]
fn test_context_cloning_is_independent() {
    let ctx1 = TypingContext::new();
    let ctx1 = ctx1.with_immutable("x".to_string(), IType::Int);

    // Clone and add different variable
    let ctx2 = ctx1.with_immutable("y".to_string(), IType::Bool);

    // ctx1 should not have y
    assert!(ctx1.lookup_immutable("x").is_some());
    assert!(ctx1.lookup_immutable("y").is_none());

    // ctx2 should have both
    assert!(ctx2.lookup_immutable("x").is_some());
    assert!(ctx2.lookup_immutable("y").is_some());
}

#[test]
fn test_context_with_function() {
    use chumsky::prelude::SimpleSpan;
    use im::HashMap;

    let sig = FunctionSignature {
        name: "foo".to_string(),
        parameters: vec![
            ("x".to_string(), IType::Int),
            ("y".to_string(), IType::Bool),
        ],
        return_type: IType::Unit,
        precondition: None,
        postcondition: None,
        span: SimpleSpan::new(0, 0),
    };

    let mut functions = HashMap::new();
    functions.insert("foo".to_string(), sig);
    let ctx = TypingContext::with_functions(functions);

    let retrieved = ctx.lookup_function("foo");
    assert!(retrieved.is_some());
    let sig = retrieved.unwrap();
    assert_eq!(sig.parameters.len(), 2);
    assert_eq!(format!("{}", sig.parameters[0].1), "int");
}
