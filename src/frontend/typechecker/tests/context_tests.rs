// Tests for TypingContext

use super::common::*;
use crate::common::ast::BinOp;
use crate::common::types::IValue;
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

// Tests for context joining (LUB computation)

#[test]
fn test_join_same_refined_types() {
    // {x: int | x > 0} ∨ {x: int | x > 0} should preserve the refinement
    let refined1 = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    let refined2 = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), refined1, IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", refined2).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // The result should be a refined type (disjunction of x > 0 || x > 0)
    match &binding.current_type {
        IType::RefinedInt { .. } => {} // Expected
        other => panic!("Expected RefinedInt, got {}", other),
    }
}

#[test]
fn test_join_different_refined_types() {
    // {x: int | x > 0} ∨ {x: int | x <= 0} should produce {x: int | x > 0 || x <= 0}
    let refined1 = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    let refined2 = make_refined_int("x", make_comparison("x", BinOp::Lte, 0));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), refined1, IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", refined2).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // The result should be a refined type with disjunction
    match &binding.current_type {
        IType::RefinedInt { prop, .. } => {
            // Verify we have a disjunctive predicate
            let pred_str = format!("{}", prop);
            assert!(
                pred_str.contains("||"),
                "Expected disjunction in predicate, got: {}",
                pred_str
            );
        }
        other => panic!("Expected RefinedInt, got {}", other),
    }
}

#[test]
fn test_join_refined_with_singleton() {
    // {x: int | x > 0} ∨ int(5) should produce {x: int | x > 0 || x == 5}
    let refined = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));
    let singleton = IType::SingletonInt(IValue::Int(5));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), refined, IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", singleton).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // The result should be a refined type with disjunction
    match &binding.current_type {
        IType::RefinedInt { prop, .. } => {
            // Verify we have a disjunctive predicate
            let pred_str = format!("{}", prop);
            assert!(
                pred_str.contains("||"),
                "Expected disjunction in predicate, got: {}",
                pred_str
            );
        }
        other => panic!("Expected RefinedInt, got {}", other),
    }
}

#[test]
fn test_join_refined_with_int_widens() {
    // {x: int | x > 0} ∨ int should widen to int
    let refined = make_refined_int("x", make_comparison("x", BinOp::Gt, 0));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), refined, IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", IType::Int).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // The result should be plain int (widened)
    match &binding.current_type {
        IType::Int => {} // Expected
        other => panic!("Expected Int, got {}", other),
    }
}

#[test]
fn test_proposition_intersection() {
    // Both branches have x > 0 => keep x > 0 after join
    let prop = make_int_prop("x", make_comparison("x", BinOp::Gt, 0));

    let ctx_base = TypingContext::new()
        .with_mutable("r".to_string(), IType::Int, IType::Int)
        .with_proposition(prop.clone());

    // Add same proposition to both branches
    let ctx1 = ctx_base.clone();
    let ctx2 = ctx_base.clone();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);

    // The proposition should be preserved
    assert_eq!(
        joined.get_propositions().len(),
        1,
        "Expected 1 proposition after intersection"
    );
}

#[test]
fn test_proposition_difference_cleared() {
    // Branch 1 has x > 0, Branch 2 has x < 0 => intersection is empty
    let prop1 = make_int_prop("x", make_comparison("x", BinOp::Gt, 0));
    let prop2 = make_int_prop("x", make_comparison("x", BinOp::Lt, 0));

    let ctx1 = TypingContext::new()
        .with_mutable("r".to_string(), IType::Int, IType::Int)
        .with_proposition(prop1);

    let ctx2 = TypingContext::new()
        .with_mutable("r".to_string(), IType::Int, IType::Int)
        .with_proposition(prop2);

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);

    // No propositions should remain (different propositions in each branch)
    assert_eq!(
        joined.get_propositions().len(),
        0,
        "Expected no propositions after intersection of different props"
    );
}

#[test]
fn test_join_different_singletons_to_int() {
    // int(5) ∨ int(10) should produce int
    let singleton1 = IType::SingletonInt(IValue::Int(5));
    let singleton2 = IType::SingletonInt(IValue::Int(10));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), singleton1, IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", singleton2).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // Different singletons should widen to int
    match &binding.current_type {
        IType::Int => {} // Expected
        other => panic!("Expected Int for different singletons, got {}", other),
    }
}

#[test]
fn test_join_same_singletons() {
    // int(5) ∨ int(5) should produce int(5)
    let singleton = IType::SingletonInt(IValue::Int(5));

    let ctx1 = TypingContext::new().with_mutable("r".to_string(), singleton.clone(), IType::Int);
    let ctx2 = ctx1.with_mutable_update("r", singleton.clone()).unwrap();

    let joined = TypingContext::join_mutable_contexts(&ctx1, &ctx2);
    let binding = joined.lookup_mutable("r").unwrap();

    // Same singletons should be preserved
    match &binding.current_type {
        IType::SingletonInt(IValue::Int(5)) => {} // Expected
        other => panic!("Expected SingletonInt(5), got {}", other),
    }
}
