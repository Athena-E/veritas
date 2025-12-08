use super::common::parse_tokens;
use crate::common::ast::Type;
use crate::frontend::parser::function_parser;
use chumsky::prelude::*;

#[test]
fn test_simple_function() {
    let src = "fn main() { let x: int = 42; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "main");
        assert_eq!(func.body.statements.len(), 1);
        assert!(func.body.return_expr.is_none());
    }
}

#[test]
fn test_function_with_parameters() {
    let src = "fn add(x: int, y: int) { let sum: int = x + y; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "add");
        assert_eq!(func.parameters.len(), 2);
        assert_eq!(func.parameters[0].0.name, "x");
        assert_eq!(func.parameters[1].0.name, "y");
    }
}

#[test]
fn test_function_with_trailing_comma() {
    let src = "fn test(a: int, b: bool,) { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_empty_function() {
    let src = "fn empty() { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "empty");
        assert_eq!(func.body.statements.len(), 0);
        assert!(func.body.return_expr.is_none());
    }
}

#[test]
fn test_function_with_multiple_statements() {
    let src = "fn compute() { let x: int = 10; let y: int = 20; x = x + y; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.body.statements.len(), 3);
    }
}

#[test]
fn test_function_with_mutable_parameter() {
    let src = "fn mutate(x: &mut int) { x = 42; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_function_with_array_parameter() {
    let src = "fn process(arr: [int; 10]) { let x: int = arr[0]; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_function_with_refined_type_parameter() {
    let src = "fn positive(n: {x: int | x > 0}) { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_function_with_int_return_type() {
    let src = "fn add(x: int, y: int) -> int { let sum: int = x + y; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "add");
        assert!(matches!(func.return_type.0, Type::Int));
    }
}

#[test]
fn test_function_with_bool_return_type() {
    let src = "fn is_positive(x: int) -> bool { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "is_positive");
        assert!(matches!(func.return_type.0, Type::Bool));
    }
}

#[test]
fn test_function_without_return_type_defaults_to_unit() {
    let src = "fn main() { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "main");
        assert!(matches!(func.return_type.0, Type::Unit));
    }
}

#[test]
fn test_function_with_array_return_type() {
    let src = "fn create_array() -> [int; 10] { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "create_array");
        assert!(matches!(func.return_type.0, Type::Array { .. }));
    }
}

#[test]
fn test_function_with_reference_return_type() {
    let src = "fn get_ref(x: &int) -> &int { }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "get_ref");
        assert!(matches!(func.return_type.0, Type::Ref(_)));
    }
}

#[test]
fn test_function_with_explicit_return() {
    let src = "fn add(x: int, y: int) -> int { return x + y; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "add");
        assert!(matches!(func.return_type.0, Type::Int));
        assert_eq!(func.body.statements.len(), 1);
        assert!(func.body.return_expr.is_none());
    }
}

#[test]
fn test_function_with_implicit_return() {
    let src = "fn add(x: int, y: int) -> int { x + y }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "add");
        assert!(matches!(func.return_type.0, Type::Int));
        assert_eq!(func.body.statements.len(), 0);
        assert!(func.body.return_expr.is_some());
    }
}

#[test]
fn test_function_with_statements_and_implicit_return() {
    let src = "fn compute(x: int) -> int { let y: int = x * 2; y + 10 }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.name, "compute");
        assert!(matches!(func.return_type.0, Type::Int));
        assert_eq!(func.body.statements.len(), 1);
        assert!(func.body.return_expr.is_some());
    }
}

#[test]
fn test_function_with_early_return() {
    let src = "fn check(x: int) -> int { return 0; }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_function_with_expression_statement() {
    let src = "fn call_something() { foo(); bar(); }";
    let tokens = parse_tokens(src);
    let result = function_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((func, _)) = result {
        assert_eq!(func.body.statements.len(), 2);
        assert!(func.body.return_expr.is_none());
    }
}
