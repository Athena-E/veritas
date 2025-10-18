use crate::frontend::parser::function_parser;
use chumsky::prelude::*;
use super::common::parse_tokens;

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
        assert_eq!(func.statements.len(), 1);
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
        assert_eq!(func.statements.len(), 0);
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
        assert_eq!(func.statements.len(), 3);
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
