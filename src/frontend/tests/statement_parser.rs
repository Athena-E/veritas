use crate::common::ast::Stmt;
use crate::frontend::parser::function_parser;
use chumsky::prelude::*;
use super::common::parse_tokens;

#[test]
fn test_let_statement_immutable() {
    let src = "fn test() { let x: int = 5; }";
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
        if let Stmt::Let { is_mut, name, .. } = &func.body.statements[0].0 {
            assert_eq!(is_mut, &false);
            assert_eq!(name, &"x");
        } else {
            panic!("Expected Let statement");
        }
    }
}

#[test]
fn test_let_statement_mutable() {
    let src = "fn test() { let mut y: bool = true; }";
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
        if let Stmt::Let { is_mut, name, .. } = &func.body.statements[0].0 {
            assert_eq!(is_mut, &true);
            assert_eq!(name, &"y");
        } else {
            panic!("Expected Let statement");
        }
    }
}

#[test]
fn test_assignment_statement() {
    let src = "fn test() { let mut x: int = 0; x = 10; }";
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
        assert!(matches!(func.body.statements[1].0, Stmt::Assignment { .. }));
    }
}

#[test]
fn test_assignment_to_array_element() {
    let src = "fn test() { let mut arr: [int; 5] = init(); arr[0] = 42; }";
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
fn test_simple_for_loop() {
    let src = "fn test() { for i in 0..10 { let x: int = i; } }";
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
        assert_eq!(func.body.statements.len(), 1);
        if let Stmt::For { var, body, .. } = &func.body.statements[0].0 {
            assert_eq!(var, &"i");
            assert_eq!(body.len(), 1);
        } else {
            panic!("Expected For statement");
        }
    }
}

#[test]
fn test_for_loop_with_variable_range() {
    let src = "fn test() { for j in start..end { j = j + 1; } }";
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
fn test_for_loop_with_multiple_statements() {
    let src = "fn test() { for i in 0..5 { let x: int = i; let y: int = x * 2; x = y; } }";
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
        if let Stmt::For { body, .. } = &func.body.statements[0].0 {
            assert_eq!(body.len(), 3);
        }
    }
}

#[test]
fn test_nested_for_loops() {
    let src = "fn test() { for i in 0..3 { for j in 0..3 { let x: int = i + j; } } }";
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
fn test_empty_for_loop() {
    let src = "fn test() { for k in 0..10 { } }";
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
fn test_let_statement_with_array_init() {
    let src = "fn test() { let arr: [int; 10] = [0; 10]; }";
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
        if let Stmt::Let { name, value, .. } = &func.body.statements[0].0 {
            assert_eq!(name, &"arr");
            assert!(matches!(value.0, crate::common::ast::Expr::ArrayInit { .. }));
        } else {
            panic!("Expected Let statement");
        }
    }
}
