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
        if let Stmt::Let { is_mut, name, .. } = &func.statements[0].0 {
            assert_eq!(*is_mut, false);
            assert_eq!(*name, "x");
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
        if let Stmt::Let { is_mut, name, .. } = &func.statements[0].0 {
            assert_eq!(*is_mut, true);
            assert_eq!(*name, "y");
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
        assert_eq!(func.statements.len(), 2);
        assert!(matches!(func.statements[1].0, Stmt::Assignment { .. }));
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
