use super::common::parse_tokens;
use crate::common::ast::Stmt;
use crate::frontend::parser::function_parser;
use chumsky::prelude::*;

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
    if let Ok((func, _)) = result
        && let Stmt::For { body, .. } = &func.body.statements[0].0
    {
        assert_eq!(body.len(), 3);
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
            assert!(matches!(
                value.0,
                crate::common::ast::Expr::ArrayInit { .. }
            ));
        } else {
            panic!("Expected Let statement");
        }
    }
}

#[test]
fn test_array_indexing_assignment_with_literal_index() {
    let src = "fn test() { let mut arr: [int; 5] = [0; 5]; arr[2] = 100; }";
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
        if let Stmt::Assignment { lhs, .. } = &func.body.statements[1].0 {
            assert!(matches!(lhs.0, crate::common::ast::Expr::Index { .. }));
        } else {
            panic!("Expected Assignment statement");
        }
    }
}

#[test]
fn test_array_indexing_assignment_with_variable_index() {
    let src = "fn test() { let mut arr: [int; 10] = [0; 10]; arr[i] = value; }";
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
        if let Stmt::Assignment { lhs, rhs } = &func.body.statements[1].0 {
            if let crate::common::ast::Expr::Index { base, index } = &lhs.0 {
                assert!(matches!(base.0, crate::common::ast::Expr::Variable("arr")));
                assert!(matches!(index.0, crate::common::ast::Expr::Variable("i")));
            } else {
                panic!("Expected Index expression");
            }
            assert!(matches!(rhs.0, crate::common::ast::Expr::Variable("value")));
        } else {
            panic!("Expected Assignment statement");
        }
    }
}

#[test]
fn test_array_indexing_assignment_with_expression_value() {
    let src = "fn test() { let mut arr: [int; 5] = [0; 5]; arr[0] = x + y * 2; }";
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
        if let Stmt::Assignment { rhs, .. } = &func.body.statements[1].0 {
            assert!(matches!(rhs.0, crate::common::ast::Expr::BinOp { .. }));
        } else {
            panic!("Expected Assignment statement");
        }
    }
}

#[test]
fn test_array_indexing_assignment_with_expression_index() {
    let src = "fn test() { let mut arr: [int; 10] = [0; 10]; arr[i + 1] = 42; }";
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
        if let Stmt::Assignment { lhs, .. } = &func.body.statements[1].0 {
            if let crate::common::ast::Expr::Index { index, .. } = &lhs.0 {
                assert!(matches!(index.0, crate::common::ast::Expr::BinOp { .. }));
            } else {
                panic!("Expected Index expression");
            }
        } else {
            panic!("Expected Assignment statement");
        }
    }
}

#[test]
fn test_nested_array_indexing_assignment() {
    let src = "fn test() { let mut matrix: [[int; 3]; 3] = init(); matrix[i][j] = 99; }";
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
        if let Stmt::Assignment { lhs, .. } = &func.body.statements[1].0 {
            // Should be Index { base: Index { ... }, index: ... }
            if let crate::common::ast::Expr::Index { base, .. } = &lhs.0 {
                assert!(matches!(base.0, crate::common::ast::Expr::Index { .. }));
            } else {
                panic!("Expected nested Index expression");
            }
        } else {
            panic!("Expected Assignment statement");
        }
    }
}
