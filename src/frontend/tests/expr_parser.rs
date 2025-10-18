use crate::common::ast::{Expr, Literal};
use crate::frontend::parser::expr_parser_for_types;
use chumsky::prelude::*;
use super::common::parse_tokens;

#[test]
fn test_expr_parser_integer_literal() {
    let src = "42";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::Literal(Literal::Int(42))));
    }
}

#[test]
fn test_expr_parser_bool_literals() {
    let src = "true";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::Literal(Literal::Bool(true))));
    }

    let src = "false";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::Literal(Literal::Bool(false))));
    }
}

#[test]
fn test_expr_parser_variable() {
    let src = "x";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::Variable("x")));
    }
}

#[test]
fn test_expr_parser_addition() {
    let src = "1 + 2";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::BinOp { .. }));
    }
}

#[test]
fn test_expr_parser_subtraction() {
    let src = "10 - 5";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_multiplication() {
    let src = "3 * 4";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_precedence() {
    let src = "1 + 2 * 3";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    // Should parse as 1 + (2 * 3)
}

#[test]
fn test_expr_parser_parentheses() {
    let src = "(1 + 2) * 3";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_comparison() {
    let src = "x > 5";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_equality() {
    let src = "x == 42";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_logical_and() {
    let src = "x > 0 && x < 10";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_logical_or() {
    let src = "x == 0 || y == 1";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_unary_not() {
    let src = "!true";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::UnaryOp { .. }));
    }
}

#[test]
fn test_expr_parser_double_negation() {
    let src = "!!x";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_function_call_no_args() {
    let src = "foo()";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        if let Expr::Call { func_name, args } = expr {
            assert_eq!(func_name, "foo");
            assert_eq!(args.0.len(), 0);
        } else {
            panic!("Expected Call expression");
        }
    }
}

#[test]
fn test_expr_parser_function_call_single_arg() {
    let src = "bar(42)";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        if let Expr::Call { func_name, args } = expr {
            assert_eq!(func_name, "bar");
            assert_eq!(args.0.len(), 1);
        } else {
            panic!("Expected Call expression");
        }
    }
}

#[test]
fn test_expr_parser_function_call_multiple_args() {
    let src = "add(1, 2, 3)";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        if let Expr::Call { func_name, args } = expr {
            assert_eq!(func_name, "add");
            assert_eq!(args.0.len(), 3);
        } else {
            panic!("Expected Call expression");
        }
    }
}

#[test]
fn test_expr_parser_function_call_with_trailing_comma() {
    let src = "func(x, y,)";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_array_indexing() {
    let src = "arr[0]";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((expr, _)) = result {
        assert!(matches!(expr, Expr::Index { .. }));
    }
}

#[test]
fn test_expr_parser_nested_array_indexing() {
    let src = "matrix[i][j]";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_expr_parser_complex_expression() {
    let src = "(x + y) * 2 > threshold && !flag";
    let tokens = parse_tokens(src);
    let result = expr_parser_for_types()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}
