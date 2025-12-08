use super::common::parse_tokens;
use crate::common::ast::Type;
use crate::frontend::parser::type_parser;
use chumsky::prelude::*;

#[test]
fn test_type_parser_int() {
    let src = "int";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::Int));
    }
}

#[test]
fn test_type_parser_bool() {
    let src = "bool";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::Bool));
    }
}

#[test]
fn test_type_parser_array() {
    let src = "[int; 10]";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::Array { .. }));
    }
}

#[test]
fn test_type_parser_nested_array() {
    let src = "[[int; 5]; 10]";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_type_parser_singleton_int() {
    let src = "int(42)";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::SingletonInt(_)));
    }
}

#[test]
fn test_type_parser_refined_int() {
    let src = "{x: int | x > 0}";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        if let Type::RefinedInt { var, .. } = ty {
            assert_eq!(var, "x");
        } else {
            panic!("Expected RefinedInt type");
        }
    }
}

#[test]
fn test_type_parser_refined_int_complex_predicate() {
    let src = "{n: int | n >= 0 && n <= 100}";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_type_parser_reference() {
    let src = "&int";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::Ref(_)));
    }
}

#[test]
fn test_type_parser_mutable_reference() {
    let src = "&mut int";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok((ty, _)) = result {
        assert!(matches!(ty, Type::RefMut(_)));
    }
}

#[test]
fn test_type_parser_nested_reference() {
    let src = "&mut &int";
    let tokens = parse_tokens(src);
    let result = type_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}
