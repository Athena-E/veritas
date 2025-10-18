use crate::frontend::parser::program_parser;
use chumsky::prelude::*;
use super::common::parse_tokens;

#[test]
fn test_program_single_function() {
    let src = "fn main() { let x: int = 1; }";
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok(program) = result {
        assert_eq!(program.functions.len(), 1);
    }
}

#[test]
fn test_program_multiple_functions() {
    let src = "fn foo() { } fn bar() { let x: int = 1; } fn baz(n: int) { }";
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
    if let Ok(program) = result {
        assert_eq!(program.functions.len(), 3);
        assert_eq!(program.functions[0].0.name, "foo");
        assert_eq!(program.functions[1].0.name, "bar");
        assert_eq!(program.functions[2].0.name, "baz");
    }
}
