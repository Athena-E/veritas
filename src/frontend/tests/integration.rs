use crate::frontend::parser::program_parser;
use chumsky::prelude::*;
use super::common::parse_tokens;

#[test]
fn test_complete_program() {
    let src = r#"
        fn is_positive(n: int) {
            let result: bool = n > 0;
        }

        fn main() {
            let x: int = 42;
            let y: int = x + 10;
            let flag: bool = is_positive(x);
        }
    "#;
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_array_operations() {
    let src = r#"
        fn array_test() {
            let arr: [int; 10] = create_array();
            let first: int = arr[0];
            let matrix: [[int; 5]; 5] = create_matrix();
            let elem: int = matrix[2][3];
        }
    "#;
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_refinement_types() {
    let src = r#"
        fn bounded(n: {x: int | x >= 0 && x <= 100}) {
            let value: int = n;
        }
    "#;
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}

#[test]
fn test_complex_expressions_in_statements() {
    let src = r#"
        fn complex() {
            let aaa: int = 10;
            let bbb: int = 20;
            let ccc: int = 30;
            let ddd: int = 40;
            let result: int = (aaa + bbb) * ccc - ddd;
            let xxx: int = 50;
            let yyy: int = 60;
            let zzz: int = 70;
            let cond: bool = xxx > 0 && yyy < 100 || zzz == 50;
            let ff: bool = true;
            let gg: bool = false;
            let negated: bool = !(ff && gg);
        }
    "#;
    let tokens = parse_tokens(src);
    let result = program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result();
    assert!(result.is_ok());
}
