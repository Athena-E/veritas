use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::{TypeError, check_program, check_program_bare_metal};
use chumsky::prelude::*;

fn parse_program<'src>(src: &'src str) -> crate::common::ast::Program<'src> {
    let tokens = lexer().parse(src).into_result().unwrap();
    program_parser()
        .parse(
            tokens
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_result()
        .unwrap()
}

#[test]
fn hosted_target_allows_passing_arrays_to_calls() {
    let src = r#"
        fn consume(arr: [int; 4]) -> int {
            arr[0]
        }

        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            consume(arr)
        }
    "#;

    let program = parse_program(src);
    check_program(&program).expect("hosted target should allow non-escaping array calls");
}

#[test]
fn hosted_target_rejects_returning_arrays() {
    let src = r#"
        fn main() -> [int; 4] {
            [0; 4]
        }
    "#;

    let program = parse_program(src);
    let err = check_program(&program).expect_err("hosted target should reject array returns");

    match err {
        TypeError::UnsupportedFeature { feature, .. } => {
            assert!(feature.contains("returning arrays on the hosted target"));
        }
        other => panic!("expected UnsupportedFeature, got {other:?}"),
    }
}

#[test]
fn bare_metal_target_allows_array_calls() {
    let src = r#"
        fn consume(arr: [int; 4]) -> int {
            arr[0]
        }

        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            consume(arr)
        }
    "#;

    let program = parse_program(src);
    check_program_bare_metal(&program)
        .expect("bare-metal target should allow array arguments without hosted regions");
}
