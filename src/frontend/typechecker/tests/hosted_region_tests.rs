use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::{TypeError, check_program};
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
fn hosted_target_rejects_passing_arrays_to_calls() {
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
    let err = check_program(&program).expect_err("hosted target should reject array call escape");

    match err {
        TypeError::UnsupportedFeature { feature, .. } => {
            assert!(feature.contains("passing arrays to function calls on the hosted target"));
        }
        other => panic!("expected UnsupportedFeature, got {other:?}"),
    }
}

