use crate::backend::lower::lower_program;
use crate::backend::tir::{Terminator, TirInstr};
use crate::common::ownership::OwnershipMode;
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
fn hosted_target_allows_non_main_array_returns() {
    let src = r#"
        fn make_arr() -> [int; 4] {
            return [0; 4];
        }

        fn main() -> int {
            let arr: [int; 4] = make_arr();
            arr[0]
        }
    "#;

    let program = parse_program(src);
    check_program(&program).expect("hosted target should allow non-main owned array returns");
}

#[test]
fn hosted_target_rejects_returning_arrays_from_main() {
    let src = r#"
        fn main() -> [int; 4] {
            [0; 4]
        }
    "#;

    let program = parse_program(src);
    let err =
        check_program(&program).expect_err("hosted target should reject array returns from main");

    match err {
        TypeError::UnsupportedFeature { feature, .. } => {
            assert!(feature.contains("returning arrays from hosted main"));
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

#[test]
fn hosted_owned_array_returns_are_explicit_in_tast_and_tir() {
    let src = r#"
        fn make_arr() -> [int; 4] {
            return [0; 4];
        }

        fn main() -> int {
            let arr: [int; 4] = make_arr();
            arr[0]
        }
    "#;

    let program = parse_program(src);
    let tast = check_program(&program).expect("hosted target should typecheck owned array return");

    let make_arr = tast
        .functions
        .iter()
        .find(|func| func.name == "make_arr")
        .expect("make_arr should exist");
    assert!(make_arr.returns_owned);
    match &make_arr.body.statements[0].0 {
        crate::common::tast::TStmt::Return { ownership, .. } => {
            assert_eq!(*ownership, OwnershipMode::Owned);
        }
        other => panic!("expected explicit return stmt, got {other:?}"),
    }

    let main = tast
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main should exist");
    assert!(!main.returns_owned);
    match &main.body.statements[0].0 {
        crate::common::tast::TStmt::Let { value, .. } => match &value.0 {
            crate::common::tast::TExpr::Call { ownership, .. } => {
                assert_eq!(*ownership, OwnershipMode::Owned);
            }
            other => panic!("expected explicit owned call, got {other:?}"),
        },
        other => panic!("expected let binding for call result, got {other:?}"),
    }

    let tir = lower_program(&tast);
    let make_arr_tir = tir
        .functions
        .iter()
        .find(|func| func.name == "make_arr")
        .expect("lowered make_arr should exist");
    assert!(make_arr_tir.returns_owned);
    let entry = make_arr_tir
        .blocks
        .get(&make_arr_tir.entry_block)
        .expect("make_arr entry block should exist");
    match &entry.terminator {
        Terminator::Return { ownership, .. } => {
            assert_eq!(*ownership, OwnershipMode::Owned);
        }
        other => panic!("expected explicit owned TIR return, got {other:?}"),
    }

    let main_tir = tir
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("lowered main should exist");
    assert!(!main_tir.returns_owned);
    let entry = main_tir
        .blocks
        .get(&main_tir.entry_block)
        .expect("main entry block should exist");
    let call = entry
        .instructions
        .iter()
        .find_map(|instr| match instr {
            TirInstr::Call {
                ownership, func, ..
            } if func == "make_arr" => Some(*ownership),
            _ => None,
        })
        .expect("main should contain a call to make_arr");
    assert_eq!(call, OwnershipMode::Owned);
}

#[test]
fn hosted_target_rejects_use_after_move_via_let_binding() {
    let src = r#"
        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            let moved: [int; 4] = arr;
            arr[0]
        }
    "#;

    let program = parse_program(src);
    let err = check_program(&program).expect_err("hosted target should reject use after move");

    match err {
        TypeError::UseAfterMove { name, .. } => assert_eq!(name, "arr"),
        other => panic!("expected UseAfterMove, got {other:?}"),
    }
}

#[test]
fn hosted_target_rejects_use_after_move_via_call() {
    let src = r#"
        fn consume(arr: [int; 4]) -> int {
            arr[0]
        }

        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            consume(arr);
            arr[0]
        }
    "#;

    let program = parse_program(src);
    let err = check_program(&program).expect_err("hosted target should reject moved call arg use");

    match err {
        TypeError::UseAfterMove { name, .. } => assert_eq!(name, "arr"),
        other => panic!("expected UseAfterMove, got {other:?}"),
    }
}

#[test]
fn hosted_target_allows_reinitializing_moved_mutable_arrays() {
    let src = r#"
        fn main() -> int {
            let mut arr: [int; 4] = [0; 4];
            let moved: [int; 4] = arr;
            arr = [1; 4];
            arr[0]
        }
    "#;

    let program = parse_program(src);
    check_program(&program).expect("reinitializing a moved mutable binding should be allowed");
}

#[test]
fn hosted_owned_moves_lower_to_explicit_tir_moves() {
    let src = r#"
        fn consume(arr: [int; 4]) -> int {
            arr[0]
        }

        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            let moved: [int; 4] = arr;
            consume(moved)
        }
    "#;

    let program = parse_program(src);
    let tast = check_program(&program).expect("hosted target should typecheck explicit moves");
    let tir = lower_program(&tast);
    let main = tir
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("lowered main should exist");
    let entry = main
        .blocks
        .get(&main.entry_block)
        .expect("main entry block should exist");

    assert!(entry
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::MoveOwned { .. })));

    let call = entry
        .instructions
        .iter()
        .find_map(|instr| match instr {
            TirInstr::Call {
                func,
                arg_ownerships,
                ..
            } if func == "consume" => Some(arg_ownerships.clone()),
            _ => None,
        })
        .expect("main should contain a call to consume");
    assert_eq!(call, vec![OwnershipMode::Owned]);
}

#[test]
fn hosted_owned_shadowing_lowers_to_explicit_tir_drop() {
    let src = r#"
        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            let arr: [int; 4] = [1; 4];
            arr[0]
        }
    "#;

    let program = parse_program(src);
    let tast = check_program(&program).expect("hosted target should typecheck shadowing");
    let tir = lower_program(&tast);
    let main = tir
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("lowered main should exist");
    let entry = main
        .blocks
        .get(&main.entry_block)
        .expect("main entry block should exist");

    assert!(entry
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::DropOwned { .. })));
}

#[test]
fn hosted_owned_reassignment_lowers_to_explicit_tir_drop() {
    let src = r#"
        fn main() -> int {
            let mut arr: [int; 4] = [0; 4];
            arr = [1; 4];
            arr[0]
        }
    "#;

    let program = parse_program(src);
    let tast = check_program(&program).expect("hosted target should typecheck reassignment");
    let tir = lower_program(&tast);
    let main = tir
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("lowered main should exist");
    let entry = main
        .blocks
        .get(&main.entry_block)
        .expect("main entry block should exist");

    assert!(entry
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::DropOwned { .. })));
}

#[test]
fn hosted_owned_locals_drop_at_function_exit() {
    let src = r#"
        fn main() -> int {
            let arr: [int; 4] = [0; 4];
            0
        }
    "#;

    let program = parse_program(src);
    let tast = check_program(&program).expect("hosted target should typecheck function-exit drop");
    let tir = lower_program(&tast);
    let main = tir
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("lowered main should exist");
    let entry = main
        .blocks
        .get(&main.entry_block)
        .expect("main entry block should exist");

    assert!(entry
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::DropOwned { .. })));
}
