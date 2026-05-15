//! Trust-architecture tests for standalone DTAL verification.
//!
//! These tests compile known-good curated feature-suite programs to DTAL,
//! apply small text-level mutations that preserve DTAL syntax, and then check
//! that standalone verification rejects the tampered program.

use veritas::pipeline::compile_verbose;
use veritas::verifier::{VerifyTextError, verify_dtal_text};

struct TamperCase {
    name: &'static str,
    source: &'static str,
    mutate: fn(&str) -> String,
    expected_error: &'static str,
}

fn replace_once(haystack: &str, needle: &str, replacement: &str) -> String {
    assert!(
        haystack.contains(needle),
        "tamper needle not found: {:?}",
        needle
    );
    haystack.replacen(needle, replacement, 1)
}

fn compile_to_dtal(source: &str) -> String {
    compile_verbose(source)
        .unwrap_or_else(|err| panic!("expected source to compile successfully: {}", err))
        .dtal
}

fn assert_tamper_rejected(case: &TamperCase) {
    let dtal = compile_to_dtal(case.source);
    verify_dtal_text(&dtal)
        .unwrap_or_else(|err| panic!("{}: baseline DTAL should verify: {}", case.name, err));

    let tampered = (case.mutate)(&dtal);
    let err = match verify_dtal_text(&tampered) {
        Ok(()) => panic!("{}: expected tampered DTAL to be rejected", case.name),
        Err(err) => err,
    };

    match err {
        VerifyTextError::ParseErrors(errors) => {
            panic!(
                "{}: expected verifier rejection, got parse failure: {:?}",
                case.name, errors
            );
        }
        VerifyTextError::VerifyError(err) => {
            let rendered = err.to_string();
            assert!(
                rendered.contains(case.expected_error),
                "{}: expected error containing {:?}, got {:?}",
                case.name,
                case.expected_error,
                rendered
            );
        }
    }
}

fn assert_tamper_accepted(name: &str, source: &str, mutate: fn(&str) -> String) {
    let dtal = compile_to_dtal(source);
    verify_dtal_text(&dtal)
        .unwrap_or_else(|err| panic!("{}: baseline DTAL should verify: {}", name, err));

    let tampered = mutate(&dtal);
    verify_dtal_text(&tampered)
        .unwrap_or_else(|err| panic!("{}: safety-preserving tamper should verify: {}", name, err));
}
#[test]
#[ignore = "slow trust-architecture tampering suite; run explicitly"]

fn tampered_dtal_corpus_is_rejected() {
    let cases = [
        TamperCase {
            name: "return_signature_mismatch",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| replace_once(dtal, ".returns int", ".returns bool"),
            expected_error: "Return type mismatch",
        },
        TamperCase {
            name: "strengthened_precondition_breaks_call_site",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    ".precondition (v0 >= 0 && v0 < 4)",
                    ".precondition (v0 >= 0 && v0 < 2)",
                );
                replace_once(
                    &dtal,
                    ".assume (v0 >= 0 && v0 < 4)",
                    ".assume (v0 >= 0 && v0 < 2)",
                )
            },
            expected_error: "Precondition not provable",
        },
        TamperCase {
            name: "shared_borrow_out_of_bounds_index",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            name: "move_owned_while_shared_borrow_live",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]",
                    "    move_owned v6, v1    : [int(7); 1]\n    borrow_end v3    : &[int(7); 1]",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            name: "i64_add_overflow_from_tampered_constants",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    "mov v0, 42    : int(42)",
                    "mov v0, 9223372036854775807    : int(9223372036854775807)",
                );
                let dtal = replace_once(
                    dtal.as_str(),
                    "mov v1, 10    : int(10)",
                    "mov v1, 1    : int(1)",
                );
                replace_once(
                    dtal.as_str(),
                    "add v2, v0, v1    : int(52)",
                    "add v2, v0, v1    : i64",
                )
            },
            expected_error: "Arithmetic overflow",
        },
        TamperCase {
            name: "shared_borrow_negative_index",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, -1    : int(-1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            name: "use_after_drop_owned",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]\n    push v5",
                    "    borrow_end v3    : &[int(7); 1]\n    drop_owned v1    : [int(7); 1]\n    move_owned v6, v1    : [int(7); 1]\n    push v5",
                )
            },
            expected_error: "used after ownership was consumed",
        },
        TamperCase {
            name: "plain_mov_duplicates_owned_value",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]\n    push v5",
                    "    borrow_end v3    : &[int(7); 1]\n    mov v6, v1    : [int(7); 1]\n    push v5",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            name: "alias_shared_while_mutable_borrow_live",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "borrow_mut r0, v0    : int",
                    "borrow_mut r0, v0    : int\n    alias_borrow v9, v0    : &[int; 1]",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            name: "double_mutable_borrow",
            source: include_str!(
                "../eval/feature_suite/programs/37_mutable_scalar_borrow_call.veri"
            ),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "borrow_mut r0, v1    : int",
                    "borrow_mut r0, v1    : int\n    borrow_mut r1, v1    : int",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            name: "mutable_store_out_of_bounds_index",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| replace_once(dtal, "mov v1, 0    : int(0)", "mov v1, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            name: "entry_param_type_weakened_from_mutable_to_shared",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    ".params {v0: &mut [int; 1]}",
                    ".params {v0: &[int; 1]}",
                )
            },
            expected_error: "Type mismatch",
        },
        TamperCase {
            name: "tampered_entry_state_type",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| replace_once(dtal, ".entry {v0: int}", ".entry {v0: bool}"),
            expected_error: "Type mismatch",
        },
        TamperCase {
            name: "impossible_assertion_inserted",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    .assume (v0 >= 0 && v0 < 4)",
                    "    .assume (v0 >= 0 && v0 < 4)\n    .assert (v0 < 0)",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            name: "branch_target_swap_breaks_edge_assumption",
            source: include_str!("../eval/feature_suite/programs/02_conditionals.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    bgt .max_of_bb1\n    jmp .max_of_bb2",
                    "    bgt .max_of_bb2\n    jmp .max_of_bb1",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            name: "false_singleton_annotation_after_mov",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| replace_once(dtal, "mov v0, 42    : int(42)", "mov v0, 42    : int(41)"),
            expected_error: "Singleton type mismatch",
        },
        TamperCase {
            name: "division_nonzero_evidence_removed",
            source: include_str!("../eval/feature_suite/programs/21_safe_division.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    ".params {v0: int, v1: {v: int | v != 0 }}",
                    ".params {v0: int, v1: int}",
                );
                replace_once(
                    &dtal,
                    ".entry {v0: int, v1: {v: int | v != 0 }}",
                    ".entry {v0: int, v1: int}",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            name: "postcondition_corrupted_to_unprovable_fact",
            source: include_str!("../eval/feature_suite/programs/38_sortedness.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    ".postcondition (forall i in 0..2 { v7[i] <= v7[(i + 1)] })",
                    ".postcondition v7[0] > v7[1]",
                )
            },
            expected_error: "Postcondition not provable",
        },
    ];

    for case in &cases {
        assert_tamper_rejected(case);
    }
}
#[test]
#[ignore = "documents the safety-only boundary of DTAL verification"]

fn type_preserving_semantic_tamper_can_still_verify() {
    assert_tamper_accepted(
        "wrong_search_constant_preserves_safety",
        include_str!("../eval/feature_suite/programs/20_binary_search.veri"),
        |dtal| replace_once(dtal, "mov v22, 34    : int(34)", "mov v22, 35    : int(35)"),
    );

    assert_tamper_accepted(
        "call_return_annotation_ignored_in_favour_of_signature",
        include_str!("../eval/feature_suite/programs/07_function_calls.veri"),
        |dtal| {
            replace_once(
                dtal,
                "call add [value,value]    : int",
                "call add [value,value]    : bool",
            )
        },
    );
}
