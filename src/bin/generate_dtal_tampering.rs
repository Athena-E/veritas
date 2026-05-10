use std::fs;
use std::path::PathBuf;

use veritas::pipeline::compile_verbose;

struct TamperCase {
    id: &'static str,
    name: &'static str,
    source: &'static str,
    mutate: fn(&str) -> String,
    expected_error: &'static str,
}

fn replace_once(haystack: &str, needle: &str, replacement: &str) -> String {
    haystack.replacen(needle, replacement, 1)
}

fn compile_to_dtal(source: &str) -> String {
    compile_verbose(source)
        .unwrap_or_else(|err| panic!("expected source to compile successfully: {}", err))
        .dtal
}

fn cases() -> [TamperCase; 14] {
    [
        TamperCase {
            id: "T01",
            name: "return_signature_mismatch",
            source: include_str!("../../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| replace_once(dtal, ".returns int", ".returns bool"),
            expected_error: "Return type mismatch",
        },
        TamperCase {
            id: "T02",
            name: "strengthened_precondition_breaks_call_site",
            source: include_str!("../../eval/feature_suite/programs/17_preconditions.veri"),
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
            id: "T03",
            name: "shared_borrow_out_of_bounds_index",
            source: include_str!("../../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T04",
            name: "move_owned_while_shared_borrow_live",
            source: include_str!("../../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
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
            id: "T05",
            name: "i64_add_overflow_from_tampered_constants",
            source: include_str!("../../eval/feature_suite/programs/01_simple.veri"),
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
            id: "T06",
            name: "shared_borrow_negative_index",
            source: include_str!("../../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, -1    : int(-1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T07",
            name: "use_after_drop_owned",
            source: include_str!("../../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
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
            id: "T08",
            name: "plain_mov_duplicates_owned_value",
            source: include_str!("../../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
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
            id: "T09",
            name: "alias_shared_while_mutable_borrow_live",
            source: include_str!("../../eval/feature_suite/programs/33_mutable_borrow.veri"),
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
            id: "T10",
            name: "double_mutable_borrow",
            source: include_str!(
                "../../eval/feature_suite/programs/37_mutable_scalar_borrow_call.veri"
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
            id: "T11",
            name: "mutable_store_out_of_bounds_index",
            source: include_str!("../../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| replace_once(dtal, "mov v1, 0    : int(0)", "mov v1, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T12",
            name: "entry_param_type_weakened_from_mutable_to_shared",
            source: include_str!("../../eval/feature_suite/programs/33_mutable_borrow.veri"),
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
            id: "T13",
            name: "tampered_entry_state_type",
            source: include_str!("../../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| replace_once(dtal, ".entry {v0: int}", ".entry {v0: bool}"),
            expected_error: "Type mismatch",
        },
        TamperCase {
            id: "T14",
            name: "impossible_assertion_inserted",
            source: include_str!("../../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    .assume (v0 >= 0 && v0 < 4)",
                    "    .assume (v0 >= 0 && v0 < 4)\n    .assert (v0 < 0)",
                )
            },
            expected_error: "Cannot prove constraint",
        },
    ]
}

fn main() {
    let out_dir = PathBuf::from("eval/dtal_tampering/generated");
    fs::create_dir_all(&out_dir)
        .unwrap_or_else(|err| panic!("failed to create {}: {}", out_dir.display(), err));

    for case in cases() {
        let dtal = compile_to_dtal(case.source);
        let tampered = (case.mutate)(&dtal);
        let file_name = format!("{}_{}.dtal", case.id, case.name);
        let path = out_dir.join(file_name);
        fs::write(&path, tampered)
            .unwrap_or_else(|err| panic!("failed to write {}: {}", path.display(), err));
        println!("{}\t{}\t{}", case.id, path.display(), case.expected_error);
    }
}
