//! End-to-end verification tests
//!
//! Compiles each .veri example file through the full pipeline
//! (lex → parse → typecheck → lower → codegen → emit → verify),
//! ensuring the DTAL verifier accepts all valid programs.

use veritas::pipeline::compile_verbose;
use veritas::verifier::verify_dtal;

fn compile_and_verify(source: &str) -> Result<(), String> {
    let output = compile_verbose(source).map_err(|e| format!("Compilation failed: {}", e))?;
    verify_dtal(&output.dtal_program).map_err(|e| format!("Verification failed: {}", e))
}

fn compile_and_verify_roundtrip(source: &str) -> Result<(), String> {
    let output = compile_verbose(source).map_err(|e| format!("Compilation failed: {}", e))?;
    verify_dtal(&output.dtal_program)
        .map_err(|e| format!("Verification failed (in-memory): {}", e))?;
    veritas::verifier::verify_dtal_text(&output.dtal)
        .map_err(|e| format!("Verification failed (text round-trip): {}", e))
}

macro_rules! verify_example {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let source = include_str!(concat!("../src/examples/", $file));
            compile_and_verify(source).unwrap_or_else(|e| panic!("{}: {}", $file, e));
        }
    };
}

macro_rules! verify_roundtrip {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let source = include_str!(concat!("../src/examples/", $file));
            compile_and_verify_roundtrip(source).unwrap_or_else(|e| panic!("{}: {}", $file, e));
        }
    };
}

macro_rules! expect_compile_error {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let source = include_str!(concat!("../src/examples/errors/", $file));
            let result = compile_verbose(source);
            assert!(result.is_err(), "{}: expected compilation error but succeeded", $file);
        }
    };
}

// ============================================================================
// Success cases: compile + verify (29 tests)
// ============================================================================

verify_example!(e2e_01_simple, "01_simple.veri");
verify_example!(e2e_02_conditionals, "02_conditionals.veri");
verify_example!(e2e_03_arrays, "03_arrays.veri");
verify_example!(e2e_04_references, "04_references.veri");
verify_example!(e2e_05_comparisons, "05_comparisons.veri");
verify_example!(e2e_06_logical, "06_logical.veri");
verify_example!(e2e_07_function_calls, "07_function_calls.veri");
verify_example!(e2e_08_mutable, "08_mutable.veri");
verify_example!(e2e_09_complex_expressions, "09_complex_expressions.veri");
verify_example!(e2e_10_advanced_types, "10_advanced_types.veri");
verify_example!(e2e_11_unary_not, "11_unary_not.veri");
verify_example!(e2e_12_function_parameters, "12_function_parameters.veri");
verify_example!(e2e_13_function_return, "13_function_return.veri");
verify_example!(e2e_14_for_loops, "14_for_loops.veri");
verify_example!(e2e_15_array_init, "15_array_init.veri");
verify_example!(e2e_16_array_assignment, "16_array_assignment.veri");
verify_example!(e2e_17_preconditions, "17_preconditions.veri");
verify_example!(e2e_18_precondition_use, "18_precondition_use.veri");
verify_example!(e2e_19_loop_invariant, "19_loop_invariant.veri");
verify_example!(e2e_19_quantifier_showcase, "19_quantifier_showcase.veri");
verify_example!(e2e_21_safe_division, "21_safe_division.veri");
verify_example!(e2e_22_bubble_sort, "22_bubble_sort.veri");
verify_example!(e2e_add, "add.veri");
verify_example!(e2e_linear_search, "linear_search.veri");
verify_example!(e2e_smt_minimal, "smt_minimal_annotations.veri");
verify_example!(e2e_smt_synthesis, "smt_synthesis_tests.veri");

// Text round-trip
verify_roundtrip!(e2e_roundtrip_01_simple, "01_simple.veri");
verify_roundtrip!(e2e_roundtrip_07_function_calls, "07_function_calls.veri");
verify_roundtrip!(e2e_roundtrip_add, "add.veri");

// ============================================================================
// Feature-dependent: need array store/select axioms in constraint domain
// ============================================================================

#[test]
#[ignore = "feature: quantified loop invariant over array contents needs Select reasoning"]
fn e2e_20_array_loop_invariant() {
    let source = include_str!("../src/examples/20_array_loop_invariant.veri");
    compile_and_verify(source).unwrap();
}

#[test]
#[ignore = "feature: precondition depends on array element value after store"]
fn e2e_selective_invalidation() {
    let source = include_str!("../src/examples/selective_invalidation.veri");
    compile_and_verify(source).unwrap();
}

#[test]
#[ignore = "feature: postcondition quantifier over sorted array contents"]
fn e2e_sortedness() {
    let source = include_str!("../src/examples/sortedness.veri");
    compile_and_verify(source).unwrap();
}

// ============================================================================
// Error cases: should fail during compilation (30 tests)
// ============================================================================

expect_compile_error!(e2e_err_01_type_mismatch, "01_type_mismatch.veri");
expect_compile_error!(e2e_err_02_undefined_variable, "02_undefined_variable.veri");
expect_compile_error!(e2e_err_03_undefined_function, "03_undefined_function.veri");
expect_compile_error!(e2e_err_04_assign_immutable, "04_assign_immutable.veri");
expect_compile_error!(e2e_err_05_return_type_mismatch, "05_return_type_mismatch.veri");
expect_compile_error!(e2e_err_06_missing_return, "06_missing_return.veri");
expect_compile_error!(e2e_err_07_not_an_array, "07_not_an_array.veri");
expect_compile_error!(e2e_err_08_wrong_argument_count, "08_wrong_argument_count.veri");
expect_compile_error!(e2e_err_09_argument_type_mismatch, "09_argument_type_mismatch.veri");
expect_compile_error!(e2e_err_10_invalid_operation, "10_invalid_operation.veri");
expect_compile_error!(e2e_err_11_bool_arithmetic, "11_bool_arithmetic.veri");
expect_compile_error!(e2e_err_12_non_bool_condition, "12_non_bool_condition.veri");
expect_compile_error!(e2e_err_13_array_index_type, "13_array_index_type.veri");
expect_compile_error!(e2e_err_14_array_element_mismatch, "14_array_element_mismatch.veri");
expect_compile_error!(e2e_err_15_logical_on_int, "15_logical_on_int.veri");
expect_compile_error!(e2e_err_16_not_on_int, "16_not_on_int.veri");
expect_compile_error!(e2e_err_17_for_loop_non_int, "17_for_loop_non_int.veri");
expect_compile_error!(e2e_err_18_multiple_errors, "18_multiple_errors.veri");
expect_compile_error!(e2e_err_19_array_out_of_bounds, "19_array_out_of_bounds.veri");
expect_compile_error!(e2e_err_20_array_negative_index, "20_array_negative_index.veri");
expect_compile_error!(e2e_err_21_precondition_violation, "21_precondition_violation.veri");
expect_compile_error!(e2e_err_22_quantifier_non_bool_body, "22_quantifier_non_bool_body.veri");
expect_compile_error!(e2e_err_23_quantifier_non_int_range, "23_quantifier_non_int_range.veri");
expect_compile_error!(e2e_err_24_implies_non_bool, "24_implies_non_bool.veri");
expect_compile_error!(e2e_err_25_precondition_quantifier_violation, "25_precondition_quantifier_violation.veri");
expect_compile_error!(e2e_err_26_quantifier_in_runtime, "26_quantifier_in_runtime.veri");
expect_compile_error!(e2e_err_27_invariant_not_preserved, "27_invariant_not_preserved.veri");
expect_compile_error!(e2e_err_28_invariant_not_established, "28_invariant_not_established.veri");
expect_compile_error!(e2e_err_division_by_zero, "division_by_zero.veri");
expect_compile_error!(e2e_err_division_by_zero_symbolic, "division_by_zero_symbolic.veri");
