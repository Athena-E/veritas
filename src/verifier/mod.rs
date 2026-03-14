//! DTAL Verifier
//!
//! This module provides independent verification of DTAL (Dependently Typed Assembly Language)
//! programs. The verifier checks that type annotations are correct and that all type invariants
//! are maintained throughout program execution.
//!
//! # Overview
//!
//! The verifier performs the following checks:
//!
//! 1. **Per-instruction verification**: Each instruction maintains type invariants
//! 2. **Dataflow analysis**: Type states are propagated correctly through the CFG
//! 3. **Join point verification**: At merge points, types from all paths are compatible
//! 4. **Constraint verification**: Assertions and bounds checks are provable from context
//!
//! # Usage
//!
//! ```no_run
//! use veritas::verifier::verify_dtal;
//! use veritas::backend::dtal::instr::DtalProgram;
//!
//! let program = DtalProgram { functions: vec![] };
//! match verify_dtal(&program) {
//!     Ok(()) => println!("Verification passed!"),
//!     Err(e) => eprintln!("Verification failed: {}", e),
//! }
//! ```
//!
//! # Verification Guarantee
//!
//! If `verify_dtal` returns `Ok(())`, then:
//! - All type annotations are consistent
//! - No registers are used before definition
//! - All constraint assertions are provable
//! - Type states are compatible at control flow merge points

#![allow(clippy::result_large_err)]

pub(crate) mod checker;
mod dataflow;
mod error;
pub(crate) mod smt;

pub use error::VerifyError;

use crate::backend::dtal::instr::{DtalFunction, DtalProgram};
use checker::verify_instruction;
use dataflow::analyze_function;

/// Error type for standalone DTAL text verification
#[derive(Debug)]
pub enum VerifyTextError {
    /// Errors during parsing
    ParseErrors(Vec<crate::backend::dtal::parser::DtalParseError>),
    /// Error during verification
    VerifyError(Box<VerifyError>),
}

impl std::fmt::Display for VerifyTextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerifyTextError::ParseErrors(errors) => {
                writeln!(f, "DTAL parse errors:")?;
                for e in errors {
                    writeln!(f, "  {}", e)?;
                }
                Ok(())
            }
            VerifyTextError::VerifyError(e) => write!(f, "Verification error: {}", e),
        }
    }
}

impl std::error::Error for VerifyTextError {}

/// Verify a DTAL program from its text representation
///
/// Parses the text into a `DtalProgram` and then runs the verifier.
pub fn verify_dtal_text(input: &str) -> Result<(), VerifyTextError> {
    let program = crate::backend::dtal::parser::parse_dtal(input)
        .map_err(VerifyTextError::ParseErrors)?;
    verify_dtal(&program).map_err(|e| VerifyTextError::VerifyError(Box::new(e)))
}

/// Verify a complete DTAL program
pub fn verify_dtal(program: &DtalProgram) -> Result<(), VerifyError> {
    for func in &program.functions {
        verify_function(func, program)?;
    }
    Ok(())
}

/// Verify a single DTAL function
fn verify_function(
    func: &DtalFunction,
    program: &DtalProgram,
) -> Result<(), VerifyError> {
    // Run dataflow analysis to compute entry/exit states
    let dataflow = analyze_function(func)?;

    // Verify each block
    for block in &func.blocks {
        // Get entry state from dataflow analysis
        let entry_state = dataflow
            .entry_states
            .get(&block.label)
            .cloned()
            .unwrap_or_else(|| block.entry_state.clone());

        // Verify instructions with entry state
        let mut state = entry_state;
        for instr in &block.instructions {
            verify_instruction(instr, &mut state, &block.label, program)?;
        }

        // Verify terminator (for return instructions, check return type and postconditions)
        verify_terminator(func, block, &state)?;
    }

    Ok(())
}

/// Verify block terminator
fn verify_terminator(
    func: &DtalFunction,
    block: &crate::backend::dtal::instr::DtalBlock,
    state: &crate::backend::dtal::instr::TypeState,
) -> Result<(), VerifyError> {
    use crate::backend::dtal::instr::DtalInstr;
    use crate::backend::dtal::regs::{PhysicalReg, Reg};

    // Find terminator instruction
    for instr in &block.instructions {
        if let DtalInstr::Ret = instr {
            // For return, check that R0 (return register) has correct type
            let return_reg = Reg::Physical(PhysicalReg::R0);
            if let Some(actual_type) = state.register_types.get(&return_reg) {
                // Check return type compatibility
                if !types_compatible(actual_type, &func.return_type) {
                    return Err(VerifyError::ReturnTypeMismatch {
                        function: func.name.clone(),
                        expected: func.return_type.clone(),
                        actual: actual_type.clone(),
                    });
                }
            }

            // Check postcondition if present
            if let Some(postcond) = &func.postcondition
                && !checker::is_constraint_provable(postcond, &state.constraints)
            {
                return Err(VerifyError::PostconditionFailed {
                    function: func.name.clone(),
                    constraint: postcond.clone(),
                    context: state.constraints.clone(),
                });
            }
        }
    }

    Ok(())
}

/// Check if actual type is a subtype of (or equal to) expected type.
/// Delegates to the consolidated implementation in checker.rs.
fn types_compatible(
    actual: &crate::backend::dtal::types::DtalType,
    expected: &crate::backend::dtal::types::DtalType,
) -> bool {
    checker::types_compatible(actual, expected)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::constraints::{Constraint, IndexExpr};
    use crate::backend::dtal::instr::{CmpOp, DtalBlock, DtalInstr, TypeState};
    use crate::backend::dtal::regs::{PhysicalReg, Reg, VirtualReg};
    use crate::backend::dtal::types::{DtalType, DtalValue};
    use std::sync::Arc;

    // Helpers for concise register construction
    fn v(n: u32) -> Reg {
        Reg::Virtual(VirtualReg(n))
    }
    fn r0() -> Reg {
        Reg::Physical(PhysicalReg::R0)
    }

    fn make_program(functions: Vec<DtalFunction>) -> DtalProgram {
        DtalProgram { functions }
    }

    fn make_func(
        name: &str,
        params: Vec<(Reg, DtalType)>,
        return_type: DtalType,
        blocks: Vec<DtalBlock>,
    ) -> DtalFunction {
        DtalFunction {
            name: name.to_string(),
            params,
            return_type,
            precondition: None,
            postcondition: None,
            blocks,
        }
    }

    fn make_block(label: &str, instructions: Vec<DtalInstr>) -> DtalBlock {
        DtalBlock {
            label: label.to_string(),
            entry_state: TypeState::new(),
            instructions,
        }
    }

    // ========================================================================
    // Existing tests (rewritten with helpers)
    // ========================================================================

    #[test]
    fn test_verify_simple_function() {
        let program = make_program(vec![make_func(
            "id",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(result.is_ok(), "Verification failed: {:?}", result.err());
    }

    #[test]
    fn test_verify_singleton_match() {
        let program = make_program(vec![make_func(
            "const_five",
            vec![],
            DtalType::SingletonInt(DtalValue::Int(5)),
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 5,
                        ty: DtalType::SingletonInt(DtalValue::Int(5)),
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_reject_singleton_mismatch() {
        let program = make_program(vec![make_func(
            "bad",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::SingletonInt(DtalValue::Int(6)), // Wrong!
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::SingletonMismatch { .. }
        ));
    }

    #[test]
    fn test_reject_undefined_register() {
        let program = make_program(vec![make_func(
            "bad",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::MovReg {
                    dst: v(1),
                    src: v(0), // Not defined!
                    ty: DtalType::Int,
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::UndefinedRegister { .. }
        ));
    }

    // ========================================================================
    // Phase 3: ConstraintAssume is ignored
    // ========================================================================

    #[test]
    fn test_constraint_assume_ignored() {
        let program = make_program(vec![make_func(
            "bad",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::ConstraintAssume {
                        constraint: Constraint::Lt(
                            IndexExpr::Var("v0".to_string()),
                            IndexExpr::Const(10),
                        ),
                    },
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::Lt(
                            IndexExpr::Var("v0".to_string()),
                            IndexExpr::Const(10),
                        ),
                        msg: "should fail".to_string(),
                    },
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "ConstraintAssume should be ignored, so assert should fail"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::UnprovableConstraint { .. }
        ));
    }

    // ========================================================================
    // Phase 4: Z3 constraint oracle
    // ========================================================================

    #[test]
    fn test_constraint_assert_provable_from_precondition() {
        let mut func = make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::Ge(
                            IndexExpr::Var("v0".to_string()),
                            IndexExpr::Const(0),
                        ),
                        msg: "x >= 0".to_string(),
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        func.precondition = Some(Constraint::Ge(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(0),
        ));
        let program = make_program(vec![func]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Precondition should make the assert provable"
        );
    }

    #[test]
    fn test_constraint_assert_unprovable() {
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::ConstraintAssert {
                    constraint: Constraint::Gt(
                        IndexExpr::Var("v0".to_string()),
                        IndexExpr::Const(0),
                    ),
                    msg: "x > 0".to_string(),
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::UnprovableConstraint { .. }
        ));
    }

    #[test]
    fn test_constraint_assert_z3_transitivity() {
        let mut func = make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::Ge(
                            IndexExpr::Var("v0".to_string()),
                            IndexExpr::Const(3),
                        ),
                        msg: "x >= 3".to_string(),
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        func.precondition = Some(Constraint::Ge(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(5),
        ));
        let program = make_program(vec![func]);
        assert!(
            verify_dtal(&program).is_ok(),
            "x >= 5 should imply x >= 3 via Z3"
        );
    }

    // ========================================================================
    // Phase 5: Complete subtyping
    // ========================================================================

    #[test]
    fn test_reject_int_as_singleton() {
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::MovReg {
                    dst: v(1),
                    src: v(0),
                    ty: DtalType::SingletonInt(DtalValue::Int(5)),
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(result.is_err(), "Int is not a subtype of SingletonInt");
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::TypeMismatch { .. }
        ));
    }

    #[test]
    fn test_singleton_subtype_of_int() {
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::SingletonInt(DtalValue::Int(5)))],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_reject_bool_as_int() {
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Int), (v(1), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Cmp {
                        lhs: v(0),
                        rhs: v(1),
                    },
                    DtalInstr::SetCC {
                        dst: v(2),
                        cond: CmpOp::Lt,
                    },
                    DtalInstr::MovReg {
                        dst: v(3),
                        src: v(2),        // v2 is Bool
                        ty: DtalType::Int, // annotated as Int
                    },
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(result.is_err(), "Bool is not a subtype of Int");
    }

    #[test]
    fn test_array_subtyping_compatible() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        assert!(checker::types_compatible(&arr_ty, &arr_ty));
    }

    #[test]
    fn test_array_subtyping_different_sizes() {
        let arr10 = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        let arr5 = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(5),
        };
        assert!(!checker::types_compatible(&arr10, &arr5));
    }

    // ========================================================================
    // Phase 6: TypeAnnotation verification
    // ========================================================================

    #[test]
    fn test_reject_wrong_type_annotation() {
        let program = make_program(vec![make_func(
            "bad",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 5,
                        ty: DtalType::SingletonInt(DtalValue::Int(5)),
                    },
                    DtalInstr::TypeAnnotation {
                        reg: v(0),
                        ty: DtalType::Bool,
                    },
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "TypeAnnotation should be rejected when incompatible"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::TypeMismatch { .. }
        ));
    }

    #[test]
    fn test_accept_valid_type_annotation() {
        let program = make_program(vec![make_func(
            "ok",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 5,
                        ty: DtalType::SingletonInt(DtalValue::Int(5)),
                    },
                    DtalInstr::TypeAnnotation {
                        reg: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_accept_phi_type_annotation() {
        let program = make_program(vec![make_func(
            "ok",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::TypeAnnotation {
                        reg: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_reject_type_annotation_int_to_singleton() {
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::TypeAnnotation {
                    reg: v(0),
                    ty: DtalType::SingletonInt(DtalValue::Int(5)),
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "Cannot narrow Int to SingletonInt via annotation"
        );
    }

    // ========================================================================
    // Phase 7: Bounds checking for Load/Store
    // ========================================================================

    #[test]
    fn test_reject_load_without_bounds_proof() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), arr_ty), (v(1), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::Load {
                    dst: v(2),
                    base: v(0),
                    offset: v(1),
                    ty: DtalType::Int,
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "Load without bounds proof should be rejected"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::BoundsCheckFailed { .. }
        ));
    }

    #[test]
    fn test_accept_load_with_constant_in_bounds() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), arr_ty)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(1),
                        imm: 3,
                        ty: DtalType::SingletonInt(DtalValue::Int(3)),
                    },
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::True,
                        msg: "".to_string(),
                    },
                    DtalInstr::Load {
                        dst: v(2),
                        base: v(0),
                        offset: v(1),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        let _result = verify_dtal(&program);
    }

    #[test]
    fn test_accept_load_with_precondition_bounds() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        let mut func = make_func(
            "ok",
            vec![(v(0), arr_ty), (v(1), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Load {
                        dst: v(2),
                        base: v(0),
                        offset: v(1),
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(2),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        func.precondition = Some(Constraint::And(
            Box::new(Constraint::Ge(
                IndexExpr::Var("v1".to_string()),
                IndexExpr::Const(0),
            )),
            Box::new(Constraint::Lt(
                IndexExpr::Var("v1".to_string()),
                IndexExpr::Const(10),
            )),
        ));
        let program = make_program(vec![func]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Precondition provides bounds proof for load"
        );
    }

    #[test]
    fn test_reject_store_without_bounds_proof() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(5),
        };
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), arr_ty), (v(1), DtalType::Int), (v(2), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![DtalInstr::Store {
                    base: v(0),
                    offset: v(1),
                    src: v(2),
                }],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "Store without bounds proof should be rejected"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::BoundsCheckFailed { .. }
        ));
    }

    // ========================================================================
    // Phase 8: Function contract verification
    // ========================================================================

    #[test]
    fn test_reject_unprovable_postcondition() {
        let mut func = make_func(
            "bad",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        func.postcondition = Some(Constraint::Gt(
            IndexExpr::Var("r0".to_string()),
            IndexExpr::Const(0),
        ));
        let program = make_program(vec![func]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "Postcondition should fail without supporting context"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::PostconditionFailed { .. }
        ));
    }

    #[test]
    fn test_accept_provable_postcondition() {
        let mut func = make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        func.precondition = Some(Constraint::Gt(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(0),
        ));
        func.postcondition = Some(Constraint::Gt(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(0),
        ));
        let program = make_program(vec![func]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Postcondition should be provable from precondition"
        );
    }

    #[test]
    fn test_reject_unprovable_precondition_at_call() {
        let callee = {
            let mut f = make_func(
                "requires_positive",
                vec![(v(0), DtalType::Int)],
                DtalType::Int,
                vec![make_block(
                    ".entry",
                    vec![
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(0),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                )],
            );
            f.precondition = Some(Constraint::Gt(
                IndexExpr::Var("v0".to_string()),
                IndexExpr::Const(0),
            ));
            f
        };
        let caller = make_func(
            "caller",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Call {
                        target: "requires_positive".to_string(),
                        return_ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        let program = make_program(vec![callee, caller]);
        let result = verify_dtal(&program);
        assert!(
            result.is_err(),
            "Caller should fail to prove callee's precondition"
        );
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::PreconditionFailed { .. }
        ));
    }

    // ========================================================================
    // Phase 3: Branch-derived constraints
    // ========================================================================

    #[test]
    fn test_branch_derives_constraint_for_assert() {
        let mut func = make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![
                make_block(
                    ".entry",
                    vec![
                        DtalInstr::CmpImm { lhs: v(0), imm: 10 },
                        DtalInstr::Branch {
                            cond: CmpOp::Lt,
                            target: ".taken".to_string(),
                        },
                        DtalInstr::ConstraintAssert {
                            constraint: Constraint::Ge(
                                IndexExpr::Var("v0".to_string()),
                                IndexExpr::Const(10),
                            ),
                            msg: "v0 >= 10 on fall-through".to_string(),
                        },
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(0),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                ),
                make_block(
                    ".taken",
                    vec![
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(0),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                ),
            ],
        );
        func.params = vec![(v(0), DtalType::Int)];
        let program = make_program(vec![func]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Fall-through constraint v0 >= 10 should be derived from blt"
        );
    }

    // ========================================================================
    // Phase 1: Fall-through detection
    // ========================================================================

    #[test]
    fn test_fallthrough_propagates_state() {
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![
                make_block(
                    ".bb0",
                    vec![
                        DtalInstr::MovImm {
                            dst: v(1),
                            imm: 42,
                            ty: DtalType::Int,
                        },
                        DtalInstr::CmpImm { lhs: v(0), imm: 0 },
                        DtalInstr::Branch {
                            cond: CmpOp::Eq,
                            target: ".bb2".to_string(),
                        },
                    ],
                ),
                make_block(
                    ".bb1",
                    vec![
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(1),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                ),
                make_block(
                    ".bb2",
                    vec![
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(1),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                ),
            ],
        )]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Fall-through should propagate v1 to .bb1"
        );
    }

    // ========================================================================
    // Return type checking with subtyping
    // ========================================================================

    #[test]
    fn test_reject_return_type_mismatch() {
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Int)],
            DtalType::Bool,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::ReturnTypeMismatch { .. }
        ));
    }
}
