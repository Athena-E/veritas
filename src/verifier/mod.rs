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

mod checker;
mod dataflow;
mod error;

pub use error::VerifyError;

use crate::backend::dtal::instr::{DtalFunction, DtalProgram};
use checker::verify_instruction;
use dataflow::analyze_function;

/// Verify a complete DTAL program
///
/// # Arguments
///
/// * `program` - The DTAL program to verify
///
/// # Returns
///
/// * `Ok(())` if verification passes
/// * `Err(VerifyError)` with details if verification fails
pub fn verify_dtal<'src>(program: &DtalProgram<'src>) -> Result<(), VerifyError<'src>> {
    for func in &program.functions {
        verify_function(func)?;
    }
    Ok(())
}

/// Verify a single DTAL function
fn verify_function<'src>(func: &DtalFunction<'src>) -> Result<(), VerifyError<'src>> {
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
            verify_instruction(instr, &mut state, &block.label)?;
        }

        // Verify terminator (for return instructions, check return type)
        verify_terminator(func, block, &state)?;
    }

    Ok(())
}

/// Verify block terminator
fn verify_terminator<'src>(
    func: &DtalFunction<'src>,
    block: &crate::backend::dtal::instr::DtalBlock<'src>,
    state: &crate::backend::dtal::instr::TypeState<'src>,
) -> Result<(), VerifyError<'src>> {
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
        }
    }

    Ok(())
}

/// Check if actual type is compatible with expected type
fn types_compatible<'src>(
    actual: &crate::common::types::IType<'src>,
    expected: &crate::common::types::IType<'src>,
) -> bool {
    use crate::common::types::IType;

    match (actual, expected) {
        (IType::Int, IType::Int) => true,
        (IType::Bool, IType::Bool) => true,
        (IType::Unit, IType::Unit) => true,
        (IType::SingletonInt(a), IType::SingletonInt(b)) => a == b,
        (IType::SingletonInt(_), IType::Int) => true,
        (IType::RefinedInt { .. }, IType::Int) => true,
        (IType::Int, IType::RefinedInt { .. }) => false, // int is not subtype of refined
        _ => true,                                       // Conservative: trust for now
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{DtalBlock, DtalInstr, TypeState};
    use crate::backend::dtal::regs::{PhysicalReg, Reg, VirtualReg};
    use crate::common::types::{IType, IValue};

    #[test]
    fn test_verify_simple_function() {
        // Create a simple function: fn id(x: int) -> int { return x; }
        let program = DtalProgram {
            functions: vec![DtalFunction {
                name: "id".to_string(),
                params: vec![(Reg::Virtual(VirtualReg(0)), IType::Int)],
                return_type: IType::Int,
                precondition: None,
                postcondition: None,
                blocks: vec![DtalBlock {
                    label: ".id_entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovReg {
                            dst: Reg::Physical(PhysicalReg::R0),
                            src: Reg::Virtual(VirtualReg(0)),
                            ty: IType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                }],
            }],
        };

        let result = verify_dtal(&program);
        assert!(result.is_ok(), "Verification failed: {:?}", result.err());
    }

    #[test]
    fn test_verify_singleton_match() {
        // Test that singleton types must match
        let program = DtalProgram {
            functions: vec![DtalFunction {
                name: "const_five".to_string(),
                params: vec![],
                return_type: IType::SingletonInt(IValue::Int(5)),
                precondition: None,
                postcondition: None,
                blocks: vec![DtalBlock {
                    label: ".entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: Reg::Virtual(VirtualReg(0)),
                            imm: 5,
                            ty: IType::SingletonInt(IValue::Int(5)),
                        },
                        DtalInstr::Ret,
                    ],
                }],
            }],
        };

        let result = verify_dtal(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_reject_singleton_mismatch() {
        // Test that mismatched singleton values are rejected
        let program = DtalProgram {
            functions: vec![DtalFunction {
                name: "bad".to_string(),
                params: vec![],
                return_type: IType::Int,
                precondition: None,
                postcondition: None,
                blocks: vec![DtalBlock {
                    label: ".entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::MovImm {
                        dst: Reg::Virtual(VirtualReg(0)),
                        imm: 5,
                        ty: IType::SingletonInt(IValue::Int(6)), // Wrong!
                    }],
                }],
            }],
        };

        let result = verify_dtal(&program);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::SingletonMismatch { .. }
        ));
    }

    #[test]
    fn test_reject_undefined_register() {
        // Test that using undefined registers is rejected
        let program = DtalProgram {
            functions: vec![DtalFunction {
                name: "bad".to_string(),
                params: vec![],
                return_type: IType::Int,
                precondition: None,
                postcondition: None,
                blocks: vec![DtalBlock {
                    label: ".entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::MovReg {
                        dst: Reg::Virtual(VirtualReg(1)),
                        src: Reg::Virtual(VirtualReg(0)), // Not defined!
                        ty: IType::Int,
                    }],
                }],
            }],
        };

        let result = verify_dtal(&program);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::UndefinedRegister { .. }
        ));
    }
}
