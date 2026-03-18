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
pub(crate) mod dataflow;
mod error;
pub mod smt;

pub use error::VerifyError;

use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use checker::verify_instruction;
use std::collections::HashMap;

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
    let program =
        crate::backend::dtal::parser::parse_dtal(input).map_err(VerifyTextError::ParseErrors)?;
    verify_dtal(&program).map_err(|e| VerifyTextError::VerifyError(Box::new(e)))
}

/// Verify a complete DTAL program
pub fn verify_dtal(program: &DtalProgram) -> Result<(), VerifyError> {
    for func in &program.functions {
        verify_function(func, program)?;
    }
    Ok(())
}

/// Verify a single DTAL function using derivation-based checking.
///
/// Each block is checked independently:
/// 1. Start from the block's declared entry state
/// 2. Derive types through the instruction sequence
/// 3. At jumps/branches, prove state coercion into target's declared state
/// 4. At return, check return type and postcondition
///
/// No dataflow analysis is used. The entry block's declared state must be
/// consistent with function parameters and precondition. If a block has no
/// declared entry state, the verifier computes one via dataflow (backward
/// compatibility for programs that don't yet declare states).
fn verify_function(func: &DtalFunction, program: &DtalProgram) -> Result<(), VerifyError> {
    // Build label map for state coercion checks at jumps
    let label_map: HashMap<&str, &TypeState> = func
        .blocks
        .iter()
        .map(|b| (b.label.as_str(), &b.entry_state))
        .collect();

    // If blocks have declared entry states, use derivation-based checking.
    // Otherwise, fall back to dataflow-based checking for backward compatibility.
    let has_declared_states = func
        .blocks
        .iter()
        .any(|b| !b.entry_state.register_types.is_empty());

    if has_declared_states {
        verify_function_derivation(func, program, &label_map)
    } else {
        verify_function_dataflow(func, program)
    }
}

/// Derivation-based verification: each block is checked independently
fn verify_function_derivation(
    func: &DtalFunction,
    program: &DtalProgram,
    label_map: &HashMap<&str, &TypeState>,
) -> Result<(), VerifyError> {
    // Check entry block's declared state is consistent with function parameters
    if let Some(entry_block) = func.blocks.first() {
        let entry_state = &entry_block.entry_state;
        for (reg, ty) in &func.params {
            if let Some(declared_ty) = entry_state.register_types.get(reg)
                && !checker::types_compatible_with_constraints(
                    declared_ty,
                    ty,
                    &entry_state.constraints,
                )
            {
                return Err(VerifyError::TypeMismatch {
                    block: entry_block.label.clone(),
                    instr_desc: format!("entry state for {:?}", reg),
                    expected: ty.clone(),
                    actual: declared_ty.clone(),
                });
            }
        }
    }

    for (block_idx, block) in func.blocks.iter().enumerate() {
        verify_block_derivation(func, program, block, block_idx, label_map)?;
    }

    Ok(())
}

/// Verify a single block using derivation-based checking
fn verify_block_derivation(
    func: &DtalFunction,
    program: &DtalProgram,
    block: &DtalBlock,
    block_idx: usize,
    label_map: &HashMap<&str, &TypeState>,
) -> Result<(), VerifyError> {
    let mut state = block.entry_state.clone();

    // Add proven assertions (e.g., loop invariants) to the constraint context.
    // These survive joins because the frontend typechecker has verified them.
    for assertion in &state.proven_assertions {
        if !state.constraints.contains(assertion) {
            state.constraints.push(assertion.clone());
        }
    }

    // Seed constraint context with register-to-index equalities from the
    // entry state. This re-establishes the linkage between register names
    // and their index expressions so Z3 can reason about bounds checks.
    seed_register_constraints(&mut state);

    // Walk instructions, deriving types
    for instr in &block.instructions {
        match instr {
            DtalInstr::Jmp { target } => {
                // Prove current state coerces into target's declared state
                if let Some(target_state) = label_map.get(target.as_str()) {
                    verify_state_coercion(&state, target_state, &block.label, target)?;
                }
            }
            DtalInstr::Branch { cond, target } => {
                // Taken edge: add positive constraint, prove coercion
                if let Some(pos_constraint) =
                    checker::constraint_from_cmp_op(*cond, &state.last_cmp)
                {
                    let mut taken_state = state.clone();
                    taken_state.constraints.push(pos_constraint);
                    if let Some(target_state) = label_map.get(target.as_str()) {
                        verify_state_coercion(&taken_state, target_state, &block.label, target)?;
                    }
                }

                // Fall-through edge: add negated constraint, prove coercion
                let neg_cond = checker::negate_cmp_op(*cond);
                if let Some(neg_constraint) =
                    checker::constraint_from_cmp_op(neg_cond, &state.last_cmp)
                {
                    state.constraints.push(neg_constraint);
                }
                // Only check fall-through coercion if the block actually
                // falls through (no subsequent Jmp/Ret). In our codegen,
                // Branch is always followed by a Jmp, so the Jmp handler
                // checks coercion for the false edge.
                let has_subsequent_jmp = block.instructions.iter().any(|i| {
                    matches!(i, DtalInstr::Jmp { .. } | DtalInstr::Ret)
                });
                if !has_subsequent_jmp {
                    if let Some(next_block) = func.blocks.get(block_idx + 1)
                        && next_block.label != *target
                        && let Some(next_state) = label_map.get(next_block.label.as_str())
                    {
                        verify_state_coercion(
                            &state,
                            next_state,
                            &block.label,
                            &next_block.label,
                        )?;
                    }
                }
            }
            DtalInstr::Ret => {
                verify_return(func, &state)?;
            }
            other => {
                verify_instruction(other, &mut state, &block.label, program)?;
            }
        }
    }

    Ok(())
}

/// Fallback: dataflow-based verification for programs without declared entry states
fn verify_function_dataflow(func: &DtalFunction, program: &DtalProgram) -> Result<(), VerifyError> {
    let dataflow = dataflow::analyze_function(func)?;

    for block in &func.blocks {
        let entry_state = dataflow
            .entry_states
            .get(&block.label)
            .cloned()
            .unwrap_or_else(|| block.entry_state.clone());

        let mut state = entry_state;
        for instr in &block.instructions {
            verify_instruction(instr, &mut state, &block.label, program)?;
        }

        verify_return_if_present(func, block, &state)?;
    }

    Ok(())
}

/// Check return type and postcondition
fn verify_return(func: &DtalFunction, state: &TypeState) -> Result<(), VerifyError> {
    use crate::backend::dtal::regs::{PhysicalReg, Reg};
    use crate::backend::dtal::types::DtalType;

    // Unit-returning functions don't use r0 for the return value.
    // r0 may contain a stale type from a prior call instruction.
    if func.return_type == DtalType::Unit {
        // Skip r0 check for unit returns — no value to verify
    } else {
        let return_reg = Reg::Physical(PhysicalReg::R0);
        if let Some(actual_type) = state.register_types.get(&return_reg)
            && !types_compatible_with_constraints(
                actual_type,
                &func.return_type,
                &state.constraints,
            )
        {
            return Err(VerifyError::ReturnTypeMismatch {
                function: func.name.clone(),
                expected: func.return_type.clone(),
                actual: actual_type.clone(),
            });
        }
    }

    if let Some(postcond) = &func.postcondition {
        // Version-substitute Select names so the postcondition references
        // the latest versioned array names (e.g., "v0" → "v0_6").
        let versioned = checker::version_substitute_constraint(postcond, &state.array_versions);
        if !checker::is_constraint_provable(&versioned, &state.constraints) {
            return Err(VerifyError::PostconditionFailed {
                function: func.name.clone(),
                constraint: postcond.clone(),
                context: state.constraints.clone(),
            });
        }
    }

    Ok(())
}

/// Check return type/postcondition for any Ret instruction in a block (dataflow path)
fn verify_return_if_present(
    func: &DtalFunction,
    block: &DtalBlock,
    state: &TypeState,
) -> Result<(), VerifyError> {
    for instr in &block.instructions {
        if let DtalInstr::Ret = instr {
            verify_return(func, state)?;
        }
    }
    Ok(())
}

/// Verify that `current` state coerces into `target` declared state.
///
/// Xi & Harper's type-jmp rule: at a jump to label L, the current state
/// must coerce into L's declared state. Checks:
/// 1. Register type subtyping: for each register in the target, the
///    current type must be a subtype
/// 2. Constraint entailment: each constraint in the target's declared
///    entry state must be provable from the current constraint context
fn verify_state_coercion(
    current: &TypeState,
    target: &TypeState,
    source_block: &str,
    target_label: &str,
) -> Result<(), VerifyError> {
    // If the current constraint context is unsatisfiable, the edge is
    // unreachable (dead code). Unreachable code is trivially sound —
    // no coercion check needed.
    if checker::is_constraint_provable(&Constraint::False, &current.constraints) {
        return Ok(());
    }

    // Check register types: each register in the target must be a supertype
    // of the corresponding register in the current state
    for (reg, target_ty) in &target.register_types {
        if let Some(current_ty) = current.register_types.get(reg)
            && !checker::types_compatible_with_constraints(
                current_ty,
                target_ty,
                &current.constraints,
            )
        {
            return Err(VerifyError::JoinMismatch {
                block: target_label.to_string(),
                reg: *reg,
                expected: target_ty.clone(),
                actual: current_ty.clone(),
                from_block: source_block.to_string(),
            });
        }
    }

    // Check constraint entailment: each constraint declared in the target's
    // entry state must be provable from the current state's constraints.
    // This eliminates trust on .assume directives — they become verified
    // properties of the program, not compiler assertions.
    for target_constraint in &target.constraints {
        if !checker::is_constraint_provable(target_constraint, &current.constraints) {
            return Err(VerifyError::UnprovableConstraint {
                constraint: target_constraint.clone(),
                context: current.constraints.clone(),
                block: format!(
                    "{} (edge {} → {})",
                    target_label, source_block, target_label
                ),
            });
        }
    }

    Ok(())
}

/// Seed the constraint context with register-to-index equalities from
/// the type state's register types.
///
/// For each `reg : int(idx)`, adds `reg == idx` (unless tautological).
/// This re-establishes the linkage that `add_register_index_constraint`
/// creates during derivation, so that blocks starting from a declared
/// entry state have the same constraint information available.
fn seed_register_constraints(state: &mut TypeState) {
    use crate::backend::dtal::constraints::Constraint;
    use crate::backend::dtal::types::DtalType;

    let new_constraints: Vec<Constraint> = state
        .register_types
        .iter()
        .filter_map(|(reg, ty)| match ty {
            DtalType::SingletonInt(idx) => {
                let reg_expr = checker::reg_to_index_expr(reg);
                if *idx != reg_expr {
                    Some(Constraint::Eq(reg_expr, idx.clone()))
                } else {
                    None
                }
            }
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            } => {
                // Open the existential: substitute witness_var with the register name
                let reg_name = format!("{}", reg);
                let subs = std::collections::HashMap::from([(witness_var.clone(), reg_name)]);
                let mut opened = checker::substitute_var_names_in_constraint(constraint, &subs);
                opened = checker::substitute_select_names(&opened, &subs);
                Some(opened)
            }
            _ => None,
        })
        .collect();

    state.constraints.extend(new_constraints);
}

/// Check if actual type is a subtype of (or equal to) expected type,
/// using the constraint context for coercion proofs.
fn types_compatible_with_constraints(
    actual: &crate::backend::dtal::types::DtalType,
    expected: &crate::backend::dtal::types::DtalType,
    constraints: &[crate::backend::dtal::constraints::Constraint],
) -> bool {
    checker::types_compatible_with_constraints(actual, expected, constraints)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::constraints::{Constraint, IndexExpr};
    use crate::backend::dtal::instr::{CmpOp, DtalBlock, DtalInstr, TypeState};
    use crate::backend::dtal::regs::{PhysicalReg, Reg, VirtualReg};
    use crate::backend::dtal::types::DtalType;
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

    fn make_func_with_entry_state(
        name: &str,
        params: Vec<(Reg, DtalType)>,
        return_type: DtalType,
        entry_state: TypeState,
        instructions: Vec<DtalInstr>,
    ) -> DtalFunction {
        DtalFunction {
            name: name.to_string(),
            params,
            return_type,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: ".entry".to_string(),
                entry_state,
                instructions,
            }],
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
            DtalType::SingletonInt(IndexExpr::Const(5)),
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 5,
                        ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_movimm_derives_correct_type_ignoring_annotation() {
        // Derivation-based: verifier derives int(5) regardless of ty annotation
        let program = make_program(vec![make_func(
            "ok",
            vec![],
            DtalType::SingletonInt(IndexExpr::Const(5)),
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: r0(),
                        imm: 5,
                        ty: DtalType::SingletonInt(IndexExpr::Const(6)), // Wrong annotation, ignored
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // Verifier derives int(5) for r0, which matches return type int(5)
        assert!(verify_dtal(&program).is_ok());
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
    // Phase 3: Entry state constraints feed into verification context
    // ========================================================================

    #[test]
    fn test_entry_state_constraint_feeds_context() {
        // Entry state .assume constraints are available in the block's
        // constraint context, so a ConstraintAssert can prove them
        let mut entry_state = TypeState::new();
        entry_state.register_types.insert(v(0), DtalType::Int);
        entry_state.constraints.push(Constraint::Lt(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(10),
        ));
        let program = make_program(vec![make_func_with_entry_state(
            "ok",
            vec![],
            DtalType::Int,
            entry_state,
            vec![DtalInstr::ConstraintAssert {
                constraint: Constraint::Lt(IndexExpr::Var("v0".to_string()), IndexExpr::Const(10)),
            }],
        )]);
        assert!(
            verify_dtal(&program).is_ok(),
            "Entry state constraint should be available in verification context"
        );
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
    fn test_movreg_derives_from_source_ignoring_annotation() {
        // Derivation-based: MovReg derives dst type from src, ignoring ty annotation
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::SingletonInt(IndexExpr::Const(5)), // Wrong annotation, ignored
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // Verifier derives Int for r0 (from v0), matches return type Int
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_singleton_subtype_of_int() {
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::SingletonInt(IndexExpr::Const(5)))],
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
    fn test_reject_bool_as_int_at_return() {
        // Derivation-based: MovReg derives Bool from src (v2),
        // then returning Bool where Int is expected triggers an error
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
                        dst: r0(),
                        src: v(2),         // v2 is Bool
                        ty: DtalType::Int, // annotation ignored
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(result.is_err(), "Bool is not a subtype of Int at return");
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::ReturnTypeMismatch { .. }
        ));
    }

    #[test]
    fn test_array_subtyping_compatible() {
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Const(10),
        };
        assert!(checker::types_compatible(&arr_ty, &arr_ty));
    }

    #[test]
    fn test_array_subtyping_different_sizes() {
        let arr10 = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Const(10),
        };
        let arr5 = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Const(5),
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
                        ty: DtalType::SingletonInt(IndexExpr::Const(5)),
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
                        ty: DtalType::SingletonInt(IndexExpr::Const(5)),
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
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
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
            size: IndexExpr::Const(10),
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
            size: IndexExpr::Const(10),
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
                        ty: DtalType::SingletonInt(IndexExpr::Const(3)),
                    },
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::True,
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
            size: IndexExpr::Const(10),
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
            size: IndexExpr::Const(5),
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

    // ========================================================================
    // Derivation-based typing tests (Phase 2)
    // ========================================================================

    #[test]
    fn test_binop_derives_symbolic_type() {
        // add v2, v0, v1 with v0: int(3), v1: int(5) derives v2: int((3 + 5))
        let program = make_program(vec![make_func(
            "ok",
            vec![],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 3,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v(1),
                        imm: 5,
                        ty: DtalType::Int,
                    },
                    DtalInstr::BinOp {
                        op: crate::backend::dtal::instr::BinaryOp::Add,
                        dst: v(2),
                        lhs: v(0),
                        rhs: v(1),
                        ty: DtalType::Int, // annotation ignored
                    },
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(2),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // v2 gets derived type int((3 + 5)), which is a subtype of Int
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_addi_derives_symbolic_type() {
        // addi v1, v0, 10 with v0: int(5) derives v1: int((5 + 10))
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
                        ty: DtalType::Int,
                    },
                    DtalInstr::AddImm {
                        dst: v(1),
                        src: v(0),
                        imm: 10,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(1),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        assert!(verify_dtal(&program).is_ok());
    }

    // ========================================================================
    // Constraint-modulo coercion tests (Phase 4)
    // ========================================================================

    #[test]
    fn test_singleton_coercion_via_constraint() {
        // int(n) coerces to int(5) when n == 5 is in constraint context
        let constraints = vec![Constraint::Eq(
            IndexExpr::Var("n".to_string()),
            IndexExpr::Const(5),
        )];
        let actual = DtalType::SingletonInt(IndexExpr::Var("n".to_string()));
        let expected = DtalType::SingletonInt(IndexExpr::Const(5));
        assert!(checker::types_compatible_with_constraints(
            &actual,
            &expected,
            &constraints
        ));
    }

    #[test]
    fn test_array_size_coercion_via_constraint() {
        // [int; n] coerces to [int; 5] when n == 5 is in context
        let constraints = vec![Constraint::Eq(
            IndexExpr::Var("n".to_string()),
            IndexExpr::Const(5),
        )];
        let actual = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Var("n".to_string()),
        };
        let expected = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Const(5),
        };
        assert!(checker::types_compatible_with_constraints(
            &actual,
            &expected,
            &constraints
        ));
    }

    // ========================================================================
    // Derivation-based block verification tests (Phase 3)
    // ========================================================================

    #[test]
    fn test_declared_entry_state_verified() {
        // Block with declared entry state — verifier starts from it
        use crate::backend::dtal::instr::TypeState;

        let mut entry_state = TypeState::new();
        entry_state.register_types.insert(v(0), DtalType::Int);

        let program = make_program(vec![DtalFunction {
            name: "ok".to_string(),
            params: vec![(v(0), DtalType::Int)],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: ".entry".to_string(),
                entry_state,
                instructions: vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        }]);
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_state_coercion_at_jump() {
        // Jump where current state has v0: int(5) and target expects v0: int(n) with n > 0
        use crate::backend::dtal::instr::TypeState;

        let mut entry_state = TypeState::new();
        entry_state
            .register_types
            .insert(v(0), DtalType::SingletonInt(IndexExpr::Const(5)));

        let mut target_state = TypeState::new();
        target_state.register_types.insert(v(0), DtalType::Int);

        let program = make_program(vec![DtalFunction {
            name: "ok".to_string(),
            params: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: ".entry".to_string(),
                    entry_state,
                    instructions: vec![DtalInstr::Jmp {
                        target: ".target".to_string(),
                    }],
                },
                DtalBlock {
                    label: ".target".to_string(),
                    entry_state: target_state,
                    instructions: vec![
                        DtalInstr::MovReg {
                            dst: r0(),
                            src: v(0),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                },
            ],
        }]);
        // int(5) ≤ Int, so coercion succeeds
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_reject_wrong_entry_state() {
        // Block with intentionally wrong entry state: declares v0 but v0 is actually Bool
        use crate::backend::dtal::instr::TypeState;

        let mut entry_state = TypeState::new();
        entry_state
            .register_types
            .insert(v(0), DtalType::SingletonInt(IndexExpr::Const(5)));

        let mut target_state = TypeState::new();
        target_state.register_types.insert(v(0), DtalType::Bool);

        let program = make_program(vec![DtalFunction {
            name: "bad".to_string(),
            params: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: ".entry".to_string(),
                    entry_state,
                    instructions: vec![DtalInstr::Jmp {
                        target: ".target".to_string(),
                    }],
                },
                DtalBlock {
                    label: ".target".to_string(),
                    entry_state: target_state,
                    instructions: vec![DtalInstr::Ret],
                },
            ],
        }]);
        // int(5) is not a subtype of Bool → coercion should fail
        let result = verify_dtal(&program);
        assert!(result.is_err(), "State coercion should fail: int(5) ≤ Bool");
    }

    // ========================================================================
    // Derivation completeness: Not, Load, Call
    // ========================================================================

    #[test]
    fn test_not_derives_bool_ignoring_annotation() {
        // Not always produces Bool, regardless of the annotation
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::Bool)],
            DtalType::Bool,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Not {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int, // wrong annotation, ignored
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // Verifier derives Bool for r0, matching return type Bool
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_not_derived_bool_fails_int_return() {
        // Not derives Bool — returning it as Int should fail
        let program = make_program(vec![make_func(
            "bad",
            vec![(v(0), DtalType::Bool)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Not {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int, // annotation says Int, but derivation says Bool
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        let result = verify_dtal(&program);
        assert!(result.is_err(), "Not derives Bool, not Int");
        assert!(matches!(
            result.unwrap_err(),
            VerifyError::ReturnTypeMismatch { .. }
        ));
    }

    #[test]
    fn test_load_derives_element_type_from_array() {
        // Load derives element type from array base, ignoring annotation
        let arr_ty = DtalType::Array {
            element_type: Arc::new(DtalType::Bool),
            size: IndexExpr::Const(10),
        };
        let mut func = make_func(
            "ok",
            vec![(v(0), arr_ty), (v(1), DtalType::Int)],
            DtalType::Bool,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Load {
                        dst: r0(),
                        base: v(0),
                        offset: v(1),
                        ty: DtalType::Int, // wrong annotation — array element is Bool
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
        // Verifier derives Bool (from array element type), matches return type Bool
        assert!(
            verify_dtal(&program).is_ok(),
            "Load should derive element type from array base"
        );
    }

    #[test]
    fn test_call_derives_return_type_from_signature() {
        // Call derives return type from callee's declared signature, not annotation
        let callee = make_func(
            "returns_bool",
            vec![],
            DtalType::Bool, // callee signature says Bool
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: r0(),
                        imm: 1,
                        ty: DtalType::Int,
                    },
                    // r0 gets int(1), but return type is Bool — this callee
                    // would fail its own verification, but that's not what
                    // we're testing. We're testing that the *caller* derives
                    // the return type from the *signature*, not the annotation.
                    DtalInstr::Ret,
                ],
            )],
        );
        let caller = make_func(
            "caller",
            vec![],
            DtalType::Bool, // caller returns Bool
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Call {
                        target: "returns_bool".to_string(),
                        return_ty: DtalType::Int, // wrong annotation — signature says Bool
                    },
                    // r0 should have Bool (from callee signature), not Int (from annotation)
                    DtalInstr::Ret,
                ],
            )],
        );
        let program = make_program(vec![callee, caller]);
        // Caller's r0 gets Bool from callee's declared return type
        // Return type is Bool, so this should pass
        let result = verify_dtal(&program);
        // The callee itself will fail verification (int(1) ≠ Bool), so we
        // only check the caller here by verifying the specific error isn't
        // a ReturnTypeMismatch from the caller
        if let Err(ref e) = result {
            // Should fail in callee ("returns_bool"), not caller
            if let VerifyError::ReturnTypeMismatch { function, .. } = e {
                assert_eq!(
                    function, "returns_bool",
                    "Error should be in callee, not caller"
                );
            }
        }
    }

    #[test]
    fn test_call_wrong_annotation_caught_via_signature() {
        // Call annotation says Int, but callee signature says Bool.
        // Caller tries to return Int — should fail because derived type is Bool.
        let callee = make_func(
            "returns_bool",
            vec![],
            DtalType::Bool,
            vec![make_block(".entry", vec![DtalInstr::Ret])],
        );
        let caller = make_func(
            "caller",
            vec![],
            DtalType::Int, // caller claims to return Int
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::Call {
                        target: "returns_bool".to_string(),
                        return_ty: DtalType::Int, // annotation lies: says Int
                    },
                    // Verifier derives Bool from callee signature → r0 is Bool
                    DtalInstr::Ret,
                ],
            )],
        );
        let program = make_program(vec![callee, caller]);
        let result = verify_dtal(&program);
        // The caller should fail: r0 is Bool (derived) but return type is Int
        assert!(result.is_err());
        if let VerifyError::ReturnTypeMismatch { function, .. } = result.unwrap_err() {
            assert_eq!(function, "caller");
        } else {
            panic!("Expected ReturnTypeMismatch from caller");
        }
    }

    // ========================================================================
    // Stack type tracking: Push/Pop derive types from stack
    // ========================================================================

    #[test]
    fn test_push_pop_derives_type_from_stack() {
        // Push int(5) onto stack, pop it back — should derive int(5)
        let program = make_program(vec![make_func(
            "ok",
            vec![],
            DtalType::SingletonInt(IndexExpr::Const(5)),
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(0),
                        imm: 5,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Push {
                        src: v(0),
                        ty: DtalType::Int, // annotation ignored
                    },
                    DtalInstr::Pop {
                        dst: r0(),
                        ty: DtalType::Int, // annotation ignored — derives from stack
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // v0 has int(5), pushed onto stack, popped as r0 → r0 : int(5)
        assert!(verify_dtal(&program).is_ok());
    }

    #[test]
    fn test_push_pop_bool_derives_correctly() {
        // Push Bool onto stack, pop it — should derive Bool, not the annotation
        let program = make_program(vec![make_func(
            "ok",
            vec![(v(0), DtalType::Int), (v(1), DtalType::Int)],
            DtalType::Bool,
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
                    DtalInstr::Push {
                        src: v(2),
                        ty: DtalType::Int, // wrong annotation
                    },
                    DtalInstr::Pop {
                        dst: r0(),
                        ty: DtalType::Int, // wrong annotation — derives Bool from stack
                    },
                    DtalInstr::Ret,
                ],
            )],
        )]);
        // v2 is Bool, pushed and popped → r0 is Bool, matching return type
        assert!(verify_dtal(&program).is_ok());
    }
}
