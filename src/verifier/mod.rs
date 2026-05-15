//! DTAL verifier.
//!
//! The verifier independently checks generated DTAL before machine-code
//! emission. It validates instruction typing, ownership state, CFG joins,
//! contracts, and constraint provability.
//!
//! # Verification Flow
//!
//! ```text
//! DTAL text
//!   -> parser
//!   -> DtalProgram
//!   -> per-function verifier
//!        |- declared block-state derivation, or
//!        `- fallback dataflow analysis
//!   -> Result<(), VerifyError>
//! ```
//!
//! # Design Notes
//!
//! Generated physical DTAL is treated as untrusted input. The verifier derives
//! types and ownership state from instructions and rejects annotations that are
//! not justified by the current state. When block entry states are present, the
//! verifier checks each edge against those declarations; otherwise it computes
//! a conservative dataflow fixed point.
//!
//! # Errors
//!
//! [`VerifyTextError`] separates parse failures from semantic verification
//! failures. Semantic failures use [`VerifyError`] so callers can distinguish
//! type, ownership, bounds, contract, and constraint errors.
//!
//! # Related Modules
//!
//! - `checker` verifies individual DTAL instructions.
//! - `dataflow` computes fallback block states.
//! - [`smt`] proves arithmetic and array constraints.

#![allow(clippy::result_large_err)]

pub(crate) mod checker;
pub(crate) mod dataflow;
mod error;
pub(crate) mod ownership;
pub mod smt;
mod text;

pub use error::VerifyError;
pub use text::{VerifyTextError, verify_dtal_text};

use crate::dtal::constraints::Constraint;
use crate::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use checker::verify_instruction;
use std::collections::HashMap;

pub fn verify_dtal(program: &DtalProgram) -> Result<(), VerifyError> {
    for func in &program.functions {
        verify_function(func, program)?;
    }
    Ok(())
}

fn verify_function(func: &DtalFunction, program: &DtalProgram) -> Result<(), VerifyError> {
    let label_map: HashMap<&str, &TypeState> = func
        .blocks
        .iter()
        .map(|b| (b.label.as_str(), &b.entry_state))
        .collect();

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

fn verify_function_derivation(
    func: &DtalFunction,
    program: &DtalProgram,
    label_map: &HashMap<&str, &TypeState>,
) -> Result<(), VerifyError> {
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

fn verify_block_derivation(
    func: &DtalFunction,
    program: &DtalProgram,
    block: &DtalBlock,
    block_idx: usize,
    label_map: &HashMap<&str, &TypeState>,
) -> Result<(), VerifyError> {
    let mut state = block.entry_state.clone();

    for assertion in &state.proven_assertions {
        if !state.constraints.contains(assertion) {
            state.constraints.push(assertion.clone());
        }
    }

    seed_register_constraints(&mut state);

    for instr in &block.instructions {
        match instr {
            DtalInstr::Jmp { target } => {
                if let Some(target_state) = label_map.get(target.as_str()) {
                    verify_state_coercion(&state, target_state, &block.label, target)?;
                }
            }
            DtalInstr::Branch { cond, target } => {
                if let Some(pos_constraint) =
                    checker::constraint_from_cmp_op(*cond, &state.last_cmp)
                {
                    let mut taken_state = state.clone();
                    taken_state.constraints.push(pos_constraint);
                    if let Some(target_state) = label_map.get(target.as_str()) {
                        verify_state_coercion(&taken_state, target_state, &block.label, target)?;
                    }
                }

                let neg_cond = checker::negate_cmp_op(*cond);
                if let Some(neg_constraint) =
                    checker::constraint_from_cmp_op(neg_cond, &state.last_cmp)
                {
                    state.constraints.push(neg_constraint);
                }
                let has_subsequent_jmp = block
                    .instructions
                    .iter()
                    .any(|i| matches!(i, DtalInstr::Jmp { .. } | DtalInstr::Ret));
                if !has_subsequent_jmp
                    && let Some(next_block) = func.blocks.get(block_idx + 1)
                    && next_block.label != *target
                    && let Some(next_state) = label_map.get(next_block.label.as_str())
                {
                    verify_state_coercion(&state, next_state, &block.label, &next_block.label)?;
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

fn verify_function_dataflow(func: &DtalFunction, program: &DtalProgram) -> Result<(), VerifyError> {
    let dataflow = dataflow::analyze_function(func)?;

    for block in &func.blocks {
        let entry_state = dataflow
            .entry_states
            .get(&block.label)
            .cloned()
            .unwrap_or_else(|| block.entry_state.clone());

        let mut state = entry_state;
        seed_register_constraints(&mut state);
        for instr in &block.instructions {
            verify_instruction(instr, &mut state, &block.label, program)?;
        }

        verify_return_if_present(func, block, &state)?;
    }

    Ok(())
}

fn verify_return(func: &DtalFunction, state: &TypeState) -> Result<(), VerifyError> {
    use crate::dtal::regs::{PhysicalReg, Reg};
    use crate::dtal::types::DtalType;

    if func.return_type == DtalType::Unit {
    } else {
        let is_physical = func.blocks.iter().any(|b| {
            b.instructions
                .iter()
                .any(|i| matches!(i, DtalInstr::Prologue { .. }))
        });

        let return_reg = if is_physical {
            Reg::Physical(PhysicalReg::LR)
        } else {
            Reg::Physical(PhysicalReg::R0)
        };

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

fn verify_state_coercion(
    current: &TypeState,
    target: &TypeState,
    source_block: &str,
    target_label: &str,
) -> Result<(), VerifyError> {
    if checker::is_constraint_provable(&Constraint::False, &current.constraints) {
        return Ok(());
    }

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

    for reg in &target.owned_registers {
        if !current.owned_registers.contains(reg) {
            return Err(VerifyError::JoinMismatch {
                block: target_label.to_string(),
                reg: *reg,
                expected: current
                    .register_types
                    .get(reg)
                    .cloned()
                    .unwrap_or(crate::dtal::types::DtalType::Int),
                actual: target
                    .register_types
                    .get(reg)
                    .cloned()
                    .unwrap_or(crate::dtal::types::DtalType::Int),
                from_block: source_block.to_string(),
            });
        }
    }

    for (reg, target_object_id) in &target.owned_object_ids {
        if current.owned_object_ids.get(reg).copied() != Some(*target_object_id) {
            return Err(VerifyError::OwnershipViolation {
                block: target_label.to_string(),
                instr_desc: "state coercion".to_string(),
                msg: format!(
                    "register {:?} must own object o{} on edge {} -> {}",
                    reg, target_object_id, source_block, target_label
                ),
            });
        }
    }

    ownership::verify_unique_owned_objects(current, source_block, "state coercion")?;
    ownership::verify_unique_owned_objects(target, target_label, "state coercion target")?;

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

fn seed_register_constraints(state: &mut TypeState) {
    use crate::dtal::constraints::Constraint;
    use crate::dtal::types::DtalType;

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
                let reg_name = format!("{}", reg);
                let subs = std::collections::HashMap::from([(witness_var.clone(), reg_name)]);
                let mut opened = checker::substitute_var_names_in_constraint(constraint, &subs);
                opened = checker::substitute_select_names(&opened, &subs);
                Some(opened)
            }
            DtalType::RefinedInt {
                var, constraint, ..
            } => {
                let reg_name = format!("{}", reg);
                let subs = std::collections::HashMap::from([(var.clone(), reg_name)]);
                let projected = checker::substitute_var_names_in_constraint(constraint, &subs);
                Some(projected)
            }
            _ => None,
        })
        .collect();

    state.constraints.extend(new_constraints);
}

fn types_compatible_with_constraints(
    actual: &crate::dtal::types::DtalType,
    expected: &crate::dtal::types::DtalType,
    constraints: &[crate::dtal::constraints::Constraint],
) -> bool {
    checker::types_compatible_with_constraints(actual, expected, constraints)
}
#[cfg(test)]
mod tests;
