//! Type state dataflow analysis
//!
//! This module computes type states at block entries using forward dataflow analysis.

#![allow(clippy::result_large_err)]

use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::common::types::IType;
use crate::verifier::error::VerifyError;
use std::collections::{HashMap, HashSet};

/// Result of dataflow analysis
#[allow(dead_code)]
pub struct DataflowResult<'src> {
    /// Type state at entry of each block
    pub entry_states: HashMap<String, TypeState<'src>>,
    /// Type state at exit of each block
    pub exit_states: HashMap<String, TypeState<'src>>,
    /// Predecessor blocks for each block
    pub predecessors: HashMap<String, Vec<String>>,
}

/// Compute type states for all blocks in a function
pub fn analyze_function<'src>(
    func: &DtalFunction<'src>,
) -> Result<DataflowResult<'src>, VerifyError<'src>> {
    // Build CFG structure
    let predecessors = compute_predecessors(func);

    // Initialize entry states
    let mut entry_states: HashMap<String, TypeState<'src>> = HashMap::new();
    let mut exit_states: HashMap<String, TypeState<'src>> = HashMap::new();

    // Set entry state for first block (function parameters)
    if let Some(entry_block) = func.blocks.first() {
        let mut entry_state = TypeState::new();

        // Initialize with function parameters
        for (reg, ty) in &func.params {
            entry_state.register_types.insert(*reg, ty.clone());
        }

        // Add precondition to constraints
        if let Some(precond) = &func.precondition {
            entry_state.constraints.push(precond.clone());
        }

        entry_states.insert(entry_block.label.clone(), entry_state);
    }

    // Fixed-point iteration
    let mut changed = true;
    let mut iterations = 0;
    const MAX_ITERATIONS: usize = 100;

    // Get entry block label for special handling
    let entry_block_label = func.blocks.first().map(|b| b.label.clone());

    while changed && iterations < MAX_ITERATIONS {
        changed = false;
        iterations += 1;

        for block in &func.blocks {
            // Compute entry state - always recompute from predecessors
            // (except for entry block which keeps function parameters)
            let entry_state = if Some(&block.label) == entry_block_label.as_ref() {
                // Entry block: use cached state with function parameters
                entry_states
                    .get(&block.label)
                    .cloned()
                    .unwrap_or_else(|| block.entry_state.clone())
            } else {
                // Non-entry blocks: always recompute from predecessors
                let preds = predecessors.get(&block.label).cloned().unwrap_or_default();
                if preds.is_empty() {
                    // No predecessors - use block's declared entry state
                    block.entry_state.clone()
                } else {
                    // Join from predecessors
                    join_states(&preds, &exit_states)?
                }
            };

            // Compute exit state by processing instructions
            let exit_state = compute_exit_state(block, &entry_state)?;

            // Check if exit state changed
            let old_exit = exit_states.get(&block.label);
            if old_exit
                .map(|s| !states_equal(s, &exit_state))
                .unwrap_or(true)
            {
                changed = true;
                exit_states.insert(block.label.clone(), exit_state);
            }

            // Update entry state if it changed
            if !entry_states.contains_key(&block.label)
                || !states_equal(entry_states.get(&block.label).unwrap(), &entry_state)
            {
                entry_states.insert(block.label.clone(), entry_state);
            }
        }
    }

    Ok(DataflowResult {
        entry_states,
        exit_states,
        predecessors,
    })
}

/// Compute predecessor blocks for each block
fn compute_predecessors<'src>(func: &DtalFunction<'src>) -> HashMap<String, Vec<String>> {
    let mut predecessors: HashMap<String, Vec<String>> = HashMap::new();

    // Initialize all blocks with empty predecessor lists
    for block in &func.blocks {
        predecessors.insert(block.label.clone(), Vec::new());
    }

    // Find successors and build predecessor lists
    for block in &func.blocks {
        let successors = get_block_successors(block);
        for succ in successors {
            if let Some(preds) = predecessors.get_mut(&succ) {
                preds.push(block.label.clone());
            }
        }
    }

    predecessors
}

/// Get successor block labels from a block
fn get_block_successors(block: &DtalBlock) -> Vec<String> {
    let mut successors = Vec::new();

    for instr in &block.instructions {
        match instr {
            DtalInstr::Jmp { target } => {
                successors.push(target.clone());
            }
            DtalInstr::Branch { target, .. } => {
                successors.push(target.clone());
            }
            _ => {}
        }
    }

    successors
}

/// Join type states from multiple predecessors
fn join_states<'src>(
    pred_labels: &[String],
    exit_states: &HashMap<String, TypeState<'src>>,
) -> Result<TypeState<'src>, VerifyError<'src>> {
    let mut result = TypeState::new();

    // Collect all registers that appear in any predecessor
    let mut all_regs: HashSet<Reg> = HashSet::new();
    for label in pred_labels {
        if let Some(state) = exit_states.get(label) {
            for reg in state.register_types.keys() {
                all_regs.insert(*reg);
            }
        }
    }

    // For each register, compute join of types
    for reg in all_regs {
        let mut types: Vec<IType<'src>> = Vec::new();

        for label in pred_labels {
            if let Some(state) = exit_states.get(label)
                && let Some(ty) = state.register_types.get(&reg)
            {
                types.push(ty.clone());
            }
        }

        if !types.is_empty() {
            // Compute join (least upper bound) of types
            let joined_type = join_types(&types);
            result.register_types.insert(reg, joined_type);
        }
    }

    // Join constraints - take intersection (constraints true on all paths)
    // For simplicity, start with first predecessor's constraints
    if let Some(first_label) = pred_labels.first()
        && let Some(first_state) = exit_states.get(first_label)
    {
        // Only keep constraints that appear in all predecessors
        for constraint in &first_state.constraints {
            let in_all = pred_labels.iter().skip(1).all(|label| {
                exit_states
                    .get(label)
                    .map(|s| s.constraints.contains(constraint))
                    .unwrap_or(false)
            });
            if in_all {
                result.constraints.push(constraint.clone());
            }
        }
    }

    Ok(result)
}

/// Join multiple types into their least upper bound
fn join_types<'src>(types: &[IType<'src>]) -> IType<'src> {
    if types.is_empty() {
        return IType::Int; // Default
    }

    if types.len() == 1 {
        return types[0].clone();
    }

    // Check if all types are the same
    let first = &types[0];
    if types.iter().all(|t| types_structurally_equal(t, first)) {
        return first.clone();
    }

    // If different singleton ints, generalize to int
    let all_singleton_ints = types
        .iter()
        .all(|t| matches!(t, IType::SingletonInt(_) | IType::Int));
    if all_singleton_ints {
        return IType::Int;
    }

    // If different refined ints, generalize to int
    let all_numeric = types.iter().all(|t| {
        matches!(
            t,
            IType::Int | IType::SingletonInt(_) | IType::RefinedInt { .. }
        )
    });
    if all_numeric {
        return IType::Int;
    }

    // Default: return first type (conservative)
    first.clone()
}

/// Check structural equality of types
fn types_structurally_equal<'src>(a: &IType<'src>, b: &IType<'src>) -> bool {
    match (a, b) {
        (IType::Unit, IType::Unit) => true,
        (IType::Int, IType::Int) => true,
        (IType::Bool, IType::Bool) => true,
        (IType::SingletonInt(va), IType::SingletonInt(vb)) => va == vb,
        (
            IType::Array {
                element_type: ea,
                size: sa,
            },
            IType::Array {
                element_type: eb,
                size: sb,
            },
        ) => types_structurally_equal(ea, eb) && sa == sb,
        (IType::Ref(a), IType::Ref(b)) => types_structurally_equal(a, b),
        (IType::RefMut(a), IType::RefMut(b)) => types_structurally_equal(a, b),
        (IType::Master(a), IType::Master(b)) => types_structurally_equal(a, b),
        // Refined types need more sophisticated comparison
        _ => false,
    }
}

/// Compute exit state by processing all instructions (non-verifying)
///
/// This function only updates the type state based on instruction definitions.
/// It does NOT verify that operands are defined - that's done separately after
/// the dataflow analysis reaches a fixed point.
fn compute_exit_state<'src>(
    block: &DtalBlock<'src>,
    entry_state: &TypeState<'src>,
) -> Result<TypeState<'src>, VerifyError<'src>> {
    let mut state = entry_state.clone();

    for instr in &block.instructions {
        update_state_for_instruction(instr, &mut state);
    }

    Ok(state)
}

/// Update type state based on instruction definitions (non-verifying)
fn update_state_for_instruction<'src>(instr: &DtalInstr<'src>, state: &mut TypeState<'src>) {
    match instr {
        DtalInstr::MovImm { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::MovReg { dst, src, ty } => {
            // Use explicit type if provided, otherwise inherit from source
            let ty = if matches!(ty, IType::Int) {
                state.register_types.get(src).cloned().unwrap_or(ty.clone())
            } else {
                ty.clone()
            };
            state.register_types.insert(*dst, ty);
        }
        DtalInstr::BinOp { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::AddImm { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::Load { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::SetCC { dst, .. } => {
            state.register_types.insert(*dst, IType::Bool);
        }
        DtalInstr::Not { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::TypeAnnotation { reg, ty } => {
            state.register_types.insert(*reg, ty.clone());
        }
        DtalInstr::ConstraintAssume { constraint } => {
            state.constraints.push(constraint.clone());
        }
        DtalInstr::Pop { dst, ty } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::Alloca { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        // Instructions that don't define registers
        DtalInstr::Store { .. }
        | DtalInstr::Cmp { .. }
        | DtalInstr::CmpImm { .. }
        | DtalInstr::Push { .. }
        | DtalInstr::ConstraintAssert { .. }
        | DtalInstr::Jmp { .. }
        | DtalInstr::Branch { .. }
        | DtalInstr::Call { .. }
        | DtalInstr::Ret => {}
    }
}

/// Check if two type states are equal
fn states_equal<'src>(a: &TypeState<'src>, b: &TypeState<'src>) -> bool {
    if a.register_types.len() != b.register_types.len() {
        return false;
    }

    for (reg, ty_a) in &a.register_types {
        match b.register_types.get(reg) {
            Some(ty_b) => {
                if !types_structurally_equal(ty_a, ty_b) {
                    return false;
                }
            }
            None => return false,
        }
    }

    // Also check constraints
    a.constraints.len() == b.constraints.len()
        && a.constraints.iter().all(|c| b.constraints.contains(c))
}
