//! Forward dataflow analysis for DTAL type states.
//!
//! This fallback analysis computes block entry states for DTAL that does not
//! yet declare explicit block states. It also carries branch-refined
//! constraints along individual CFG edges.
//!
//! # Fixed Point
//!
//! ```text
//! entry state
//!    |
//!    v
//! block transfer -> edge states
//!    |                 |
//!    `---- join <------`
//!          |
//!          v
//! repeat until stable
//! ```
//!
//! # Design Notes
//!
//! Declared block states are preferred when available because they make joins
//! explicit and easier to audit. This module exists for DTAL without those
//! declarations and therefore computes the least common state accepted by all
//! predecessors. Branch constraints are stored per edge so a taken branch can
//! prove facts that are not valid on the fallthrough edge.
//!
//! # Errors
//!
//! Analysis returns [`VerifyError`] when instruction transfer fails, when
//! predecessor states cannot be joined, or when the fixed point exposes an
//! invalid return obligation.
//!
//! # Related Modules
//!
//! The `checker` module performs validating instruction derivation; this module
//! mirrors the type effects needed for state propagation.

#![allow(clippy::result_large_err)]

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{CmpOperands, DtalBlock, DtalFunction, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
use crate::verifier::checker::{self, constraint_from_cmp_op, extract_index, negate_cmp_op};
use crate::verifier::error::VerifyError;
use std::collections::{HashMap, HashSet};

/// Entry, exit, edge, and predecessor state computed for a function.
#[allow(dead_code)]
pub struct DataflowResult {
    /// Type state at entry of each block.
    pub entry_states: HashMap<String, TypeState>,
    /// Type state at exit of each block.
    pub exit_states: HashMap<String, TypeState>,
    /// Per-edge exit states keyed by `(source_label, target_label)`.
    pub edge_states: HashMap<(String, String), TypeState>,
    /// Predecessor blocks for each block.
    pub predecessors: HashMap<String, Vec<String>>,
}

/// Compute block entry and exit states for a function.
///
/// # Errors
///
/// Returns [`VerifyError`] if transfer or join logic discovers an invalid DTAL
/// state while computing the fixed point.
pub fn analyze_function(func: &DtalFunction) -> Result<DataflowResult, VerifyError> {
    let predecessors = compute_predecessors(func);

    let mut entry_states: HashMap<String, TypeState> = HashMap::new();
    let mut exit_states: HashMap<String, TypeState> = HashMap::new();
    let mut edge_states: HashMap<(String, String), TypeState> = HashMap::new();

    if let Some(entry_block) = func.blocks.first() {
        let mut entry_state = TypeState::new();

        for ((reg, ty), param_kind) in func.params.iter().zip(func.parameter_kinds.iter()) {
            entry_state.register_types.insert(*reg, ty.clone());
            if param_kind.is_owned_value() && matches!(ty, DtalType::Array { .. }) {
                let object_id = fresh_object_id(&mut entry_state);
                entry_state.owned_registers.insert(*reg);
                entry_state.owned_object_ids.insert(*reg, object_id);
                entry_state.shared_borrow_object_ids.remove(reg);
                entry_state.mutable_borrow_object_ids.remove(reg);
            } else if param_kind.is_shared_borrow() && matches!(ty, DtalType::Array { .. }) {
                let object_id = fresh_object_id(&mut entry_state);
                entry_state.shared_borrow_object_ids.insert(*reg, object_id);
                entry_state.owned_registers.remove(reg);
                entry_state.owned_object_ids.remove(reg);
                entry_state.mutable_borrow_object_ids.remove(reg);
            } else if param_kind.is_mutable_borrow() && matches!(ty, DtalType::Array { .. }) {
                let object_id = fresh_object_id(&mut entry_state);
                entry_state
                    .mutable_borrow_object_ids
                    .insert(*reg, object_id);
                entry_state.owned_registers.remove(reg);
                entry_state.owned_object_ids.remove(reg);
                entry_state.shared_borrow_object_ids.remove(reg);
            }
        }

        if let Some(precond) = &func.precondition {
            entry_state.constraints.push(precond.clone());
        }

        entry_states.insert(entry_block.label.clone(), entry_state);
    }

    let mut changed = true;
    let mut iterations = 0;
    const MAX_ITERATIONS: usize = 100;

    let entry_block_label = func.blocks.first().map(|b| b.label.clone());

    while changed && iterations < MAX_ITERATIONS {
        changed = false;
        iterations += 1;

        for (block_idx, block) in func.blocks.iter().enumerate() {
            let entry_state = if Some(&block.label) == entry_block_label.as_ref() {
                entry_states
                    .get(&block.label)
                    .cloned()
                    .unwrap_or_else(|| block.entry_state.clone())
            } else {
                let preds = predecessors.get(&block.label).cloned().unwrap_or_default();
                if preds.is_empty() {
                    block.entry_state.clone()
                } else {
                    join_states(&preds, &block.label, &exit_states, &edge_states)?
                }
            };

            let exit_state = compute_exit_state(block, &entry_state)?;

            compute_edge_states(func, block_idx, &exit_state, &mut edge_states);

            let old_exit = exit_states.get(&block.label);
            if old_exit
                .map(|s| !states_equal(s, &exit_state))
                .unwrap_or(true)
            {
                changed = true;
                exit_states.insert(block.label.clone(), exit_state);
            }

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
        edge_states,
        predecessors,
    })
}

/// Compute predecessor labels for each block.
fn compute_predecessors(func: &DtalFunction) -> HashMap<String, Vec<String>> {
    let mut predecessors: HashMap<String, Vec<String>> = HashMap::new();

    for block in &func.blocks {
        predecessors.insert(block.label.clone(), Vec::new());
    }

    for (i, block) in func.blocks.iter().enumerate() {
        let successors = get_block_successors(func, i);
        for succ in successors {
            if let Some(preds) = predecessors.get_mut(&succ) {
                preds.push(block.label.clone());
            }
        }
    }

    predecessors
}

/// Get successor labels for a block.
///
/// A conditional branch without `Jmp` or `Ret` falls through to the next block.
fn get_block_successors(func: &DtalFunction, block_index: usize) -> Vec<String> {
    let block = &func.blocks[block_index];
    let mut successors = Vec::new();
    let mut has_jmp = false;
    let mut has_ret = false;
    let mut has_branch = false;

    for instr in &block.instructions {
        match instr {
            DtalInstr::Jmp { target } => {
                successors.push(target.clone());
                has_jmp = true;
            }
            DtalInstr::Branch { target, .. } => {
                successors.push(target.clone());
                has_branch = true;
            }
            DtalInstr::Ret => {
                has_ret = true;
            }
            _ => {}
        }
    }

    if has_branch
        && !has_jmp
        && !has_ret
        && let Some(next_block) = func.blocks.get(block_index + 1)
    {
        successors.push(next_block.label.clone());
    }

    successors
}

/// Compute branch-refined exit states for CFG edges.
fn compute_edge_states(
    func: &DtalFunction,
    block_index: usize,
    exit_state: &TypeState,
    edge_states: &mut HashMap<(String, String), TypeState>,
) {
    let block = &func.blocks[block_index];
    let mut has_jmp = false;
    let mut has_ret = false;

    for instr in &block.instructions {
        match instr {
            DtalInstr::Branch { cond, target } => {
                if let Some(pos_constraint) = constraint_from_cmp_op(*cond, &exit_state.last_cmp) {
                    let mut taken_state = exit_state.clone();
                    taken_state.constraints.push(pos_constraint);
                    edge_states.insert((block.label.clone(), target.clone()), taken_state);
                }

                // Avoid overwriting the taken edge when it targets the layout successor.
                if !has_jmp
                    && !has_ret
                    && let Some(next_block) = func.blocks.get(block_index + 1)
                    && next_block.label != *target
                {
                    let neg_cond = negate_cmp_op(*cond);
                    if let Some(neg_constraint) =
                        constraint_from_cmp_op(neg_cond, &exit_state.last_cmp)
                    {
                        let mut fallthrough_state = exit_state.clone();
                        fallthrough_state.constraints.push(neg_constraint);
                        edge_states.insert(
                            (block.label.clone(), next_block.label.clone()),
                            fallthrough_state,
                        );
                    }
                }
            }
            DtalInstr::Jmp { target } => {
                // A `jmp` after a branch is the generated false edge.
                if !has_jmp && !has_ret {
                    let had_branch = block
                        .instructions
                        .iter()
                        .any(|i| matches!(i, DtalInstr::Branch { .. }));
                    if had_branch {
                        for prev_instr in &block.instructions {
                            if let DtalInstr::Branch { cond, .. } = prev_instr {
                                let neg_cond = negate_cmp_op(*cond);
                                if let Some(neg_constraint) =
                                    constraint_from_cmp_op(neg_cond, &exit_state.last_cmp)
                                {
                                    let mut jmp_state = exit_state.clone();
                                    jmp_state.constraints.push(neg_constraint);
                                    edge_states
                                        .insert((block.label.clone(), target.clone()), jmp_state);
                                }
                                break;
                            }
                        }
                    }
                }
                has_jmp = true;
            }
            DtalInstr::Ret => {
                has_ret = true;
            }
            _ => {}
        }
    }
}

/// Join predecessor states, preferring branch-refined edge states.
fn join_states(
    pred_labels: &[String],
    target_label: &str,
    exit_states: &HashMap<String, TypeState>,
    edge_states: &HashMap<(String, String), TypeState>,
) -> Result<TypeState, VerifyError> {
    let mut result = TypeState::new();

    let get_pred_state = |pred_label: &String| -> Option<&TypeState> {
        let edge_key = (pred_label.clone(), target_label.to_string());
        edge_states
            .get(&edge_key)
            .or_else(|| exit_states.get(pred_label))
    };
    let pred_states: Vec<&TypeState> = pred_labels.iter().filter_map(get_pred_state).collect();
    if pred_states.is_empty() {
        return Ok(result);
    }

    verify_borrow_join_compatibility(&pred_states, target_label)?;

    let mut all_regs: HashSet<Reg> = HashSet::new();
    for state in &pred_states {
        for reg in state.register_types.keys() {
            all_regs.insert(*reg);
        }
    }

    for reg in all_regs {
        let mut types: Vec<DtalType> = Vec::new();

        for state in &pred_states {
            if let Some(ty) = state.register_types.get(&reg) {
                types.push(ty.clone());
            }
        }

        if !types.is_empty() {
            let joined_type = join_types(&types);
            result.register_types.insert(reg, joined_type);
        }
    }

    // Keep only constraints entailed by every predecessor.
    let mut kept: HashSet<usize> = HashSet::new();

    let mut all_constraints: Vec<crate::backend::dtal::constraints::Constraint> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for state in &pred_states {
        for c in &state.constraints {
            let key = format!("{:?}", c);
            if seen.insert(key) {
                all_constraints.push(c.clone());
            }
        }
    }

    for (idx, constraint) in all_constraints.iter().enumerate() {
        let provable_from_all = pred_states.iter().all(|s| {
            s.constraints.contains(constraint)
                || crate::verifier::checker::is_constraint_provable(constraint, &s.constraints)
        });
        if provable_from_all {
            kept.insert(idx);
        }
    }

    for (idx, constraint) in all_constraints.iter().enumerate() {
        if kept.contains(&idx) {
            result.constraints.push(constraint.clone());
        }
    }

    // Keep array names fresh after joins.
    for state in &pred_states {
        for (reg, version) in &state.array_versions {
            let current = result.array_versions.get(reg).copied().unwrap_or(0);
            if *version > current {
                result.array_versions.insert(*reg, *version);
            }
        }
    }

    // Frontend-verified assertions can be unioned across predecessors.
    let mut seen_assertions: std::collections::HashSet<String> = std::collections::HashSet::new();
    for state in &pred_states {
        for assertion in &state.proven_assertions {
            let key = format!("{:?}", assertion);
            if seen_assertions.insert(key) {
                result.proven_assertions.push(assertion.clone());
            }
        }
    }

    for reg in result.register_types.keys() {
        if pred_states
            .iter()
            .all(|state| state.owned_registers.contains(reg))
        {
            result.owned_registers.insert(*reg);
        }
    }

    for reg in &result.owned_registers {
        let first_object_id = pred_states
            .iter()
            .filter_map(|state| state.owned_object_ids.get(reg).copied())
            .next();
        if let Some(object_id) = first_object_id
            && pred_states
                .iter()
                .all(|state| state.owned_object_ids.get(reg).copied() == Some(object_id))
        {
            result.owned_object_ids.insert(*reg, object_id);
        }
    }

    for reg in result.register_types.keys() {
        let first_object_id = pred_states
            .iter()
            .filter_map(|state| state.shared_borrow_object_ids.get(reg).copied())
            .next();
        if let Some(object_id) = first_object_id
            && pred_states
                .iter()
                .all(|state| state.shared_borrow_object_ids.get(reg).copied() == Some(object_id))
        {
            result.shared_borrow_object_ids.insert(*reg, object_id);
        }
    }

    for reg in result.register_types.keys() {
        let first_object_id = pred_states
            .iter()
            .filter_map(|state| state.mutable_borrow_object_ids.get(reg).copied())
            .next();
        if let Some(object_id) = first_object_id
            && pred_states
                .iter()
                .all(|state| state.mutable_borrow_object_ids.get(reg).copied() == Some(object_id))
        {
            result.mutable_borrow_object_ids.insert(*reg, object_id);
        }
    }

    for reg in result.register_types.keys() {
        if pred_states
            .iter()
            .any(|state| state.consumed_registers.contains(reg))
        {
            result.consumed_registers.insert(*reg);
        }
    }

    let stack_len = pred_states.first().map(|state| state.owned_stack.len());
    let same_stack_shape = pred_states
        .iter()
        .all(|state| Some(state.owned_stack.len()) == stack_len);
    if same_stack_shape {
        result.owned_stack = (0..stack_len.unwrap_or(0))
            .map(|idx| {
                pred_states
                    .iter()
                    .all(|state| state.owned_stack.get(idx).copied().unwrap_or(false))
            })
            .collect();
        result.owned_stack_object_ids = (0..stack_len.unwrap_or(0))
            .map(|idx| {
                let first_object_id = pred_states
                    .iter()
                    .filter_map(|state| state.owned_stack_object_ids.get(idx).cloned().flatten())
                    .next();
                if let Some(object_id) = first_object_id
                    && pred_states.iter().all(|state| {
                        state.owned_stack_object_ids.get(idx).cloned().flatten() == Some(object_id)
                    })
                {
                    Some(object_id)
                } else {
                    None
                }
            })
            .collect();
        result.shared_borrow_stack_object_ids = (0..stack_len.unwrap_or(0))
            .map(|idx| {
                let first_object_id = pred_states
                    .iter()
                    .filter_map(|state| {
                        state
                            .shared_borrow_stack_object_ids
                            .get(idx)
                            .cloned()
                            .flatten()
                    })
                    .next();
                if let Some(object_id) = first_object_id
                    && pred_states.iter().all(|state| {
                        state
                            .shared_borrow_stack_object_ids
                            .get(idx)
                            .cloned()
                            .flatten()
                            == Some(object_id)
                    })
                {
                    Some(object_id)
                } else {
                    None
                }
            })
            .collect();
        result.mutable_borrow_stack_object_ids = (0..stack_len.unwrap_or(0))
            .map(|idx| {
                let first_object_id = pred_states
                    .iter()
                    .filter_map(|state| {
                        state
                            .mutable_borrow_stack_object_ids
                            .get(idx)
                            .cloned()
                            .flatten()
                    })
                    .next();
                if let Some(object_id) = first_object_id
                    && pred_states.iter().all(|state| {
                        state
                            .mutable_borrow_stack_object_ids
                            .get(idx)
                            .cloned()
                            .flatten()
                            == Some(object_id)
                    })
                {
                    Some(object_id)
                } else {
                    None
                }
            })
            .collect();
    }

    for offset in result.spill_types.keys() {
        if pred_states
            .iter()
            .all(|state| state.owned_spills.contains(offset))
        {
            result.owned_spills.insert(*offset);
            let first_object_id = pred_states
                .iter()
                .filter_map(|state| state.owned_spill_object_ids.get(offset).copied())
                .next();
            if let Some(object_id) = first_object_id
                && pred_states.iter().all(|state| {
                    state.owned_spill_object_ids.get(offset).copied() == Some(object_id)
                })
            {
                result.owned_spill_object_ids.insert(*offset, object_id);
            }
        }
        let first_borrow_id = pred_states
            .iter()
            .filter_map(|state| state.shared_borrow_spill_object_ids.get(offset).copied())
            .next();
        if let Some(object_id) = first_borrow_id
            && pred_states.iter().all(|state| {
                state.shared_borrow_spill_object_ids.get(offset).copied() == Some(object_id)
            })
        {
            result
                .shared_borrow_spill_object_ids
                .insert(*offset, object_id);
        }
        let first_mut_borrow_id = pred_states
            .iter()
            .filter_map(|state| state.mutable_borrow_spill_object_ids.get(offset).copied())
            .next();
        if let Some(object_id) = first_mut_borrow_id
            && pred_states.iter().all(|state| {
                state.mutable_borrow_spill_object_ids.get(offset).copied() == Some(object_id)
            })
        {
            result
                .mutable_borrow_spill_object_ids
                .insert(*offset, object_id);
        }
    }

    result.next_object_id = pred_states
        .iter()
        .map(|state| state.next_object_id)
        .max()
        .unwrap_or(0);

    checker::verify_unique_owned_objects(&result, target_label, "join")?;

    Ok(result)
}

fn verify_borrow_join_compatibility(
    pred_states: &[&TypeState],
    target_label: &str,
) -> Result<(), VerifyError> {
    if pred_states.len() <= 1 {
        return Ok(());
    }

    let baseline_shared_objects = shared_borrow_objects(pred_states[0]);
    let baseline_mutable_objects = mutable_borrow_objects(pred_states[0]);
    for state in pred_states.iter().skip(1) {
        if baseline_shared_objects != shared_borrow_objects(state) {
            return Err(VerifyError::OwnershipViolation {
                block: target_label.to_string(),
                instr_desc: "join".to_string(),
                msg: "predecessors disagree on shared-borrow object state".to_string(),
            });
        }
        if baseline_mutable_objects != mutable_borrow_objects(state) {
            return Err(VerifyError::OwnershipViolation {
                block: target_label.to_string(),
                instr_desc: "join".to_string(),
                msg: "predecessors disagree on mutable-borrow object state".to_string(),
            });
        }
    }

    for object_id in baseline_shared_objects {
        if !has_common_shared_borrow_location(pred_states, object_id) {
            return Err(VerifyError::OwnershipViolation {
                block: target_label.to_string(),
                instr_desc: "join".to_string(),
                msg: format!("shared-borrow object {object_id} has no common join location"),
            });
        }
    }

    for object_id in baseline_mutable_objects {
        if !has_common_mutable_borrow_location(pred_states, object_id) {
            return Err(VerifyError::OwnershipViolation {
                block: target_label.to_string(),
                instr_desc: "join".to_string(),
                msg: format!("mutable-borrow object {object_id} has no common join location"),
            });
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum BorrowLocation {
    Reg(Reg),
    Stack(usize),
    Spill(i32),
}

fn shared_borrow_objects(state: &TypeState) -> HashSet<u32> {
    borrow_objects(
        state.shared_borrow_object_ids.values().copied(),
        state
            .shared_borrow_stack_object_ids
            .iter()
            .filter_map(|object_id| *object_id),
        state.shared_borrow_spill_object_ids.values().copied(),
    )
}

fn mutable_borrow_objects(state: &TypeState) -> HashSet<u32> {
    borrow_objects(
        state.mutable_borrow_object_ids.values().copied(),
        state
            .mutable_borrow_stack_object_ids
            .iter()
            .filter_map(|object_id| *object_id),
        state.mutable_borrow_spill_object_ids.values().copied(),
    )
}

fn borrow_objects(
    register_objects: impl Iterator<Item = u32>,
    stack_objects: impl Iterator<Item = u32>,
    spill_objects: impl Iterator<Item = u32>,
) -> HashSet<u32> {
    register_objects
        .chain(stack_objects)
        .chain(spill_objects)
        .collect()
}

fn has_common_shared_borrow_location(pred_states: &[&TypeState], object_id: u32) -> bool {
    has_common_borrow_location(pred_states, object_id, shared_borrow_locations)
}

fn has_common_mutable_borrow_location(pred_states: &[&TypeState], object_id: u32) -> bool {
    has_common_borrow_location(pred_states, object_id, mutable_borrow_locations)
}

fn has_common_borrow_location(
    pred_states: &[&TypeState],
    object_id: u32,
    locations_for_state: fn(&TypeState, u32) -> HashSet<BorrowLocation>,
) -> bool {
    let Some((first_state, rest)) = pred_states.split_first() else {
        return false;
    };

    let mut common_locations = locations_for_state(first_state, object_id);
    for state in rest {
        let locations = locations_for_state(state, object_id);
        common_locations.retain(|location| locations.contains(location));
    }

    !common_locations.is_empty()
}

fn shared_borrow_locations(state: &TypeState, object_id: u32) -> HashSet<BorrowLocation> {
    borrow_locations(
        state.shared_borrow_object_ids.iter(),
        state.shared_borrow_stack_object_ids.iter(),
        state.shared_borrow_spill_object_ids.iter(),
        object_id,
    )
}

fn mutable_borrow_locations(state: &TypeState, object_id: u32) -> HashSet<BorrowLocation> {
    borrow_locations(
        state.mutable_borrow_object_ids.iter(),
        state.mutable_borrow_stack_object_ids.iter(),
        state.mutable_borrow_spill_object_ids.iter(),
        object_id,
    )
}

fn borrow_locations<'a>(
    register_objects: impl Iterator<Item = (&'a Reg, &'a u32)>,
    stack_objects: impl Iterator<Item = &'a Option<u32>>,
    spill_objects: impl Iterator<Item = (&'a i32, &'a u32)>,
    object_id: u32,
) -> HashSet<BorrowLocation> {
    let mut locations = HashSet::new();

    for (reg, reg_object_id) in register_objects {
        if *reg_object_id == object_id {
            locations.insert(BorrowLocation::Reg(*reg));
        }
    }

    for (idx, stack_object_id) in stack_objects.enumerate() {
        if *stack_object_id == Some(object_id) {
            locations.insert(BorrowLocation::Stack(idx));
        }
    }

    for (offset, spill_object_id) in spill_objects {
        if *spill_object_id == object_id {
            locations.insert(BorrowLocation::Spill(*offset));
        }
    }

    locations
}

/// Join multiple types into their least upper bound.
fn join_types(types: &[DtalType]) -> DtalType {
    if types.is_empty() {
        return DtalType::Int;
    }

    if types.len() == 1 {
        return types[0].clone();
    }

    let first = &types[0];
    if types.iter().all(|t| t == first) {
        return first.clone();
    }

    // Preserve existentials when every incoming type can inhabit them.
    if let Some(existential) = types
        .iter()
        .find(|t| matches!(t, DtalType::ExistentialInt { .. }))
    {
        let all_compatible = types.iter().all(|t| {
            matches!(
                t,
                DtalType::ExistentialInt { .. } | DtalType::SingletonInt(_) | DtalType::Int
            )
        });
        if all_compatible {
            let has_plain_int = types.iter().any(|t| matches!(t, DtalType::Int));
            if has_plain_int {
                return DtalType::Int;
            }
            return existential.clone();
        }
    }

    let all_singleton_ints = types
        .iter()
        .all(|t| matches!(t, DtalType::SingletonInt(_) | DtalType::Int));
    if all_singleton_ints {
        return DtalType::Int;
    }

    let all_machine_i64_like = types
        .iter()
        .all(|t| matches!(t, DtalType::I64 | DtalType::SingletonInt(_)));
    if all_machine_i64_like {
        return DtalType::I64;
    }

    let all_machine_u64_like = types
        .iter()
        .all(|t| matches!(t, DtalType::U64 | DtalType::SingletonInt(_)));
    if all_machine_u64_like {
        return DtalType::U64;
    }

    let all_numeric = types.iter().all(|t| {
        matches!(
            t,
            DtalType::Int
                | DtalType::I64
                | DtalType::U64
                | DtalType::SingletonInt(_)
                | DtalType::RefinedInt { .. }
                | DtalType::ExistentialInt { .. }
        )
    });
    if all_numeric {
        return DtalType::Int;
    }

    first.clone()
}

/// Check structural equality of types.
fn types_structurally_equal(a: &DtalType, b: &DtalType) -> bool {
    a == b
}

/// Compute a non-verifying exit state from instruction definitions.
fn compute_exit_state(
    block: &DtalBlock,
    entry_state: &TypeState,
) -> Result<TypeState, VerifyError> {
    let mut state = entry_state.clone();

    for instr in &block.instructions {
        update_state_for_instruction(instr, &mut state);
    }

    Ok(state)
}

/// Apply verifier-like type derivation without operand validation.
fn update_state_for_instruction(instr: &DtalInstr, state: &mut TypeState) {
    use crate::backend::dtal::instr::BinaryOp;

    match instr {
        DtalInstr::MovImm { dst, imm, .. } => {
            state
                .register_types
                .insert(*dst, DtalType::SingletonInt(IndexExpr::Const(*imm)));
            let reg_expr = crate::verifier::checker::reg_to_index_expr(dst);
            state
                .constraints
                .push(Constraint::Eq(reg_expr, IndexExpr::Const(*imm)));
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::MovReg { dst, src, .. } => {
            let ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, ty);
            preserve_plain_mov_alias_ownership(*src, *dst, state);
            preserve_plain_mov_shared_borrow(*src, *dst, state);
            preserve_plain_mov_mutable_borrow(*src, *dst, state);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::AliasBorrow { dst, src, .. } => {
            let ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, ty);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            assign_shared_borrow_from(*src, *dst, state);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::BorrowMut { dst, src, .. } => {
            let ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, ty);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            assign_mutable_borrow_from(*src, *dst, state);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::BorrowEnd { src, .. } => {
            state.shared_borrow_object_ids.remove(src);
            state.mutable_borrow_object_ids.remove(src);
        }
        DtalInstr::MoveOwned { dst, src, .. } => {
            let ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, ty);
            transfer_owned(*src, *dst, state);
            state.consumed_registers.insert(*src);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } => {
            let lhs_ty = state
                .register_types
                .get(lhs)
                .cloned()
                .unwrap_or(DtalType::Int);
            let rhs_ty = state
                .register_types
                .get(rhs)
                .cloned()
                .unwrap_or(DtalType::Int);

            let derived_ty = if *op == BinaryOp::Add {
                if checker::is_numeric_type(&rhs_ty)
                    && let Some(derived_ty) = checker::pointer_arithmetic_result_type(&lhs_ty, ty)
                {
                    derived_ty
                } else if checker::is_numeric_type(&lhs_ty)
                    && let Some(derived_ty) = checker::pointer_arithmetic_result_type(&rhs_ty, ty)
                {
                    derived_ty
                } else {
                    match op {
                        BinaryOp::And | BinaryOp::Or => DtalType::Bool,
                        BinaryOp::BitAnd
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor
                        | BinaryOp::Shl
                        | BinaryOp::Shr => DtalType::Int,
                        BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Div
                        | BinaryOp::Mod => {
                            let lhs_idx = extract_index(&lhs_ty, lhs);
                            let rhs_idx = extract_index(&rhs_ty, rhs);
                            let result_idx = match op {
                                BinaryOp::Add => {
                                    IndexExpr::Add(Box::new(lhs_idx), Box::new(rhs_idx))
                                }
                                BinaryOp::Sub => {
                                    IndexExpr::Sub(Box::new(lhs_idx), Box::new(rhs_idx))
                                }
                                BinaryOp::Mul => {
                                    IndexExpr::Mul(Box::new(lhs_idx), Box::new(rhs_idx))
                                }
                                BinaryOp::Div => {
                                    IndexExpr::Div(Box::new(lhs_idx), Box::new(rhs_idx))
                                }
                                BinaryOp::Mod => {
                                    IndexExpr::Mod(Box::new(lhs_idx), Box::new(rhs_idx))
                                }
                                _ => unreachable!(),
                            };
                            DtalType::SingletonInt(result_idx)
                        }
                    }
                }
            } else {
                match op {
                    BinaryOp::And | BinaryOp::Or => DtalType::Bool,
                    BinaryOp::BitAnd
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::Shl
                    | BinaryOp::Shr => DtalType::Int,
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let lhs_idx = extract_index(&lhs_ty, lhs);
                        let rhs_idx = extract_index(&rhs_ty, rhs);
                        let result_idx = match op {
                            BinaryOp::Add => IndexExpr::Add(Box::new(lhs_idx), Box::new(rhs_idx)),
                            BinaryOp::Sub => IndexExpr::Sub(Box::new(lhs_idx), Box::new(rhs_idx)),
                            BinaryOp::Mul => IndexExpr::Mul(Box::new(lhs_idx), Box::new(rhs_idx)),
                            BinaryOp::Div => IndexExpr::Div(Box::new(lhs_idx), Box::new(rhs_idx)),
                            BinaryOp::Mod => IndexExpr::Mod(Box::new(lhs_idx), Box::new(rhs_idx)),
                            _ => unreachable!(),
                        };
                        DtalType::SingletonInt(result_idx)
                    }
                }
            };
            state.register_types.insert(*dst, derived_ty);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::AddImm { dst, src, imm, ty } => {
            let src_ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            if let Some(derived_ty) = checker::pointer_arithmetic_result_type(&src_ty, ty) {
                state.register_types.insert(*dst, derived_ty);
            } else {
                let src_idx = extract_index(&src_ty, src);
                let result_idx =
                    IndexExpr::Add(Box::new(src_idx), Box::new(IndexExpr::Const(*imm)));
                state
                    .register_types
                    .insert(*dst, DtalType::SingletonInt(result_idx));
            }
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Load { dst, base, ty, .. } => {
            // Prefer element types derived from typed array bases.
            let derived_ty = if let Some(base_ty) = state.register_types.get(base) {
                match base_ty {
                    DtalType::Array { element_type, .. } => element_type.as_ref().clone(),
                    DtalType::Ref(inner) | DtalType::RefMut(inner) => match inner.as_ref() {
                        DtalType::Array { element_type, .. } => element_type.as_ref().clone(),
                        _ => ty.clone(),
                    },
                    _ => ty.clone(),
                }
            } else {
                ty.clone()
            };
            state.register_types.insert(*dst, derived_ty.clone());
            if matches!(derived_ty, DtalType::Array { .. }) {
                let object_id = fresh_object_id(state);
                state.owned_registers.insert(*dst);
                state.owned_object_ids.insert(*dst, object_id);
            } else {
                state.owned_registers.remove(dst);
                state.owned_object_ids.remove(dst);
            }
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::LoadOp { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::SetCC { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Bool);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Not { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Bool);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Neg { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::ShlImm { dst, ty, .. } | DtalInstr::ShrImm { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::TypeAnnotation { reg, ty } => {
            state.register_types.insert(*reg, ty.clone());
        }
        DtalInstr::Pop { dst, .. } => {
            let popped_ty = state.stack.pop().unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, popped_ty);
            if state.owned_stack.pop().unwrap_or(false) {
                let object_id = state.owned_stack_object_ids.pop().unwrap_or(None);
                state.owned_registers.insert(*dst);
                if let Some(object_id) = object_id {
                    state.owned_object_ids.insert(*dst, object_id);
                }
            } else {
                state.owned_registers.remove(dst);
                state.owned_object_ids.remove(dst);
                let _ = state.owned_stack_object_ids.pop();
            }
            if let Some(object_id) = state.shared_borrow_stack_object_ids.pop().unwrap_or(None) {
                state.shared_borrow_object_ids.insert(*dst, object_id);
            } else {
                state.shared_borrow_object_ids.remove(dst);
            }
            if let Some(object_id) = state.mutable_borrow_stack_object_ids.pop().unwrap_or(None) {
                state.mutable_borrow_object_ids.insert(*dst, object_id);
            } else {
                state.mutable_borrow_object_ids.remove(dst);
            }
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Alloca { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
            if matches!(ty, DtalType::Array { .. }) {
                let object_id = fresh_object_id(state);
                state.owned_registers.insert(*dst);
                state.owned_object_ids.insert(*dst, object_id);
            } else {
                state.owned_registers.remove(dst);
                state.owned_object_ids.remove(dst);
            }
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Call {
            arg_kinds,
            return_ty,
            ownership,
            ..
        } => {
            use crate::backend::dtal::regs::PhysicalReg;
            for (index, arg_kind) in arg_kinds.iter().enumerate() {
                if let Some(param_reg) = crate::backend::dtal::regs::PhysicalReg::param_regs()
                    .get(index)
                    .copied()
                {
                    let param_reg = Reg::Physical(param_reg);
                    if arg_kind.is_owned_value() {
                        state.owned_registers.remove(&param_reg);
                        state.owned_object_ids.remove(&param_reg);
                        state.shared_borrow_object_ids.remove(&param_reg);
                        state.mutable_borrow_object_ids.remove(&param_reg);
                        state.consumed_registers.insert(param_reg);
                    } else {
                        state.owned_registers.remove(&param_reg);
                        state.owned_object_ids.remove(&param_reg);
                        state.shared_borrow_object_ids.remove(&param_reg);
                        state.mutable_borrow_object_ids.remove(&param_reg);
                    }
                }
            }
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R0), return_ty.clone());
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::LR), return_ty.clone());
            if ownership.produces_owned_output() {
                let object_id = fresh_object_id(state);
                state.owned_registers.insert(Reg::Physical(PhysicalReg::R0));
                state.owned_registers.insert(Reg::Physical(PhysicalReg::LR));
                state
                    .owned_object_ids
                    .insert(Reg::Physical(PhysicalReg::R0), object_id);
                state
                    .owned_object_ids
                    .insert(Reg::Physical(PhysicalReg::LR), object_id);
                state
                    .shared_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .shared_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::LR));
                state
                    .mutable_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .mutable_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::LR));
            } else {
                state
                    .owned_registers
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .owned_registers
                    .remove(&Reg::Physical(PhysicalReg::LR));
                state
                    .owned_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .owned_object_ids
                    .remove(&Reg::Physical(PhysicalReg::LR));
                state
                    .shared_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .shared_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::LR));
                state
                    .mutable_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R0));
                state
                    .mutable_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::LR));
            }
            state
                .consumed_registers
                .remove(&Reg::Physical(PhysicalReg::R0));
            state
                .consumed_registers
                .remove(&Reg::Physical(PhysicalReg::LR));
        }
        DtalInstr::Cmp { lhs, rhs } => {
            state.last_cmp = Some(CmpOperands::RegReg(*lhs, *rhs));
        }
        DtalInstr::CmpImm { lhs, imm } => {
            state.last_cmp = Some(CmpOperands::RegImm(*lhs, *imm));
        }
        DtalInstr::Push { src, .. } => {
            let src_ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.stack.push(src_ty);
            state.owned_stack.push(state.owned_registers.contains(src));
            state
                .owned_stack_object_ids
                .push(state.owned_object_ids.get(src).copied());
            state
                .shared_borrow_stack_object_ids
                .push(state.shared_borrow_object_ids.get(src).copied());
            state
                .mutable_borrow_stack_object_ids
                .push(state.mutable_borrow_object_ids.get(src).copied());
        }
        DtalInstr::Store { .. } => {}
        DtalInstr::ConstraintAssert { constraint, .. } => {
            // Proven assertions participate in downstream joins.
            state.constraints.push(constraint.clone());
            state.proven_assertions.push(constraint.clone());
        }
        DtalInstr::Jmp { .. } | DtalInstr::Branch { .. } | DtalInstr::Ret => {}

        DtalInstr::Cqo => {
            use crate::backend::dtal::regs::PhysicalReg;
            let rax = Reg::Physical(PhysicalReg::LR);
            let rdx = Reg::Physical(PhysicalReg::R2);
            let rax_ty = state
                .register_types
                .get(&rax)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(rdx, rax_ty);
            if let Some(object_id) = state.owned_object_ids.get(&rax).copied() {
                state.owned_registers.insert(rdx);
                state.owned_object_ids.insert(rdx, object_id);
            } else {
                state.owned_registers.remove(&rdx);
                state.owned_object_ids.remove(&rdx);
                state.shared_borrow_object_ids.remove(&rdx);
                state.mutable_borrow_object_ids.remove(&rdx);
            }
            state.consumed_registers.remove(&rdx);
        }
        DtalInstr::Idiv { src: _ } => {
            use crate::backend::dtal::regs::PhysicalReg;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::LR), DtalType::Int);
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R2), DtalType::Int);
            state
                .owned_registers
                .remove(&Reg::Physical(PhysicalReg::LR));
            state
                .owned_registers
                .remove(&Reg::Physical(PhysicalReg::R2));
            state
                .owned_object_ids
                .remove(&Reg::Physical(PhysicalReg::LR));
            state
                .owned_object_ids
                .remove(&Reg::Physical(PhysicalReg::R2));
            state
                .shared_borrow_object_ids
                .remove(&Reg::Physical(PhysicalReg::LR));
            state
                .shared_borrow_object_ids
                .remove(&Reg::Physical(PhysicalReg::R2));
            state
                .mutable_borrow_object_ids
                .remove(&Reg::Physical(PhysicalReg::LR));
            state
                .mutable_borrow_object_ids
                .remove(&Reg::Physical(PhysicalReg::R2));
            state
                .consumed_registers
                .remove(&Reg::Physical(PhysicalReg::LR));
            state
                .consumed_registers
                .remove(&Reg::Physical(PhysicalReg::R2));
        }
        DtalInstr::PortIn { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Int);
            state.owned_registers.remove(dst);
            state.owned_object_ids.remove(dst);
            state.shared_borrow_object_ids.remove(dst);
            state.mutable_borrow_object_ids.remove(dst);
            state.consumed_registers.remove(dst);
        }
        DtalInstr::PortOut { .. } => {}
        DtalInstr::SpillStore { src, offset, .. } => {
            if let Some(object_id) = state.owned_object_ids.get(src).copied() {
                state.owned_spills.insert(*offset);
                state.owned_spill_object_ids.insert(*offset, object_id);
                clear_owned_alias_group(*src, Some(object_id), state);
                consume_owned_alias_group(*src, Some(object_id), state);
            } else {
                state.owned_spills.remove(offset);
                state.owned_spill_object_ids.remove(offset);
            }
            if let Some(object_id) = state.shared_borrow_object_ids.get(src).copied() {
                state
                    .shared_borrow_spill_object_ids
                    .insert(*offset, object_id);
            } else {
                state.shared_borrow_spill_object_ids.remove(offset);
            }
            if let Some(object_id) = state.mutable_borrow_object_ids.get(src).copied() {
                state
                    .mutable_borrow_spill_object_ids
                    .insert(*offset, object_id);
            } else {
                state.mutable_borrow_spill_object_ids.remove(offset);
            }
        }
        DtalInstr::SpillLoad { dst, ty, offset } => {
            let derived_ty = if let Some(stored_ty) = state.spill_types.get(offset).cloned() {
                stored_ty
            } else {
                ty.clone()
            };
            state.register_types.insert(*dst, derived_ty);
            if let Some(object_id) = state.owned_spill_object_ids.get(offset).copied() {
                state.owned_registers.insert(*dst);
                state.owned_object_ids.insert(*dst, object_id);
            } else {
                state.owned_registers.remove(dst);
                state.owned_object_ids.remove(dst);
            }
            if let Some(object_id) = state.shared_borrow_spill_object_ids.get(offset).copied() {
                state.shared_borrow_object_ids.insert(*dst, object_id);
            } else {
                state.shared_borrow_object_ids.remove(dst);
            }
            if let Some(object_id) = state.mutable_borrow_spill_object_ids.get(offset).copied() {
                state.mutable_borrow_object_ids.insert(*dst, object_id);
            } else {
                state.mutable_borrow_object_ids.remove(dst);
            }
            state.consumed_registers.remove(dst);
        }
        DtalInstr::Prologue { .. } => {
            // Match verifier prologue handling for successor blocks.
            use crate::backend::dtal::regs::PhysicalReg;
            for preg in &[
                PhysicalReg::LR,
                PhysicalReg::R0,
                PhysicalReg::R1,
                PhysicalReg::R2,
                PhysicalReg::R3,
                PhysicalReg::R4,
                PhysicalReg::R5,
                PhysicalReg::R6,
                PhysicalReg::R7,
                PhysicalReg::R8,
                PhysicalReg::R9,
                PhysicalReg::R10,
                PhysicalReg::R11,
                PhysicalReg::R12,
            ] {
                state
                    .register_types
                    .entry(Reg::Physical(*preg))
                    .or_insert(DtalType::Int);
                state
                    .mutable_borrow_object_ids
                    .remove(&Reg::Physical(*preg));
                state.consumed_registers.remove(&Reg::Physical(*preg));
            }
        }
        DtalInstr::Epilogue { .. } => {}
        DtalInstr::DropOwned { src, .. } => {
            let object_id = state.owned_object_ids.get(src).copied();
            clear_owned_alias_group(*src, object_id, state);
            consume_owned_alias_group(*src, object_id, state);
        }
    }
}

fn transfer_owned(src: Reg, dst: Reg, state: &mut TypeState) {
    let object_id = state.owned_object_ids.get(&src).copied();
    clear_owned_alias_group(src, object_id, state);
    if let Some(object_id) = object_id {
        state.owned_registers.insert(dst);
        state.owned_object_ids.insert(dst, object_id);
        state.shared_borrow_object_ids.remove(&dst);
        state.mutable_borrow_object_ids.remove(&dst);
    } else {
        state.owned_registers.remove(&dst);
        state.owned_object_ids.remove(&dst);
        state.shared_borrow_object_ids.remove(&dst);
        state.mutable_borrow_object_ids.remove(&dst);
    }
}

fn abi_owned_alias_counterpart(reg: Reg) -> Option<Reg> {
    match reg {
        Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0) => {
            Some(Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR))
        }
        Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR) => {
            Some(Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0))
        }
        _ => None,
    }
}

fn clear_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    state.owned_registers.remove(&reg);
    state.owned_object_ids.remove(&reg);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            state.owned_registers.remove(&counterpart);
            state.owned_object_ids.remove(&counterpart);
        }
    }
}

fn consume_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    state.consumed_registers.insert(reg);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            state.consumed_registers.insert(counterpart);
        }
    }
}

fn preserve_plain_mov_alias_ownership(src: Reg, dst: Reg, state: &mut TypeState) {
    let is_abi_return_alias = matches!(
        (src, dst),
        (
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0)
        ) | (
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR)
        )
    );
    if is_abi_return_alias && let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        state.owned_registers.insert(dst);
        state.owned_object_ids.insert(dst, object_id);
    } else {
        state.owned_registers.remove(&dst);
        state.owned_object_ids.remove(&dst);
    }
}

fn preserve_plain_mov_shared_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        state.shared_borrow_object_ids.insert(dst, object_id);
    } else {
        state.shared_borrow_object_ids.remove(&dst);
    }
}

fn preserve_plain_mov_mutable_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if src == dst {
        if let Some(object_id) = state.mutable_borrow_object_ids.get(&src).copied() {
            state.mutable_borrow_object_ids.insert(dst, object_id);
        } else {
            state.mutable_borrow_object_ids.remove(&dst);
        }
    } else {
        state.mutable_borrow_object_ids.remove(&dst);
    }
}

fn assign_shared_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        state.shared_borrow_object_ids.insert(dst, object_id);
    } else if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        state.shared_borrow_object_ids.insert(dst, object_id);
    } else {
        state.shared_borrow_object_ids.remove(&dst);
    }
}

fn assign_mutable_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        state.owned_registers.remove(&dst);
        state.owned_object_ids.remove(&dst);
        state.shared_borrow_object_ids.remove(&dst);
        state.mutable_borrow_object_ids.insert(dst, object_id);
    } else {
        state.mutable_borrow_object_ids.remove(&dst);
    }
}

fn fresh_object_id(state: &mut TypeState) -> u32 {
    let object_id = state.next_object_id;
    state.next_object_id += 1;
    object_id
}

/// Check whether two type states are equivalent for dataflow convergence.
fn states_equal(a: &TypeState, b: &TypeState) -> bool {
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

    a.constraints.len() == b.constraints.len()
        && a.constraints.iter().all(|c| b.constraints.contains(c))
        && a.stack.len() == b.stack.len()
        && a.stack.iter().zip(&b.stack).all(|(a, b)| a == b)
        && a.owned_registers == b.owned_registers
        && a.owned_object_ids == b.owned_object_ids
        && a.shared_borrow_object_ids == b.shared_borrow_object_ids
        && a.mutable_borrow_object_ids == b.mutable_borrow_object_ids
        && a.owned_stack == b.owned_stack
        && a.owned_stack_object_ids == b.owned_stack_object_ids
        && a.shared_borrow_stack_object_ids == b.shared_borrow_stack_object_ids
        && a.mutable_borrow_stack_object_ids == b.mutable_borrow_stack_object_ids
        && a.owned_spills == b.owned_spills
        && a.owned_spill_object_ids == b.owned_spill_object_ids
        && a.shared_borrow_spill_object_ids == b.shared_borrow_spill_object_ids
        && a.mutable_borrow_spill_object_ids == b.mutable_borrow_spill_object_ids
        && a.consumed_registers == b.consumed_registers
        && a.proven_assertions.len() == b.proven_assertions.len()
        && a.proven_assertions
            .iter()
            .all(|c| b.proven_assertions.contains(c))
}
