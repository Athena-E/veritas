//! Type state dataflow analysis
//!
//! This module computes type states at block entries using forward dataflow analysis.

#![allow(clippy::result_large_err)]

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{CmpOperands, DtalBlock, DtalFunction, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
use crate::verifier::checker::{self, constraint_from_cmp_op, extract_index, negate_cmp_op};
use crate::verifier::error::VerifyError;
use std::collections::HashMap;

/// Result of dataflow analysis
#[allow(dead_code)]
pub struct DataflowResult {
    /// Type state at entry of each block
    pub entry_states: HashMap<String, TypeState>,
    /// Type state at exit of each block
    pub exit_states: HashMap<String, TypeState>,
    /// Per-edge exit states (source_label, target_label) -> state
    /// Used for branch-refined constraints on specific edges
    pub edge_states: HashMap<(String, String), TypeState>,
    /// Predecessor blocks for each block
    pub predecessors: HashMap<String, Vec<String>>,
}

/// Compute type states for all blocks in a function
pub fn analyze_function(func: &DtalFunction) -> Result<DataflowResult, VerifyError> {
    // Build CFG structure
    let predecessors = compute_predecessors(func);

    // Initialize entry states
    let mut entry_states: HashMap<String, TypeState> = HashMap::new();
    let mut exit_states: HashMap<String, TypeState> = HashMap::new();
    let mut edge_states: HashMap<(String, String), TypeState> = HashMap::new();

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

        for (block_idx, block) in func.blocks.iter().enumerate() {
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
                    // Join from predecessors using per-edge states when available
                    join_states(&preds, &block.label, &exit_states, &edge_states)?
                }
            };

            // Compute exit state by processing instructions
            let exit_state = compute_exit_state(block, &entry_state)?;

            // Compute per-edge exit states for branches
            compute_edge_states(func, block_idx, &exit_state, &mut edge_states);

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
        edge_states,
        predecessors,
    })
}

/// Compute predecessor blocks for each block
fn compute_predecessors(func: &DtalFunction) -> HashMap<String, Vec<String>> {
    let mut predecessors: HashMap<String, Vec<String>> = HashMap::new();

    // Initialize all blocks with empty predecessor lists
    for block in &func.blocks {
        predecessors.insert(block.label.clone(), Vec::new());
    }

    // Find successors and build predecessor lists
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

/// Get successor block labels from a block
///
/// Detects implicit fall-through: if a block has a `Branch` but no `Jmp` or `Ret`,
/// the next block in layout order is a fall-through successor.
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

    // If block has a conditional branch but no unconditional jump or return,
    // the next block in layout order is a fall-through successor
    if has_branch
        && !has_jmp
        && !has_ret
        && let Some(next_block) = func.blocks.get(block_index + 1)
    {
        successors.push(next_block.label.clone());
    }

    successors
}

/// Compute per-edge exit states for branches.
///
/// When a block ends with a conditional Branch, the taken edge gets the positive
/// constraint and the fall-through edge gets the negated constraint.
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
                // Taken edge: add positive constraint
                if let Some(pos_constraint) = constraint_from_cmp_op(*cond, &exit_state.last_cmp) {
                    let mut taken_state = exit_state.clone();
                    taken_state.constraints.push(pos_constraint);
                    edge_states.insert((block.label.clone(), target.clone()), taken_state);
                }

                // Fall-through edge: add negated constraint.
                // Only if the next block in layout is NOT the branch target
                // (otherwise we'd overwrite the taken-edge constraint).
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
                // If this Jmp follows a Branch in the same block,
                // it's the fall-through path. Add the negated branch
                // constraint to the jump target's edge state.
                if !has_jmp && !has_ret {
                    // Check if a Branch was seen earlier in this block
                    let had_branch = block
                        .instructions
                        .iter()
                        .any(|i| matches!(i, DtalInstr::Branch { .. }));
                    if had_branch {
                        // Find the branch's condition and negate it
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

/// Join type states from multiple predecessors
///
/// Uses per-edge states when available (for branch-refined constraints),
/// otherwise falls back to the block's exit state.
fn join_states(
    pred_labels: &[String],
    target_label: &str,
    exit_states: &HashMap<String, TypeState>,
    edge_states: &HashMap<(String, String), TypeState>,
) -> Result<TypeState, VerifyError> {
    let mut result = TypeState::new();

    // Helper: get the state for a predecessor edge
    let get_pred_state = |pred_label: &String| -> Option<&TypeState> {
        // Prefer per-edge state if available
        let edge_key = (pred_label.clone(), target_label.to_string());
        edge_states
            .get(&edge_key)
            .or_else(|| exit_states.get(pred_label))
    };

    // Collect all registers that appear in any predecessor
    let mut all_regs: HashSet<Reg> = HashSet::new();
    for label in pred_labels {
        if let Some(state) = get_pred_state(label) {
            for reg in state.register_types.keys() {
                all_regs.insert(*reg);
            }
        }
    }

    // For each register, compute join of types
    for reg in all_regs {
        let mut types: Vec<DtalType> = Vec::new();

        for label in pred_labels {
            if let Some(state) = get_pred_state(label)
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

    // Join constraints: keep constraints provable from ALL predecessors.
    // First, do fast syntactic intersection (constraints in all predecessors).
    // Then, for constraints in some but not all predecessors, use Z3 to check
    // if they're provable from the other predecessors' contexts (e.g., a loop
    // invariant that's vacuously true on the entry edge).
    use std::collections::HashSet;
    let mut kept: HashSet<usize> = HashSet::new();

    // Collect all unique constraints from all predecessors
    let mut all_constraints: Vec<crate::backend::dtal::constraints::Constraint> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for label in pred_labels {
        if let Some(state) = get_pred_state(label) {
            for c in &state.constraints {
                let key = format!("{:?}", c);
                if seen.insert(key) {
                    all_constraints.push(c.clone());
                }
            }
        }
    }

    for (idx, constraint) in all_constraints.iter().enumerate() {
        let provable_from_all = pred_labels.iter().all(|label| {
            get_pred_state(label)
                .map(|s| {
                    s.constraints.contains(constraint)
                        || crate::verifier::checker::is_constraint_provable(
                            constraint,
                            &s.constraints,
                        )
                })
                .unwrap_or(false)
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

    // Join array versions - take max to ensure fresh names for future stores
    for label in pred_labels {
        if let Some(state) = get_pred_state(label) {
            for (reg, version) in &state.array_versions {
                let current = result.array_versions.get(reg).copied().unwrap_or(0);
                if *version > current {
                    result.array_versions.insert(*reg, *version);
                }
            }
        }
    }

    // Join proven assertions - take union (these are frontend-verified)
    let mut seen_assertions: std::collections::HashSet<String> = std::collections::HashSet::new();
    for label in pred_labels {
        if let Some(state) = get_pred_state(label) {
            for assertion in &state.proven_assertions {
                let key = format!("{:?}", assertion);
                if seen_assertions.insert(key) {
                    result.proven_assertions.push(assertion.clone());
                }
            }
        }
    }

    Ok(result)
}

/// Join multiple types into their least upper bound
fn join_types(types: &[DtalType]) -> DtalType {
    if types.is_empty() {
        return DtalType::Int; // Default
    }

    if types.len() == 1 {
        return types[0].clone();
    }

    // Check if all types are the same
    let first = &types[0];
    if types.iter().all(|t| t == first) {
        return first.clone();
    }

    // If any type is an existential, try to preserve it.
    // ExistentialInt + SingletonInt → ExistentialInt (singleton is a subtype)
    // ExistentialInt + ExistentialInt (same) → keep as-is
    // ExistentialInt + Int → Int (conservative widening)
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

    // If different singleton ints, generalize to int
    let all_singleton_ints = types
        .iter()
        .all(|t| matches!(t, DtalType::SingletonInt(_) | DtalType::Int));
    if all_singleton_ints {
        return DtalType::Int;
    }

    // If different refined ints, generalize to int
    let all_numeric = types.iter().all(|t| {
        matches!(
            t,
            DtalType::Int
                | DtalType::SingletonInt(_)
                | DtalType::RefinedInt { .. }
                | DtalType::ExistentialInt { .. }
        )
    });
    if all_numeric {
        return DtalType::Int;
    }

    // Default: return first type (conservative)
    first.clone()
}

/// Check structural equality of types
fn types_structurally_equal(a: &DtalType, b: &DtalType) -> bool {
    a == b
}

/// Compute exit state by processing all instructions (non-verifying)
///
/// This function only updates the type state based on instruction definitions.
/// It does NOT verify that operands are defined - that's done separately after
/// the dataflow analysis reaches a fixed point.
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

/// Update type state based on instruction definitions (non-verifying).
///
/// Derives types using the same rules as the verifier (Xi & Harper derivation)
/// so that dataflow-computed states match verifier expectations.
fn update_state_for_instruction(instr: &DtalInstr, state: &mut TypeState) {
    use crate::backend::dtal::instr::BinaryOp;

    match instr {
        DtalInstr::MovImm { dst, imm, .. } => {
            state
                .register_types
                .insert(*dst, DtalType::SingletonInt(IndexExpr::Const(*imm)));
            // Add equality constraint so branch conditions can reference this value
            let reg_expr = crate::verifier::checker::reg_to_index_expr(dst);
            state
                .constraints
                .push(Constraint::Eq(reg_expr, IndexExpr::Const(*imm)));
        }
        DtalInstr::MovReg { dst, src, .. } => {
            let ty = state
                .register_types
                .get(src)
                .cloned()
                .unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, ty);
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
        }
        DtalInstr::Load { dst, base, ty, .. } => {
            // Derive element type from array base when available
            let derived_ty = if let Some(base_ty) = state.register_types.get(base)
                && let DtalType::Array { element_type, .. } = base_ty
            {
                element_type.as_ref().clone()
            } else {
                ty.clone()
            };
            state.register_types.insert(*dst, derived_ty);
        }
        DtalInstr::LoadOp { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::SetCC { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Bool);
        }
        DtalInstr::Not { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Bool);
        }
        DtalInstr::Neg { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::ShlImm { dst, ty, .. } | DtalInstr::ShrImm { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::TypeAnnotation { reg, ty } => {
            state.register_types.insert(*reg, ty.clone());
        }
        DtalInstr::Pop { dst, .. } => {
            let popped_ty = state.stack.pop().unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, popped_ty);
        }
        DtalInstr::Alloca { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::Call { return_ty, .. } => {
            use crate::backend::dtal::regs::PhysicalReg;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R0), return_ty.clone());
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
        }
        DtalInstr::Store { .. } => {}
        DtalInstr::ConstraintAssert { constraint, .. } => {
            // Propagate proven assertions through the dataflow.
            state.constraints.push(constraint.clone());
            state.proven_assertions.push(constraint.clone());
        }
        DtalInstr::Jmp { .. } | DtalInstr::Branch { .. } | DtalInstr::Ret => {}

        // Physical allocation instructions (post-regalloc)
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
        }
        DtalInstr::Idiv { src: _ } => {
            use crate::backend::dtal::regs::PhysicalReg;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::LR), DtalType::Int);
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R2), DtalType::Int);
        }
        DtalInstr::PortIn { dst, .. } => {
            state.register_types.insert(*dst, DtalType::Int);
        }
        DtalInstr::PortOut { .. } => {}
        DtalInstr::SpillStore { .. } => {}
        DtalInstr::SpillLoad { dst, ty, .. } => {
            state.register_types.insert(*dst, ty.clone());
        }
        DtalInstr::Prologue { .. } => {
            // Mirror the checker's Prologue handling: define all physical
            // registers as Int so they're available in successor blocks.
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
                    .insert(Reg::Physical(*preg), DtalType::Int);
            }
        }
        DtalInstr::Epilogue { .. } => {}
    }
}

/// Check if two type states are equal
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

    // Also check constraints, stack, and proven assertions
    a.constraints.len() == b.constraints.len()
        && a.constraints.iter().all(|c| b.constraints.contains(c))
        && a.stack.len() == b.stack.len()
        && a.stack.iter().zip(&b.stack).all(|(a, b)| a == b)
        && a.proven_assertions.len() == b.proven_assertions.len()
        && a.proven_assertions
            .iter()
            .all(|c| b.proven_assertions.contains(c))
}
