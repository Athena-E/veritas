//! Loop-Invariant Code Motion (LICM)
//!
//! Hoists pure computations from loop bodies to before the loop when all
//! their operands are defined outside the loop. This avoids redundant
//! recomputation on every iteration.
//!
//! # Algorithm
//!
//! 1. Build CFG (successors/predecessors) using liveness infrastructure
//! 2. Compute dominators via iterative dataflow
//! 3. Detect natural loops via back-edge identification
//! 4. For each loop, find the entry block (non-loop predecessor of header)
//! 5. Identify loop-invariant instructions (pure, all operands external)
//! 6. Hoist them to the entry block, before the terminal jump
//!
//! # Safety
//!
//! Only pure computations are hoisted (no memory, control flow, or
//! verification side effects). `TypeAnnotation` and `ConstraintAssert`
//! are never moved.

use crate::backend::dtal::instr::{DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use crate::backend::regalloc::liveness::LivenessAnalysis;
use std::collections::{HashMap, HashSet};

/// A detected natural loop
struct NaturalLoop {
    /// The loop header block label (target of the back-edge)
    _header: String,
    /// All block labels in the loop (including header)
    blocks: HashSet<String>,
    /// The entry block label (non-loop predecessor of header)
    entry: String,
}

/// Apply LICM to a function
///
/// Returns true if any instructions were hoisted
pub fn licm_function(func: &mut DtalFunction) -> bool {
    if func.blocks.len() < 2 {
        return false;
    }

    let (successors, predecessors) = LivenessAnalysis::build_cfg(func);
    let block_labels: Vec<String> = func.blocks.iter().map(|b| b.label.clone()).collect();

    let dominators = compute_dominators(&block_labels, &predecessors);
    let loops = find_loops(&block_labels, &successors, &predecessors, &dominators);

    if loops.is_empty() {
        return false;
    }

    // Collect all defs per block for quick lookup
    let block_defs = collect_block_defs(func);

    let mut changed = false;
    for lp in &loops {
        changed |= hoist_loop(func, lp, &block_defs);
    }

    changed
}

/// Compute dominators using iterative dataflow
///
/// dom[entry] = {entry}
/// dom[B] = {B} ∪ ∩{dom[P] | P ∈ predecessors(B)}
fn compute_dominators(
    block_labels: &[String],
    predecessors: &HashMap<String, Vec<String>>,
) -> HashMap<String, HashSet<String>> {
    let all_blocks: HashSet<String> = block_labels.iter().cloned().collect();
    let mut dom: HashMap<String, HashSet<String>> = HashMap::new();

    // Entry block dominates only itself
    if let Some(entry) = block_labels.first() {
        let mut entry_set = HashSet::new();
        entry_set.insert(entry.clone());
        dom.insert(entry.clone(), entry_set);
    }

    // All other blocks: start with all blocks
    for label in block_labels.iter().skip(1) {
        dom.insert(label.clone(), all_blocks.clone());
    }

    // Iterate until stable
    let mut changed = true;
    while changed {
        changed = false;
        for label in block_labels.iter().skip(1) {
            let preds = match predecessors.get(label) {
                Some(p) => p,
                None => continue,
            };

            // Intersect dominator sets of all predecessors
            let mut new_dom: Option<HashSet<String>> = None;
            for pred in preds {
                if let Some(pred_dom) = dom.get(pred) {
                    new_dom = Some(match new_dom {
                        None => pred_dom.clone(),
                        Some(current) => current.intersection(pred_dom).cloned().collect(),
                    });
                }
            }

            // Add self
            let mut new_dom = new_dom.unwrap_or_default();
            new_dom.insert(label.clone());

            if dom.get(label) != Some(&new_dom) {
                dom.insert(label.clone(), new_dom);
                changed = true;
            }
        }
    }

    dom
}

/// Find all natural loops in the function
fn find_loops(
    block_labels: &[String],
    successors: &HashMap<String, Vec<String>>,
    predecessors: &HashMap<String, Vec<String>>,
    dominators: &HashMap<String, HashSet<String>>,
) -> Vec<NaturalLoop> {
    let mut loops = Vec::new();

    // Find back-edges: B → H where H dominates B
    for label in block_labels {
        if let Some(succs) = successors.get(label) {
            for succ in succs {
                let is_back_edge = dominators
                    .get(label)
                    .is_some_and(|dom_set| dom_set.contains(succ));

                if is_back_edge {
                    // Found back-edge: label → succ
                    let header = succ.clone();
                    let loop_blocks = collect_natural_loop(&header, label, predecessors);

                    // Find entry block: predecessor of header not in loop
                    if let Some(header_preds) = predecessors.get(&header) {
                        for pred in header_preds {
                            if !loop_blocks.contains(pred) {
                                loops.push(NaturalLoop {
                                    _header: header.clone(),
                                    blocks: loop_blocks.clone(),
                                    entry: pred.clone(),
                                });
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    loops
}

/// Collect all blocks in a natural loop given a back-edge B → H
fn collect_natural_loop(
    header: &str,
    back_edge_source: &str,
    predecessors: &HashMap<String, Vec<String>>,
) -> HashSet<String> {
    let mut loop_blocks = HashSet::new();
    loop_blocks.insert(header.to_string());

    if header == back_edge_source {
        // Self-loop
        return loop_blocks;
    }

    let mut worklist = vec![back_edge_source.to_string()];
    while let Some(node) = worklist.pop() {
        if loop_blocks.insert(node.clone()) {
            // New node — add its predecessors to worklist
            if let Some(preds) = predecessors.get(&node) {
                for pred in preds {
                    if !loop_blocks.contains(pred) {
                        worklist.push(pred.clone());
                    }
                }
            }
        }
    }

    loop_blocks
}

/// Count the number of definitions per virtual register across all blocks.
/// A register with multiple definitions is loop-carried or non-SSA; hoisting
/// one of its writes would break the update flow.
fn count_all_defs(func: &DtalFunction) -> HashMap<VirtualReg, usize> {
    let mut counts: HashMap<VirtualReg, usize> = HashMap::new();
    for block in &func.blocks {
        for instr in &block.instructions {
            if let Some(vreg) = instruction_def(instr) {
                *counts.entry(vreg).or_insert(0) += 1;
            }
        }
    }
    counts
}

/// Collect the set of virtual registers defined in each block
fn collect_block_defs(func: &DtalFunction) -> HashMap<String, HashSet<VirtualReg>> {
    let mut result = HashMap::new();
    for block in &func.blocks {
        let mut defs = HashSet::new();
        for instr in &block.instructions {
            if let Some(vreg) = instruction_def(instr) {
                defs.insert(vreg);
            }
        }
        result.insert(block.label.clone(), defs);
    }
    result
}

/// Hoist loop-invariant instructions from a single loop
fn hoist_loop(
    func: &mut DtalFunction,
    lp: &NaturalLoop,
    block_defs: &HashMap<String, HashSet<VirtualReg>>,
) -> bool {
    // Compute registers defined outside the loop
    let mut external_defs: HashSet<VirtualReg> = HashSet::new();
    for block in &func.blocks {
        if !lp.blocks.contains(&block.label)
            && let Some(defs) = block_defs.get(&block.label)
        {
            external_defs.extend(defs);
        }
    }

    // Also include function parameters (physical regs mapped to virtuals)
    // These are implicitly available at function entry

    // Collect registers defined inside the loop
    let mut loop_defs: HashSet<VirtualReg> = HashSet::new();
    for block_label in &lp.blocks {
        if let Some(defs) = block_defs.get(block_label) {
            loop_defs.extend(defs);
        }
    }

    // Count total definitions per register across the entire function.
    // DTAL isn't strict SSA: loop-carried virtuals (e.g. the induction
    // variable) are written in both the pre-header and the loop body. Hoisting
    // a second write of such a register into the pre-header destroys the
    // loop's update step. Only registers with exactly one definition in the
    // whole function are safe to relocate.
    let def_counts = count_all_defs(func);

    // Find hoistable instructions using fixed-point iteration
    // An instruction is hoistable if:
    // 1. It is a pure computation (is_hoistable_kind)
    // 2. Its destination register has only one definition in the function
    //    (otherwise we would break a loop-carried update)
    // 3. All its operand registers are either external or defined by
    //    another hoistable instruction
    let mut hoistable_defs: HashSet<VirtualReg> = HashSet::new();
    let mut hoistable_instrs: Vec<(String, usize)> = Vec::new(); // (block_label, instr_index)

    let mut changed = true;
    while changed {
        changed = false;

        for block in func.blocks.iter() {
            if !lp.blocks.contains(&block.label) {
                continue;
            }

            for (i, instr) in block.instructions.iter().enumerate() {
                // Skip if already identified as hoistable
                if let Some(def) = instruction_def(instr)
                    && hoistable_defs.contains(&def)
                {
                    continue;
                }

                if !is_hoistable_kind(instr) {
                    continue;
                }

                // Refuse to hoist a definition of a register written anywhere
                // else in the function — that signals a loop-carried variable
                // whose update must stay in place.
                if let Some(def) = instruction_def(instr)
                    && def_counts.get(&def).copied().unwrap_or(0) > 1
                {
                    continue;
                }

                // Check all operands are either external-only or hoistable
                // A register redefined inside the loop is NOT external even if
                // it was also defined outside (the loop definition shadows it)
                let uses = instruction_uses(instr);
                let all_available = uses.iter().all(|u| {
                    hoistable_defs.contains(u)
                        || (external_defs.contains(u) && !loop_defs.contains(u))
                });

                if all_available {
                    if let Some(def) = instruction_def(instr) {
                        hoistable_defs.insert(def);
                    }
                    hoistable_instrs.push((block.label.clone(), i));
                    changed = true;
                }
            }
        }
    }

    if hoistable_instrs.is_empty() {
        return false;
    }

    // Collect hoisted instructions (in order)
    let mut hoisted: Vec<DtalInstr> = Vec::new();
    for (block_label, instr_idx) in &hoistable_instrs {
        let block = func
            .blocks
            .iter()
            .find(|b| b.label == *block_label)
            .unwrap();
        hoisted.push(block.instructions[*instr_idx].clone());
    }

    // Remove hoisted instructions from their source blocks
    // Build a set of (block_label, index) pairs to remove
    let remove_set: HashSet<(String, usize)> = hoistable_instrs.into_iter().collect();
    for block in &mut func.blocks {
        if !lp.blocks.contains(&block.label) {
            continue;
        }
        let label = block.label.clone();
        let mut idx = 0;
        block.instructions.retain(|_| {
            let keep = !remove_set.contains(&(label.clone(), idx));
            idx += 1;
            keep
        });
    }

    // Insert hoisted instructions into the entry block before the terminal Jmp
    let entry_block = func
        .blocks
        .iter_mut()
        .find(|b| b.label == lp.entry)
        .unwrap();

    // Find insertion point: before the first MovReg that targets a phi dst,
    // or before the terminal Jmp/Branch, whichever comes first
    let insert_pos = entry_block
        .instructions
        .iter()
        .rposition(|instr| !matches!(instr, DtalInstr::Jmp { .. } | DtalInstr::MovReg { .. }))
        .map(|p| p + 1)
        .unwrap_or(0);

    // Insert all hoisted instructions at the insertion point
    for (offset, instr) in hoisted.into_iter().enumerate() {
        entry_block.instructions.insert(insert_pos + offset, instr);
    }

    true
}

/// Check if an instruction is a kind that can be hoisted (pure computation)
fn is_hoistable_kind(instr: &DtalInstr) -> bool {
    matches!(
        instr,
        DtalInstr::MovImm { .. }
            | DtalInstr::BinOp { .. }
            | DtalInstr::AddImm { .. }
            | DtalInstr::ShlImm { .. }
            | DtalInstr::ShrImm { .. }
            | DtalInstr::Not { .. }
            | DtalInstr::Neg { .. }
            | DtalInstr::MovReg { .. }
    )
}

/// Get the virtual register defined by an instruction (if any)
fn instruction_def(instr: &DtalInstr) -> Option<VirtualReg> {
    let reg = match instr {
        DtalInstr::MovImm { dst, .. }
        | DtalInstr::MovReg { dst, .. }
        | DtalInstr::Load { dst, .. }
        | DtalInstr::BinOp { dst, .. }
        | DtalInstr::AddImm { dst, .. }
        | DtalInstr::ShlImm { dst, .. }
        | DtalInstr::ShrImm { dst, .. }
        | DtalInstr::Not { dst, .. }
        | DtalInstr::Neg { dst, .. }
        | DtalInstr::Pop { dst, .. }
        | DtalInstr::Alloca { dst, .. }
        | DtalInstr::SetCC { dst, .. } => Some(*dst),
        _ => None,
    };

    reg.and_then(|r| match r {
        Reg::Virtual(v) => Some(v),
        Reg::Physical(_) => None,
    })
}

/// Get the virtual registers used by an instruction
fn instruction_uses(instr: &DtalInstr) -> Vec<VirtualReg> {
    let regs: Vec<Reg> = match instr {
        DtalInstr::MovReg { src, .. } => vec![*src],
        DtalInstr::BinOp { lhs, rhs, .. } => vec![*lhs, *rhs],
        DtalInstr::AddImm { src, .. } => vec![*src],
        DtalInstr::ShlImm { src, .. } | DtalInstr::ShrImm { src, .. } => vec![*src],
        DtalInstr::Not { src, .. } | DtalInstr::Neg { src, .. } => vec![*src],
        // MovImm has no register uses
        DtalInstr::MovImm { .. } => vec![],
        _ => vec![],
    };

    regs.into_iter()
        .filter_map(|r| match r {
            Reg::Virtual(v) => Some(v),
            Reg::Physical(_) => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{BinaryOp, DtalBlock, TypeState};
    use crate::backend::dtal::regs::VirtualReg;
    use crate::backend::dtal::types::DtalType;

    fn vreg(n: u32) -> Reg {
        Reg::Virtual(VirtualReg(n))
    }

    /// Build a function with a simple loop:
    ///
    /// entry:
    ///     mov v10, 0           (loop init)
    ///     mov v0, v10          (phi move: i = 0)
    ///     jmp header
    ///
    /// header:
    ///     cmpimm v0, 10
    ///     blt body
    ///     jmp exit
    ///
    /// body:
    ///     mov v3, 7            ← loop-invariant (constant)
    ///     mov v4, 3            ← loop-invariant (constant)
    ///     mul v5, v3, v4       ← loop-invariant (both operands invariant)
    ///     add v6, v0, v5       ← NOT invariant (uses v0 = loop var)
    ///     addimm v1, v0, 1     (i_next = i + 1)
    ///     mov v0, v1           (phi move: i = i_next)
    ///     jmp header
    ///
    /// exit:
    ///     ret
    fn make_loop_func() -> DtalFunction {
        DtalFunction {
            name: "test_loop".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: ".test_loop_bb0".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: vreg(10),
                            imm: 0,
                            ty: DtalType::Int,
                        },
                        DtalInstr::MovReg {
                            dst: vreg(0),
                            src: vreg(10),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_loop_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_loop_bb1".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::CmpImm {
                            lhs: vreg(0),
                            imm: 10,
                        },
                        DtalInstr::Branch {
                            cond: crate::backend::dtal::instr::CmpOp::Lt,
                            target: ".test_loop_bb2".to_string(),
                        },
                        DtalInstr::Jmp {
                            target: ".test_loop_bb3".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_loop_bb2".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: vreg(3),
                            imm: 7,
                            ty: DtalType::Int,
                        },
                        DtalInstr::MovImm {
                            dst: vreg(4),
                            imm: 3,
                            ty: DtalType::Int,
                        },
                        DtalInstr::BinOp {
                            op: BinaryOp::Mul,
                            dst: vreg(5),
                            lhs: vreg(3),
                            rhs: vreg(4),
                            ty: DtalType::Int,
                        },
                        DtalInstr::BinOp {
                            op: BinaryOp::Add,
                            dst: vreg(6),
                            lhs: vreg(0),
                            rhs: vreg(5),
                            ty: DtalType::Int,
                        },
                        DtalInstr::AddImm {
                            dst: vreg(1),
                            src: vreg(0),
                            imm: 1,
                            ty: DtalType::Int,
                        },
                        DtalInstr::MovReg {
                            dst: vreg(0),
                            src: vreg(1),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_loop_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_loop_bb3".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::Ret],
                },
            ],
        }
    }

    #[test]
    fn test_licm_hoists_invariant_instructions() {
        let mut func = make_loop_func();

        let changed = licm_function(&mut func);
        assert!(changed);

        // Entry block should now contain the hoisted instructions
        let entry = &func.blocks[0];
        // Original: MovImm v10, MovReg v0, Jmp
        // After LICM: MovImm v10, [MovImm v3, MovImm v4, BinOp Mul v5], MovReg v0, Jmp
        // The hoisted instructions go before the phi moves

        // Check that v3=7, v4=3, v5=v3*v4 are in the entry block
        let has_v3 = entry
            .instructions
            .iter()
            .any(|i| matches!(i, DtalInstr::MovImm { dst, imm: 7, .. } if *dst == vreg(3)));
        let has_v4 = entry
            .instructions
            .iter()
            .any(|i| matches!(i, DtalInstr::MovImm { dst, imm: 3, .. } if *dst == vreg(4)));
        let has_v5 = entry.instructions.iter().any(
            |i| matches!(i, DtalInstr::BinOp { op: BinaryOp::Mul, dst, .. } if *dst == vreg(5)),
        );
        assert!(has_v3, "v3 = 7 should be hoisted to entry");
        assert!(has_v4, "v4 = 3 should be hoisted to entry");
        assert!(has_v5, "v5 = v3 * v4 should be hoisted to entry");

        // Body should NOT have v3, v4, v5 definitions anymore
        let body = &func.blocks[2];
        let body_has_v3 = body
            .instructions
            .iter()
            .any(|i| matches!(i, DtalInstr::MovImm { dst, imm: 7, .. } if *dst == vreg(3)));
        assert!(!body_has_v3, "v3 should be removed from body");

        // Body should still have v6 (depends on loop var v0)
        let body_has_v6 = body.instructions.iter().any(
            |i| matches!(i, DtalInstr::BinOp { op: BinaryOp::Add, dst, .. } if *dst == vreg(6)),
        );
        assert!(body_has_v6, "v6 should remain in body (uses loop var)");
    }

    #[test]
    fn test_licm_no_hoist_when_nothing_invariant() {
        // Loop where every instruction depends on the loop variable
        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: ".test_bb0".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: vreg(0),
                            imm: 0,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb1".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::CmpImm {
                            lhs: vreg(0),
                            imm: 10,
                        },
                        DtalInstr::Branch {
                            cond: crate::backend::dtal::instr::CmpOp::Lt,
                            target: ".test_bb2".to_string(),
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb3".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb2".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        // v1 = v0 * 2 — depends on loop var
                        DtalInstr::BinOp {
                            op: BinaryOp::Mul,
                            dst: vreg(1),
                            lhs: vreg(0),
                            rhs: vreg(0),
                            ty: DtalType::Int,
                        },
                        DtalInstr::AddImm {
                            dst: vreg(0),
                            src: vreg(0),
                            imm: 1,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb3".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::Ret],
                },
            ],
        };

        let changed = licm_function(&mut func);
        assert!(!changed);
    }

    #[test]
    fn test_licm_no_hoist_side_effects() {
        // Loop with a Store — should not be hoisted
        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: ".test_bb0".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: vreg(0),
                            imm: 0,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb1".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::CmpImm {
                            lhs: vreg(0),
                            imm: 10,
                        },
                        DtalInstr::Branch {
                            cond: crate::backend::dtal::instr::CmpOp::Lt,
                            target: ".test_bb2".to_string(),
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb3".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb2".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        // Store has side effects — not hoistable
                        DtalInstr::Store {
                            base: vreg(10),
                            offset: vreg(0),
                            src: vreg(11),
                        },
                        DtalInstr::AddImm {
                            dst: vreg(0),
                            src: vreg(0),
                            imm: 1,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: ".test_bb1".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: ".test_bb3".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::Ret],
                },
            ],
        };

        let changed = licm_function(&mut func);
        assert!(!changed);
    }
}
