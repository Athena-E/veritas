//! Dead Code Elimination Pass
//!
//! This pass removes instructions whose destination registers are never used.
//! It reuses the existing `LivenessAnalysis` from the register allocator.
//!
//! # Algorithm
//!
//! 1. Run liveness analysis to compute which registers are live after each instruction
//! 2. For each instruction that defines a register:
//!    - If the defined register is not in the live_after set
//!    - And the instruction has no side effects
//!    - Remove the instruction
//! 3. Iterate until no more changes (removing dead code may expose more dead code)
//!
//! # Side Effects
//!
//! The following instructions are never removed (they have side effects):
//! - Store, Call, Jmp, Branch, Ret, Push
//! - ConstraintAssert, ConstraintAssume

use crate::backend::dtal::instr::{DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use crate::backend::regalloc::liveness::LivenessAnalysis;
use std::collections::HashSet;

/// Eliminate dead code from a function
///
/// Returns true if any instructions were removed
pub fn eliminate_dead_code(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    // Run liveness analysis
    let liveness = LivenessAnalysis::analyze(func);

    // Process each block
    for block in &mut func.blocks {
        let block_liveness = match liveness.blocks.get(&block.label) {
            Some(l) => l,
            None => continue,
        };

        // Get per-instruction liveness (live_after for each instruction)
        let live_after_sets =
            LivenessAnalysis::compute_instruction_liveness(block, &block_liveness.live_out);

        // Build list of instruction indices to keep
        let mut keep_indices = Vec::new();
        for (i, instr) in block.instructions.iter().enumerate() {
            let live_after = if i < live_after_sets.len() {
                &live_after_sets[i]
            } else {
                // Fallback to live_out if we don't have per-instruction info
                &block_liveness.live_out
            };

            if should_keep_instruction(instr, live_after) {
                keep_indices.push(i);
            } else {
                changed = true;
            }
        }

        // Rebuild instruction list with only kept instructions
        if changed {
            let new_instructions: Vec<_> = keep_indices
                .into_iter()
                .map(|i| block.instructions[i].clone())
                .collect();
            block.instructions = new_instructions;
        }
    }

    changed
}

/// Check if an instruction should be kept
///
/// An instruction is kept if:
/// - It has side effects (Store, Call, etc.), OR
/// - Its defined register is live after the instruction
/// - It's not a trivial self-move (mov r, r)
fn should_keep_instruction(instr: &DtalInstr, live_after: &HashSet<VirtualReg>) -> bool {
    // Remove trivial self-moves (mov r, r where src == dst)
    if is_trivial_move(instr) {
        return false;
    }

    // Always keep instructions with side effects
    if has_side_effects(instr) {
        return true;
    }

    // Check if the instruction defines a register
    let def_reg = instruction_def(instr);

    match def_reg {
        // If no definition, keep it (shouldn't happen for non-side-effect instrs)
        None => true,
        // Keep if the defined register is live after this instruction
        Some(vreg) => live_after.contains(&vreg),
    }
}

/// Check if an instruction is a trivial self-move (mov r, r)
fn is_trivial_move(instr: &DtalInstr) -> bool {
    if let DtalInstr::MovReg { dst, src, .. } = instr {
        dst == src
    } else {
        false
    }
}

/// Check if an instruction has side effects (should never be removed)
fn has_side_effects(instr: &DtalInstr) -> bool {
    matches!(
        instr,
        DtalInstr::Store { .. }
            | DtalInstr::Call { .. }
            | DtalInstr::Jmp { .. }
            | DtalInstr::Branch { .. }
            | DtalInstr::Ret
            | DtalInstr::Push { .. }
            | DtalInstr::ConstraintAssert { .. }
            | DtalInstr::ConstraintAssume { .. }
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
        | DtalInstr::Not { dst, .. }
        | DtalInstr::Pop { dst, .. }
        | DtalInstr::Alloca { dst, .. }
        | DtalInstr::SetCC { dst, .. } => Some(*dst),
        _ => None,
    };

    // Only return virtual registers
    reg.and_then(|r| match r {
        Reg::Virtual(v) => Some(v),
        Reg::Physical(_) => None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{BinaryOp, DtalBlock, TypeState};
    use crate::common::types::IType;

    #[test]
    fn test_remove_unused_mov() {
        // v0 = 42  (used)
        // v1 = 100 (unused - should be removed)
        // push v0
        // ret
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: IType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 100,
                        ty: IType::Int,
                    },
                    DtalInstr::Push {
                        src: v0,
                        ty: IType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        assert!(changed);

        // v1 = 100 should have been removed
        assert_eq!(func.blocks[0].instructions.len(), 3);

        // Check remaining instructions
        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::MovImm { dst, imm: 42, .. } if *dst == v0
        ));
        assert!(matches!(
            &func.blocks[0].instructions[1],
            DtalInstr::Push { src, .. } if *src == v0
        ));
        assert!(matches!(&func.blocks[0].instructions[2], DtalInstr::Ret));
    }

    #[test]
    fn test_keep_used_values() {
        // v0 = 42
        // v1 = v0 + v0
        // push v1
        // ret
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: IType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v1,
                        lhs: v0,
                        rhs: v0,
                        ty: IType::Int,
                    },
                    DtalInstr::Push {
                        src: v1,
                        ty: IType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        // All values are used, nothing should be removed
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }

    #[test]
    fn test_keep_side_effects() {
        // Store instruction should never be removed even if "unused"
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 0,
                        ty: IType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 0,
                        ty: IType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v2,
                        imm: 42,
                        ty: IType::Int,
                    },
                    DtalInstr::Store {
                        base: v0,
                        offset: v1,
                        src: v2,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        // Nothing should be removed - all values feed into the Store
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 5);
    }

    #[test]
    fn test_cascading_dead_code() {
        // v0 = 42
        // v1 = v0 + v0  (this becomes dead if v2 is dead)
        // v2 = v1 + v1  (dead - not used)
        // ret
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: IType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v1,
                        lhs: v0,
                        rhs: v0,
                        ty: IType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v2,
                        lhs: v1,
                        rhs: v1,
                        ty: IType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        // First pass: v2 is dead, remove it
        let changed1 = eliminate_dead_code(&mut func);
        assert!(changed1);

        // Second pass: now v1 is dead, remove it
        let changed2 = eliminate_dead_code(&mut func);
        assert!(changed2);

        // Third pass: now v0 is dead, remove it
        let changed3 = eliminate_dead_code(&mut func);
        assert!(changed3);

        // Only ret should remain
        assert_eq!(func.blocks[0].instructions.len(), 1);
        assert!(matches!(&func.blocks[0].instructions[0], DtalInstr::Ret));
    }
}
