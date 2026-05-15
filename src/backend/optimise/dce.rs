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

use crate::backend::regalloc::liveness::LivenessAnalysis;
use crate::dtal::instr::{DtalFunction, DtalInstr};
use crate::dtal::regs::{Reg, VirtualReg};
use std::collections::HashSet;

pub fn eliminate_dead_code(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    let liveness = LivenessAnalysis::analyze(func);

    for block in &mut func.blocks {
        let block_liveness = match liveness.blocks.get(&block.label) {
            Some(l) => l,
            None => continue,
        };

        let live_after_sets =
            LivenessAnalysis::compute_instruction_liveness(block, &block_liveness.live_out);

        let mut keep_indices = Vec::new();
        for (i, instr) in block.instructions.iter().enumerate() {
            let live_after = if i < live_after_sets.len() {
                &live_after_sets[i]
            } else {
                &block_liveness.live_out
            };

            if should_keep_instruction(instr, live_after) {
                keep_indices.push(i);
            } else {
                changed = true;
            }
        }

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

fn should_keep_instruction(instr: &DtalInstr, live_after: &HashSet<VirtualReg>) -> bool {
    if is_trivial_move(instr) {
        return false;
    }

    if has_side_effects(instr) {
        return true;
    }

    if let DtalInstr::TypeAnnotation { reg, .. } = instr {
        return match reg {
            Reg::Virtual(vreg) => live_after.contains(vreg),
            Reg::Physical(_) => true,
        };
    }

    let def_reg = instruction_def(instr);

    match def_reg {
        None => true,
        Some(vreg) => live_after.contains(&vreg),
    }
}

fn is_trivial_move(instr: &DtalInstr) -> bool {
    if let DtalInstr::MovReg { dst, src, .. } = instr {
        dst == src
    } else {
        false
    }
}

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
    )
}

fn instruction_def(instr: &DtalInstr) -> Option<VirtualReg> {
    let reg = match instr {
        DtalInstr::MovImm { dst, .. }
        | DtalInstr::MovReg { dst, .. }
        | DtalInstr::Load { dst, .. }
        | DtalInstr::LoadOp { dst, .. }
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
#[cfg(test)]

mod tests {
    use super::*;
    use crate::dtal::instr::{BinaryOp, DtalBlock, TypeState};
    use crate::dtal::types::DtalType;
    #[test]

    fn test_remove_unused_mov() {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 100,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Push {
                        src: v0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        assert!(changed);

        assert_eq!(func.blocks[0].instructions.len(), 3);

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
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v1,
                        lhs: v0,
                        rhs: v0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Push {
                        src: v1,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }
    #[test]

    fn test_keep_side_effects() {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v2,
                        imm: 42,
                        ty: DtalType::Int,
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
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 5);
    }
    #[test]

    fn test_cascading_dead_code() {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v1,
                        lhs: v0,
                        rhs: v0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v2,
                        lhs: v1,
                        rhs: v1,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed1 = eliminate_dead_code(&mut func);
        assert!(changed1);

        let changed2 = eliminate_dead_code(&mut func);
        assert!(changed2);

        let changed3 = eliminate_dead_code(&mut func);
        assert!(changed3);

        assert_eq!(func.blocks[0].instructions.len(), 1);
        assert!(matches!(&func.blocks[0].instructions[0], DtalInstr::Ret));
    }
    #[test]

    fn test_remove_dead_type_annotation() {
        let v0 = Reg::Virtual(VirtualReg(0));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::TypeAnnotation {
                        reg: v0,
                        ty: DtalType::Bool,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        assert!(changed);

        assert_eq!(func.blocks[0].instructions.len(), 1);
        assert!(matches!(&func.blocks[0].instructions[0], DtalInstr::Ret));
    }
    #[test]

    fn test_keep_live_type_annotation() {
        let v0 = Reg::Virtual(VirtualReg(0));

        let mut func = DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::TypeAnnotation {
                        reg: v0,
                        ty: DtalType::Bool,
                    },
                    DtalInstr::Push {
                        src: v0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let changed = eliminate_dead_code(&mut func);
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }
}
