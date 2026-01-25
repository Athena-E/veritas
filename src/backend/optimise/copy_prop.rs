//! Copy Propagation Pass
//!
//! This pass replaces uses of registers that were defined via `MovReg`
//! with the original source register, reducing unnecessary copies.
//!
//! # Algorithm
//!
//! Forward dataflow within each block:
//! 1. Track `CopyMap: HashMap<VirtualReg, Reg>` mapping dst→src
//! 2. For each instruction:
//!    - Replace uses with resolved sources from CopyMap
//!    - If instruction redefines a register, invalidate stale copies
//!    - If `MovReg { dst, src }` where dst is virtual, record in CopyMap
//!
//! # Behavior
//!
//! - Tracks copies from both physical and virtual registers to virtual registers
//! - Only replaces virtual register uses (physical registers are not replaced)
//! - Conservative at block boundaries (clears map at each block entry)

use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use std::collections::HashMap;

/// Type alias for copy map: virtual destination → any source register
type CopyMap = HashMap<VirtualReg, Reg>;

/// Apply copy propagation to a function
///
/// Returns true if any changes were made
pub fn copy_propagate_function(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        changed |= copy_propagate_block(block);
    }

    changed
}

/// Apply copy propagation within a single block
fn copy_propagate_block(block: &mut DtalBlock) -> bool {
    let mut changed = false;
    let mut copy_map: CopyMap = HashMap::new();

    for instr in &mut block.instructions {
        // First, rewrite uses in this instruction
        changed |= rewrite_uses(instr, &copy_map);

        // Update copy map based on this instruction's effects
        update_copy_map(instr, &mut copy_map);
    }

    changed
}

/// Resolve a register through the copy chain to find the original source
///
/// Returns the ultimate source register (could be physical or virtual)
fn resolve(reg: VirtualReg, copy_map: &CopyMap) -> Reg {
    let mut current = Reg::Virtual(reg);
    // Follow the copy chain (handles transitive copies: a=b; c=a → c=b)
    loop {
        match current {
            Reg::Virtual(vreg) => {
                if let Some(&src) = copy_map.get(&vreg) {
                    if src == current {
                        break; // Avoid infinite loops
                    }
                    current = src;
                } else {
                    break;
                }
            }
            Reg::Physical(_) => break, // Physical registers are terminal
        }
    }
    current
}

/// Rewrite register uses in an instruction using the copy map
///
/// Returns true if any register was replaced
fn rewrite_uses(instr: &mut DtalInstr, copy_map: &CopyMap) -> bool {
    let mut changed = false;

    match instr {
        DtalInstr::MovReg { src, .. } => {
            changed |= try_replace_virtual(src, copy_map);
        }
        DtalInstr::Load { base, offset, .. } => {
            changed |= try_replace_virtual(base, copy_map);
            changed |= try_replace_virtual(offset, copy_map);
        }
        DtalInstr::Store { base, offset, src } => {
            changed |= try_replace_virtual(base, copy_map);
            changed |= try_replace_virtual(offset, copy_map);
            changed |= try_replace_virtual(src, copy_map);
        }
        DtalInstr::BinOp { lhs, rhs, .. } => {
            changed |= try_replace_virtual(lhs, copy_map);
            changed |= try_replace_virtual(rhs, copy_map);
        }
        DtalInstr::AddImm { src, .. } => {
            changed |= try_replace_virtual(src, copy_map);
        }
        DtalInstr::Cmp { lhs, rhs } => {
            changed |= try_replace_virtual(lhs, copy_map);
            changed |= try_replace_virtual(rhs, copy_map);
        }
        DtalInstr::CmpImm { lhs, .. } => {
            changed |= try_replace_virtual(lhs, copy_map);
        }
        DtalInstr::Not { src, .. } => {
            changed |= try_replace_virtual(src, copy_map);
        }
        DtalInstr::Push { src, .. } => {
            changed |= try_replace_virtual(src, copy_map);
        }
        // Instructions without register uses
        DtalInstr::MovImm { .. }
        | DtalInstr::Pop { .. }
        | DtalInstr::Alloca { .. }
        | DtalInstr::SetCC { .. }
        | DtalInstr::Jmp { .. }
        | DtalInstr::Branch { .. }
        | DtalInstr::Call { .. }
        | DtalInstr::Ret
        | DtalInstr::TypeAnnotation { .. }
        | DtalInstr::ConstraintAssume { .. }
        | DtalInstr::ConstraintAssert { .. } => {}
    }

    changed
}

/// Try to replace a virtual register with its resolved source
///
/// Returns true if the register was replaced
fn try_replace_virtual(reg: &mut Reg, copy_map: &CopyMap) -> bool {
    if let Reg::Virtual(vreg) = *reg {
        let resolved = resolve(vreg, copy_map);
        if resolved != *reg {
            *reg = resolved;
            return true;
        }
    }
    false
}

/// Update the copy map based on an instruction's definition
fn update_copy_map(instr: &DtalInstr, copy_map: &mut CopyMap) {
    // Get the register defined by this instruction
    let def_reg = match instr {
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

    // If this instruction defines a register, invalidate any copies that use it as source
    if let Some(def) = def_reg {
        if let Reg::Virtual(def_vreg) = def {
            // Remove any existing mapping FOR this register (it's being redefined)
            copy_map.remove(&def_vreg);
        }

        // Invalidate any mappings that have this register as their source
        // (works for both physical and virtual register sources)
        copy_map.retain(|_, src| *src != def);
    }

    // If this is a MovReg where dst is virtual, record the copy
    if let DtalInstr::MovReg { dst, src, .. } = instr {
        if let Reg::Virtual(dst_vreg) = dst {
            // Resolve the source through existing copies (if source is virtual)
            let resolved_src = match src {
                Reg::Virtual(src_vreg) => resolve(*src_vreg, copy_map),
                Reg::Physical(_) => *src, // Physical registers are already resolved
            };
            copy_map.insert(*dst_vreg, resolved_src);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{BinaryOp, TypeState};
    use crate::common::types::IType;

    #[test]
    fn test_simple_copy_propagation() {
        // v0 = 42
        // v1 = v0  (copy)
        // v2 = v1 + v1  (should become v0 + v0)
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut block = DtalBlock {
            label: "test".to_string(),
            entry_state: TypeState::new(),
            instructions: vec![
                DtalInstr::MovImm {
                    dst: v0,
                    imm: 42,
                    ty: IType::Int,
                },
                DtalInstr::MovReg {
                    dst: v1,
                    src: v0,
                    ty: IType::Int,
                },
                DtalInstr::BinOp {
                    op: BinaryOp::Add,
                    dst: v2,
                    lhs: v1,
                    rhs: v1,
                    ty: IType::Int,
                },
            ],
        };

        let changed = copy_propagate_block(&mut block);
        assert!(changed);

        // Check that the BinOp now uses v0 instead of v1
        if let DtalInstr::BinOp { lhs, rhs, .. } = &block.instructions[2] {
            assert_eq!(*lhs, v0);
            assert_eq!(*rhs, v0);
        } else {
            panic!("Expected BinOp instruction");
        }
    }

    #[test]
    fn test_transitive_copy_propagation() {
        // v0 = 42
        // v1 = v0
        // v2 = v1  (transitive copy)
        // v3 = v2 + v2  (should become v0 + v0)
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));
        let v3 = Reg::Virtual(VirtualReg(3));

        let mut block = DtalBlock {
            label: "test".to_string(),
            entry_state: TypeState::new(),
            instructions: vec![
                DtalInstr::MovImm {
                    dst: v0,
                    imm: 42,
                    ty: IType::Int,
                },
                DtalInstr::MovReg {
                    dst: v1,
                    src: v0,
                    ty: IType::Int,
                },
                DtalInstr::MovReg {
                    dst: v2,
                    src: v1,
                    ty: IType::Int,
                },
                DtalInstr::BinOp {
                    op: BinaryOp::Add,
                    dst: v3,
                    lhs: v2,
                    rhs: v2,
                    ty: IType::Int,
                },
            ],
        };

        let changed = copy_propagate_block(&mut block);
        assert!(changed);

        // Check that the BinOp now uses v0 instead of v2
        if let DtalInstr::BinOp { lhs, rhs, .. } = &block.instructions[3] {
            assert_eq!(*lhs, v0);
            assert_eq!(*rhs, v0);
        } else {
            panic!("Expected BinOp instruction");
        }
    }

    #[test]
    fn test_invalidation_on_redef() {
        // v0 = 42
        // v1 = v0  (copy)
        // v0 = 100  (redefine v0)
        // v2 = v1 + v1  (should stay v1 + v1, since v0 was redefined)
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let mut block = DtalBlock {
            label: "test".to_string(),
            entry_state: TypeState::new(),
            instructions: vec![
                DtalInstr::MovImm {
                    dst: v0,
                    imm: 42,
                    ty: IType::Int,
                },
                DtalInstr::MovReg {
                    dst: v1,
                    src: v0,
                    ty: IType::Int,
                },
                DtalInstr::MovImm {
                    dst: v0,
                    imm: 100,
                    ty: IType::Int,
                },
                DtalInstr::BinOp {
                    op: BinaryOp::Add,
                    dst: v2,
                    lhs: v1,
                    rhs: v1,
                    ty: IType::Int,
                },
            ],
        };

        let changed = copy_propagate_block(&mut block);
        // No change because the copy v1=v0 is invalidated by v0's redefinition
        assert!(!changed);

        // v1 should still be v1
        if let DtalInstr::BinOp { lhs, rhs, .. } = &block.instructions[3] {
            assert_eq!(*lhs, v1);
            assert_eq!(*rhs, v1);
        } else {
            panic!("Expected BinOp instruction");
        }
    }

    #[test]
    fn test_propagation_from_physical_regs() {
        use crate::backend::dtal::regs::PhysicalReg;

        // v0 = r0  (copy from physical)
        // v1 = v0 + v0  (should become r0 + r0)
        let r0 = Reg::Physical(PhysicalReg::R0);
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));

        let mut block = DtalBlock {
            label: "test".to_string(),
            entry_state: TypeState::new(),
            instructions: vec![
                DtalInstr::MovReg {
                    dst: v0,
                    src: r0,
                    ty: IType::Int,
                },
                DtalInstr::BinOp {
                    op: BinaryOp::Add,
                    dst: v1,
                    lhs: v0,
                    rhs: v0,
                    ty: IType::Int,
                },
            ],
        };

        let changed = copy_propagate_block(&mut block);
        // v0 should be replaced with r0
        assert!(changed);

        // Check that the BinOp now uses r0 instead of v0
        if let DtalInstr::BinOp { lhs, rhs, .. } = &block.instructions[1] {
            assert_eq!(*lhs, r0);
            assert_eq!(*rhs, r0);
        } else {
            panic!("Expected BinOp instruction");
        }
    }
}
