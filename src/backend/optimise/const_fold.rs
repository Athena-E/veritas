//! Constant Folding & Immediate Operand Folding Pass
//!
//! This pass performs two related optimisations on DTAL IR:
//!
//! 1. **Constant folding**: When a `BinOp` has both operands known at compile
//!    time (via preceding `MovImm` definitions), evaluate the operation and
//!    replace the `BinOp` with a single `MovImm`.
//!
//! 2. **Immediate operand folding**: When a `BinOp Add` has exactly one operand
//!    known, replace with `AddImm`. When a `Cmp` has RHS known, replace with
//!    `CmpImm`.
//!
//! # Algorithm
//!
//! Forward dataflow within each block:
//! 1. Track `ConstMap: HashMap<VirtualReg, i128>` mapping registers to known constants
//! 2. For each instruction:
//!    - If `MovImm { dst, imm, .. }` where dst is virtual, record in ConstMap
//!    - If `BinOp` with both operands in ConstMap, evaluate and replace with `MovImm`
//!    - If `BinOp Add` with one operand in ConstMap, replace with `AddImm`
//!    - If `Cmp { lhs, rhs }` with rhs in ConstMap, replace with `CmpImm`
//!    - If instruction redefines a register, invalidate stale entries
//!
//! # Safety
//!
//! - Division/modulo by zero is never folded (the instruction is left as-is)
//! - Overflow wraps using standard two's-complement semantics (i128::wrapping_*)
//! - Conservative at block boundaries (clears map at each block entry)

use crate::backend::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use crate::backend::dtal::types::DtalType;
use std::collections::HashMap;

/// Type alias for the constant map: virtual register → known immediate value
type ConstMap = HashMap<VirtualReg, i128>;

/// Apply constant folding to a function
///
/// Returns true if any changes were made
pub fn constant_fold_function(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        changed |= constant_fold_block(block);
    }

    changed
}

/// Apply constant folding within a single block
fn constant_fold_block(block: &mut DtalBlock) -> bool {
    let mut changed = false;
    let mut const_map: ConstMap = HashMap::new();

    for instr in &mut block.instructions {
        changed |= try_fold(instr, &const_map);
        update_const_map(instr, &mut const_map);
    }

    changed
}

/// Try to fold an instruction using known constants
///
/// Returns true if the instruction was rewritten
fn try_fold(instr: &mut DtalInstr, const_map: &ConstMap) -> bool {
    match instr {
        // Constant-fold BinOp when both operands are known
        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } => {
            let lhs_val = lookup(lhs, const_map);
            let rhs_val = lookup(rhs, const_map);

            match (lhs_val, rhs_val) {
                // Both known → full constant fold
                (Some(l), Some(r)) => {
                    if let Some(result) = eval_binop(*op, l, r) {
                        *instr = DtalInstr::MovImm {
                            dst: *dst,
                            imm: result,
                            ty: ty.clone(),
                        };
                        return true;
                    }
                    false
                }

                // One known → immediate operand folding and identity rewrites

                // Add: fold to AddImm (commutative)
                (Some(imm), None) if *op == BinaryOp::Add => {
                    let src = *rhs;
                    *instr = DtalInstr::AddImm {
                        dst: *dst,
                        src,
                        imm,
                        ty: ty.clone(),
                    };
                    true
                }
                (None, Some(imm)) if *op == BinaryOp::Add => {
                    let src = *lhs;
                    *instr = DtalInstr::AddImm {
                        dst: *dst,
                        src,
                        imm,
                        ty: ty.clone(),
                    };
                    true
                }

                // Sub: x - 0 = x
                (None, Some(0)) if *op == BinaryOp::Sub => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }

                // Mul: x * 0 = 0 (commutative)
                (Some(0), None) | (None, Some(0)) if *op == BinaryOp::Mul => {
                    *instr = DtalInstr::MovImm {
                        dst: *dst,
                        imm: 0,
                        ty: ty.clone(),
                    };
                    true
                }
                // Mul: x * 1 = x (commutative)
                (Some(1), None) if *op == BinaryOp::Mul => {
                    let src = *rhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }
                (None, Some(1)) if *op == BinaryOp::Mul => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }

                // Div: x / 1 = x
                (None, Some(1)) if *op == BinaryOp::Div => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }

                // Mod: x % 1 = 0
                (None, Some(1)) if *op == BinaryOp::Mod => {
                    *instr = DtalInstr::MovImm {
                        dst: *dst,
                        imm: 0,
                        ty: ty.clone(),
                    };
                    true
                }

                // Strength reduction: Mul by power of 2 → ShlImm (commutative)
                (Some(imm), None) if *op == BinaryOp::Mul && is_power_of_two(imm) => {
                    *instr = DtalInstr::ShlImm {
                        dst: *dst,
                        src: *rhs,
                        imm: imm.trailing_zeros() as u8,
                        ty: ty.clone(),
                    };
                    true
                }
                (None, Some(imm)) if *op == BinaryOp::Mul && is_power_of_two(imm) => {
                    *instr = DtalInstr::ShlImm {
                        dst: *dst,
                        src: *lhs,
                        imm: imm.trailing_zeros() as u8,
                        ty: ty.clone(),
                    };
                    true
                }

                // Strength reduction: unsigned Div by power of 2 → ShrImm
                (None, Some(imm))
                    if *op == BinaryOp::Div && is_unsigned(ty) && is_power_of_two(imm) =>
                {
                    *instr = DtalInstr::ShrImm {
                        dst: *dst,
                        src: *lhs,
                        imm: imm.trailing_zeros() as u8,
                        ty: ty.clone(),
                    };
                    true
                }

                _ => false,
            }
        }

        // Fold Cmp with constant RHS into CmpImm
        DtalInstr::Cmp { lhs, rhs } => {
            if let Some(imm) = lookup(rhs, const_map) {
                let lhs_reg = *lhs;
                *instr = DtalInstr::CmpImm { lhs: lhs_reg, imm };
                true
            } else {
                false
            }
        }

        _ => false,
    }
}

/// Look up a register's constant value if it is a virtual register in the map
fn lookup(reg: &Reg, const_map: &ConstMap) -> Option<i128> {
    match reg {
        Reg::Virtual(vreg) => const_map.get(vreg).copied(),
        Reg::Physical(_) => None,
    }
}

/// Check if a value is a power of two (positive, exactly one bit set)
fn is_power_of_two(val: i128) -> bool {
    val > 1 && (val & (val - 1)) == 0
}

/// Check if a DTAL type is unsigned
fn is_unsigned(ty: &DtalType) -> bool {
    matches!(ty, DtalType::U64)
}

/// Evaluate a binary operation at compile time
///
/// Returns `None` for division/modulo by zero (unsafe to fold)
fn eval_binop(op: BinaryOp, lhs: i128, rhs: i128) -> Option<i128> {
    match op {
        BinaryOp::Add => Some(lhs.wrapping_add(rhs)),
        BinaryOp::Sub => Some(lhs.wrapping_sub(rhs)),
        BinaryOp::Mul => Some(lhs.wrapping_mul(rhs)),
        BinaryOp::Div => {
            if rhs == 0 {
                None
            } else {
                Some(lhs.wrapping_div(rhs))
            }
        }
        BinaryOp::Mod => {
            if rhs == 0 {
                None
            } else {
                Some(lhs.wrapping_rem(rhs))
            }
        }
        BinaryOp::BitAnd => Some(lhs & rhs),
        BinaryOp::BitOr => Some(lhs | rhs),
        BinaryOp::BitXor => Some(lhs ^ rhs),
        BinaryOp::Shl => Some(lhs.wrapping_shl(rhs as u32)),
        BinaryOp::Shr => Some(lhs.wrapping_shr(rhs as u32)),
        BinaryOp::And => Some(if (lhs != 0) && (rhs != 0) { 1 } else { 0 }),
        BinaryOp::Or => Some(if (lhs != 0) || (rhs != 0) { 1 } else { 0 }),
    }
}

/// Update the constant map based on an instruction's definition
fn update_const_map(instr: &DtalInstr, const_map: &mut ConstMap) {
    match instr {
        // Record new constant definitions
        DtalInstr::MovImm {
            dst: Reg::Virtual(vreg),
            imm,
            ..
        } => {
            const_map.insert(*vreg, *imm);
        }

        // Any other instruction that defines a virtual register invalidates it
        _ => {
            if let Some(Reg::Virtual(vreg)) = instruction_dst(instr) {
                const_map.remove(&vreg);
            }
        }
    }
}

/// Get the destination register of an instruction (if any)
fn instruction_dst(instr: &DtalInstr) -> Option<Reg> {
    match instr {
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
        | DtalInstr::SetCC { dst, .. }
        | DtalInstr::SpillLoad { dst, .. } => Some(*dst),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{DtalBlock, DtalFunction, TypeState};
    use crate::backend::dtal::types::DtalType;

    fn vreg(n: u32) -> Reg {
        Reg::Virtual(VirtualReg(n))
    }

    fn make_func(instructions: Vec<DtalInstr>) -> DtalFunction {
        DtalFunction {
            name: "test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions,
            }],
        }
    }

    #[test]
    fn test_fold_add_two_constants() {
        // v0 = 10; v1 = 32; v2 = v0 + v1  →  v2 = 42
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 10,
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 32,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        // The BinOp should now be a MovImm with value 42
        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 42, .. }
        ));
    }

    #[test]
    fn test_fold_mul_constants() {
        // v0 = 6; v1 = 7; v2 = v0 * v1  →  v2 = 42
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 6,
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 7,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 42, .. }
        ));
    }

    #[test]
    fn test_no_fold_div_by_zero() {
        // v0 = 42; v1 = 0; v2 = v0 / v1  →  no fold
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 42,
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 0,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Div,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(!changed);

        // Should still be a BinOp
        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::BinOp {
                op: BinaryOp::Div,
                ..
            }
        ));
    }

    #[test]
    fn test_immediate_fold_add_rhs_known() {
        // v0 = <param>; v1 = 5; v2 = v0 + v1  →  v2 = addimm v0, 5
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::AddImm { dst, src, imm, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(0));
            assert_eq!(*imm, 5);
        } else {
            panic!("Expected AddImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_immediate_fold_add_lhs_known() {
        // v0 = 5; v2 = v0 + v1  →  v2 = addimm v1, 5
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::AddImm { dst, src, imm, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
            assert_eq!(*imm, 5);
        } else {
            panic!("Expected AddImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_cmp_imm_fold() {
        // v0 = <param>; v1 = 10; cmp v0, v1  →  cmpimm v0, 10
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 10,
                ty: DtalType::Int,
            },
            DtalInstr::Cmp {
                lhs: vreg(0),
                rhs: vreg(1),
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::CmpImm { lhs, imm } = &func.blocks[0].instructions[1] {
            assert_eq!(*lhs, vreg(0));
            assert_eq!(*imm, 10);
        } else {
            panic!("Expected CmpImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_no_fold_non_constant_operands() {
        // v0 = <param>; v1 = <param>; v2 = v0 + v1  →  no change
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(!changed);
    }

    #[test]
    fn test_invalidation_on_redef() {
        // v0 = 10; v0 = <load>; v1 = 5; v2 = v0 + v1  →  no full fold (v0 unknown)
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 10,
                ty: DtalType::Int,
            },
            // v0 is redefined by a non-constant instruction
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(0),
                lhs: vreg(3),
                rhs: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        // Should be AddImm (one operand known), not MovImm (both would need to be known)
        assert!(matches!(
            &func.blocks[0].instructions[3],
            DtalInstr::AddImm { imm: 5, .. }
        ));
    }

    #[test]
    fn test_cascading_fold() {
        // v0 = 2; v1 = 3; v2 = v0 + v1 (→ 5); v3 = 7; v4 = v2 * v3 (→ 35)
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 2,
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 3,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(3),
                imm: 7,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(4),
                lhs: vreg(2),
                rhs: vreg(3),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        // v2 should be folded to 5
        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 5, .. }
        ));
        // v4 should be folded to 35
        assert!(matches!(
            &func.blocks[0].instructions[4],
            DtalInstr::MovImm { imm: 35, .. }
        ));
    }

    #[test]
    fn test_fold_bitwise_ops() {
        // v0 = 0xFF; v1 = 0x0F; v2 = v0 & v1  →  v2 = 0x0F
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 0xFF,
                ty: DtalType::Int,
            },
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 0x0F,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::BitAnd,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 0x0F, .. }
        ));
    }

    #[test]
    fn test_no_immediate_fold_for_sub() {
        // v0 = 5; v2 = v1 - v0  →  stays as BinOp (no SubImm instruction)
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        // Sub with non-zero known RHS should NOT be folded
        assert!(!changed);
    }

    #[test]
    fn test_sub_rhs_zero_identity() {
        // v0 = 0; v2 = v1 - v0  →  v2 = v1
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 0,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
        } else {
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_mul_by_zero() {
        // v0 = 0; v2 = v1 * v0  →  v2 = 0
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 0,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[1],
            DtalInstr::MovImm { imm: 0, .. }
        ));
    }

    #[test]
    fn test_mul_by_one() {
        // v0 = 1; v2 = v1 * v0  →  v2 = v1
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 1,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
        } else {
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_mul_by_one_commutative() {
        // v0 = 1; v2 = v0 * v1  →  v2 = v1
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 1,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
        } else {
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_div_by_one() {
        // v0 = 1; v2 = v1 / v0  →  v2 = v1
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 1,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Div,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
        } else {
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_mod_by_one() {
        // v0 = 1; v2 = v1 % v0  →  v2 = 0
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 1,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mod,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[1],
            DtalInstr::MovImm { imm: 0, .. }
        ));
    }

    #[test]
    fn test_mul_by_power_of_two_to_shl() {
        // v0 = 8; v2 = v1 * v0  →  v2 = shlimm v1, 3
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 8,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::ShlImm { dst, src, imm, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
            assert_eq!(*imm, 3); // log2(8) = 3
        } else {
            panic!("Expected ShlImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_mul_by_power_of_two_commutative() {
        // v0 = 4; v2 = v0 * v1  →  v2 = shlimm v1, 2
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 4,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(0),
                rhs: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::ShlImm { dst, src, imm, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
            assert_eq!(*imm, 2); // log2(4) = 2
        } else {
            panic!("Expected ShlImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_mul_by_non_power_of_two_unchanged() {
        // v0 = 7; v2 = v1 * v0  →  no strength reduction
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 7,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        // No change — 7 is not a power of 2
        assert!(!changed);
    }

    #[test]
    fn test_unsigned_div_by_power_of_two_to_shr() {
        // v0 = 16; v2 = v1 / v0  →  v2 = shrimm v1, 4  (U64 type)
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 16,
                ty: DtalType::U64,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Div,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::U64,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::U64,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        assert!(changed);

        if let DtalInstr::ShrImm { dst, src, imm, .. } = &func.blocks[0].instructions[1] {
            assert_eq!(*dst, vreg(2));
            assert_eq!(*src, vreg(1));
            assert_eq!(*imm, 4); // log2(16) = 4
        } else {
            panic!("Expected ShrImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }

    #[test]
    fn test_signed_div_by_power_of_two_not_reduced() {
        // v0 = 8; v2 = v1 / v0  →  no reduction (signed Int type)
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 8,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Div,
                dst: vreg(2),
                lhs: vreg(1),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = constant_fold_function(&mut func);
        // No change — signed division cannot use shift
        assert!(!changed);
    }
}
