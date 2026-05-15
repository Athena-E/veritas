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

use crate::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr};
use crate::dtal::regs::{Reg, VirtualReg};
use crate::dtal::types::DtalType;
use std::collections::HashMap;

type ConstMap = HashMap<VirtualReg, i128>;

pub fn constant_fold_function(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        changed |= constant_fold_block(block);
    }

    changed
}

fn constant_fold_block(block: &mut DtalBlock) -> bool {
    let mut changed = false;
    let mut const_map: ConstMap = HashMap::new();

    for instr in &mut block.instructions {
        changed |= try_fold(instr, &const_map);
        update_const_map(instr, &mut const_map);
    }

    changed
}

fn try_fold(instr: &mut DtalInstr, const_map: &ConstMap) -> bool {
    match instr {
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

                (None, Some(0)) if *op == BinaryOp::Sub => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }

                (Some(0), None) | (None, Some(0)) if *op == BinaryOp::Mul => {
                    *instr = DtalInstr::MovImm {
                        dst: *dst,
                        imm: 0,
                        ty: ty.clone(),
                    };
                    true
                }
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

                (None, Some(1)) if *op == BinaryOp::Div => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }

                (None, Some(1)) if *op == BinaryOp::Mod => {
                    *instr = DtalInstr::MovImm {
                        dst: *dst,
                        imm: 0,
                        ty: ty.clone(),
                    };
                    true
                }

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

fn lookup(reg: &Reg, const_map: &ConstMap) -> Option<i128> {
    match reg {
        Reg::Virtual(vreg) => const_map.get(vreg).copied(),
        Reg::Physical(_) => None,
    }
}

fn is_power_of_two(val: i128) -> bool {
    val > 1 && (val & (val - 1)) == 0
}

fn is_unsigned(ty: &DtalType) -> bool {
    matches!(ty, DtalType::U64)
}

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

fn update_const_map(instr: &DtalInstr, const_map: &mut ConstMap) {
    match instr {
        DtalInstr::MovImm {
            dst: Reg::Virtual(vreg),
            imm,
            ..
        } => {
            const_map.insert(*vreg, *imm);
        }

        _ => {
            if let Some(Reg::Virtual(vreg)) = instruction_dst(instr) {
                const_map.remove(&vreg);
            }
        }
    }
}

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
    use crate::dtal::instr::{DtalBlock, DtalFunction, TypeState};
    use crate::dtal::types::DtalType;

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

        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 42, .. }
        ));
    }
    #[test]

    fn test_fold_mul_constants() {
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
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(0),
                imm: 10,
                ty: DtalType::Int,
            },
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

        assert!(matches!(
            &func.blocks[0].instructions[3],
            DtalInstr::AddImm { imm: 5, .. }
        ));
    }
    #[test]

    fn test_cascading_fold() {
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

        assert!(matches!(
            &func.blocks[0].instructions[2],
            DtalInstr::MovImm { imm: 5, .. }
        ));
        assert!(matches!(
            &func.blocks[0].instructions[4],
            DtalInstr::MovImm { imm: 35, .. }
        ));
    }
    #[test]

    fn test_fold_bitwise_ops() {
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
        assert!(!changed);
    }
    #[test]

    fn test_sub_rhs_zero_identity() {
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
            assert_eq!(*imm, 3);
        } else {
            panic!("Expected ShlImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }
    #[test]

    fn test_mul_by_power_of_two_commutative() {
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
            assert_eq!(*imm, 2);
        } else {
            panic!("Expected ShlImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }
    #[test]

    fn test_mul_by_non_power_of_two_unchanged() {
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
        assert!(!changed);
    }
    #[test]

    fn test_unsigned_div_by_power_of_two_to_shr() {
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
            assert_eq!(*imm, 4);
        } else {
            panic!("Expected ShrImm, got {:?}", &func.blocks[0].instructions[1]);
        }
    }
    #[test]

    fn test_signed_div_by_power_of_two_not_reduced() {
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
        assert!(!changed);
    }
}
