//! Peephole Optimisation Pass
//!
//! Purely structural rewrites on DTAL instructions. Does not track
//! constant values — relies on const_fold running first to expose
//! opportunities (e.g., const_fold turns `BinOp Add dst, x, v_zero`
//! into `AddImm dst, x, 0`, then peephole simplifies to `MovReg`).
//!
//! # Single-instruction rules
//!
//! - `AddImm dst, src, 0` → `MovReg dst, src` (identity)
//! - `BinOp Sub dst, x, x` → `MovImm dst, 0` (self-subtraction)
//! - `BinOp BitXor dst, x, x` → `MovImm dst, 0` (self-xor)
//! - `BinOp BitAnd dst, x, x` → `MovReg dst, x` (idempotent)
//! - `BinOp BitOr dst, x, x` → `MovReg dst, x` (idempotent)
//! - `BinOp And dst, x, x` → `MovReg dst, x` (idempotent)
//! - `BinOp Or dst, x, x` → `MovReg dst, x` (idempotent)

use crate::backend::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr};

/// Apply peephole optimisations to a function
///
/// Returns true if any changes were made
pub fn peephole_function(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        changed |= peephole_block(block);
    }

    changed
}

/// Apply peephole optimisations within a single block
fn peephole_block(block: &mut DtalBlock) -> bool {
    let mut changed = false;

    for instr in &mut block.instructions {
        changed |= try_peephole(instr);
    }

    changed
}

/// Try to apply a peephole rewrite to a single instruction
///
/// Returns true if the instruction was rewritten
fn try_peephole(instr: &mut DtalInstr) -> bool {
    match instr {
        // AddImm with immediate 0 → MovReg (identity)
        DtalInstr::AddImm { dst, src, imm, ty } if *imm == 0 => {
            *instr = DtalInstr::MovReg {
                dst: *dst,
                src: *src,
                ty: ty.clone(),
            };
            true
        }

        // Same-register BinOp patterns
        DtalInstr::BinOp {
            op, dst, lhs, rhs, ty,
        } if lhs == rhs => {
            match op {
                // x - x = 0, x ^ x = 0
                BinaryOp::Sub | BinaryOp::BitXor => {
                    *instr = DtalInstr::MovImm {
                        dst: *dst,
                        imm: 0,
                        ty: ty.clone(),
                    };
                    true
                }
                // x & x = x, x | x = x, x && x = x, x || x = x
                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::And | BinaryOp::Or => {
                    let src = *lhs;
                    *instr = DtalInstr::MovReg {
                        dst: *dst,
                        src,
                        ty: ty.clone(),
                    };
                    true
                }
                _ => false,
            }
        }

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{DtalBlock, DtalFunction, TypeState};
    use crate::backend::dtal::regs::{Reg, VirtualReg};
    use crate::backend::dtal::types::DtalType;

    fn vreg(n: u32) -> Reg {
        Reg::Virtual(VirtualReg(n))
    }

    fn make_func(instructions: Vec<DtalInstr>) -> DtalFunction {
        DtalFunction {
            name: "test".to_string(),
            params: vec![],
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
    fn test_addimm_zero_to_mov() {
        let mut func = make_func(vec![
            DtalInstr::AddImm {
                dst: vreg(1),
                src: vreg(0),
                imm: 0,
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[0] {
            assert_eq!(*dst, vreg(1));
            assert_eq!(*src, vreg(0));
        } else {
            panic!(
                "Expected MovReg, got {:?}",
                &func.blocks[0].instructions[0]
            );
        }
    }

    #[test]
    fn test_addimm_nonzero_unchanged() {
        let mut func = make_func(vec![
            DtalInstr::AddImm {
                dst: vreg(1),
                src: vreg(0),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(!changed);
    }

    #[test]
    fn test_sub_same_reg_to_zero() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::MovImm { imm: 0, .. }
        ));
    }

    #[test]
    fn test_xor_same_reg_to_zero() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::BitXor,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::MovImm { imm: 0, .. }
        ));
    }

    #[test]
    fn test_bitand_same_reg_to_copy() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::BitAnd,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[0] {
            assert_eq!(*dst, vreg(1));
            assert_eq!(*src, vreg(0));
        } else {
            panic!(
                "Expected MovReg, got {:?}",
                &func.blocks[0].instructions[0]
            );
        }
    }

    #[test]
    fn test_bitor_same_reg_to_copy() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::BitOr,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        if let DtalInstr::MovReg { dst, src, .. } = &func.blocks[0].instructions[0] {
            assert_eq!(*dst, vreg(1));
            assert_eq!(*src, vreg(0));
        } else {
            panic!(
                "Expected MovReg, got {:?}",
                &func.blocks[0].instructions[0]
            );
        }
    }

    #[test]
    fn test_logical_and_same_reg_to_copy() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::And,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Bool,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Bool,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::MovReg { .. }
        ));
    }

    #[test]
    fn test_logical_or_same_reg_to_copy() {
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::Or,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Bool,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Bool,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(changed);

        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::MovReg { .. }
        ));
    }

    #[test]
    fn test_different_regs_unchanged() {
        // sub v2, v0, v1 where v0 != v1 — should NOT be rewritten
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
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

        let changed = peephole_function(&mut func);
        assert!(!changed);
    }

    #[test]
    fn test_add_same_reg_unchanged() {
        // add v1, v0, v0 — NOT an identity, should stay (it's x + x = 2x)
        let mut func = make_func(vec![
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(1),
                lhs: vreg(0),
                rhs: vreg(0),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(1),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(!changed);
    }

    #[test]
    fn test_constraint_assert_untouched() {
        use crate::backend::dtal::constraints::{Constraint, IndexExpr};

        let mut func = make_func(vec![
            DtalInstr::ConstraintAssert {
                constraint: Constraint::Gt(IndexExpr::Var("x".to_string()), IndexExpr::Const(0)),
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(!changed);
    }
}
