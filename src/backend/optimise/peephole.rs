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

use crate::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr};

pub fn peephole_function(func: &mut DtalFunction) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        changed |= peephole_block(block);
    }

    changed
}

fn peephole_block(block: &mut DtalBlock) -> bool {
    let mut changed = false;

    let mut remove = vec![false; block.instructions.len()];
    let len = block.instructions.len();
    #[allow(clippy::needless_range_loop)]
    if len >= 2 {
        for i in 0..len - 1 {
            if let Some(replacement) =
                try_peephole_pair(&block.instructions[i], &block.instructions[i + 1])
            {
                block.instructions[i + 1] = replacement;
                remove[i] = true;
                changed = true;
            }
        }
    }
    if changed {
        let mut idx = 0;
        block.instructions.retain(|_| {
            let keep = !remove[idx];
            idx += 1;
            keep
        });
    }

    for instr in &mut block.instructions {
        changed |= try_peephole(instr);
    }

    changed
}

fn try_peephole_pair(first: &DtalInstr, second: &DtalInstr) -> Option<DtalInstr> {
    if let (
        DtalInstr::MovImm {
            dst: d1, imm: 0, ..
        },
        DtalInstr::BinOp {
            op: BinaryOp::Sub,
            dst: d2,
            lhs: d3,
            rhs: operand,
            ty,
        },
    ) = (first, second)
        && d1 == d2
        && d2 == d3
    {
        return Some(DtalInstr::Neg {
            dst: *d2,
            src: *operand,
            ty: ty.clone(),
        });
    }

    None
}

fn try_peephole(instr: &mut DtalInstr) -> bool {
    match instr {
        DtalInstr::ShlImm {
            dst,
            src,
            imm: 0,
            ty,
        }
        | DtalInstr::ShrImm {
            dst,
            src,
            imm: 0,
            ty,
        } => {
            *instr = DtalInstr::MovReg {
                dst: *dst,
                src: *src,
                ty: ty.clone(),
            };
            true
        }

        DtalInstr::AddImm { dst, src, imm, ty } if *imm == 0 => {
            *instr = DtalInstr::MovReg {
                dst: *dst,
                src: *src,
                ty: ty.clone(),
            };
            true
        }

        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } if lhs == rhs => match op {
            BinaryOp::Sub | BinaryOp::BitXor => {
                *instr = DtalInstr::MovImm {
                    dst: *dst,
                    imm: 0,
                    ty: ty.clone(),
                };
                true
            }
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
        },

        _ => false,
    }
}
#[cfg(test)]

mod tests {
    use super::*;
    use crate::dtal::instr::{DtalBlock, DtalFunction, TypeState};
    use crate::dtal::regs::{Reg, VirtualReg};
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
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[0]);
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
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[0]);
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
            panic!("Expected MovReg, got {:?}", &func.blocks[0].instructions[0]);
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
        use crate::dtal::constraints::{Constraint, IndexExpr};

        let mut func = make_func(vec![
            DtalInstr::ConstraintAssert {
                constraint: Constraint::Gt(IndexExpr::Var("x".to_string()), IndexExpr::Const(0)),
            },
            DtalInstr::Ret,
        ]);

        let changed = peephole_function(&mut func);
        assert!(!changed);
    }
    #[test]

    fn test_negation_fusion() {
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 0,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(1),
                lhs: vreg(1),
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

        assert_eq!(func.blocks[0].instructions.len(), 3);

        if let DtalInstr::Neg { dst, src, .. } = &func.blocks[0].instructions[0] {
            assert_eq!(*dst, vreg(1));
            assert_eq!(*src, vreg(0));
        } else {
            panic!("Expected Neg, got {:?}", &func.blocks[0].instructions[0]);
        }
    }
    #[test]

    fn test_negation_no_fusion_nonzero_imm() {
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(1),
                imm: 5,
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(1),
                lhs: vreg(1),
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
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }
    #[test]

    fn test_negation_no_fusion_different_regs() {
        let mut func = make_func(vec![
            DtalInstr::MovImm {
                dst: vreg(1),
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

        let changed = peephole_function(&mut func);
        assert!(!changed);
        assert_eq!(func.blocks[0].instructions.len(), 4);
    }
}
