//! Load-Op Fusion Pass
//!
//! Fuses `Load dst, base, offset; BinOp Add result, dst, other` into a
//! single `LoadOp` instruction, which lowers to x86 `AddRM`
//! (add with memory operand).
//!
//! # Pattern
//!
//! ```text
//! Load dst1, base, offset
//! (optional TypeAnnotation dst1, _)
//! BinOp Add, dst2, lhs, rhs   where lhs == dst1
//! ```
//!
//! Becomes:
//!
//! ```text
//! LoadOp { op, dst: dst2, base, offset, other: rhs, ty }
//! ```
//!
//! # Conditions
//!
//! - `dst1` is used exactly once in the function (by the BinOp)
//! - `rhs != dst1` (avoid self-use like `arr[i] + arr[i]`)
//! - No `Store`/`Call` between Load and BinOp (aliasing)
//! - No instruction between them redefines `base`, `offset`, or `rhs`

use crate::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr};
use crate::dtal::regs::{Reg, VirtualReg};
use std::collections::HashMap;

pub fn fuse_loads_function(func: &mut DtalFunction) -> bool {
    let use_counts = count_uses(func);

    let mut changed = false;
    for block in &mut func.blocks {
        changed |= fuse_loads_block(block, &use_counts);
    }
    changed
}

fn count_uses(func: &DtalFunction) -> HashMap<VirtualReg, u32> {
    let mut counts: HashMap<VirtualReg, u32> = HashMap::new();
    for block in &func.blocks {
        for instr in &block.instructions {
            for vreg in instruction_uses(instr) {
                *counts.entry(vreg).or_insert(0) += 1;
            }
        }
    }
    counts
}

fn fuse_loads_block(block: &mut DtalBlock, use_counts: &HashMap<VirtualReg, u32>) -> bool {
    let mut changed = false;
    let mut remove = vec![false; block.instructions.len()];

    let n = block.instructions.len();
    let mut i = 0;
    while i < n {
        if remove[i] {
            i += 1;
            continue;
        }

        if let DtalInstr::Load {
            dst: load_dst,
            base,
            offset,
            ty,
        } = &block.instructions[i]
        {
            let load_dst = *load_dst;
            let base = *base;
            let offset = *offset;
            let _ = ty;

            let load_dst_vreg = match load_dst {
                Reg::Virtual(v) => v,
                Reg::Physical(_) => {
                    i += 1;
                    continue;
                }
            };
            if use_counts.get(&load_dst_vreg).copied().unwrap_or(0) != 1 {
                i += 1;
                continue;
            }

            let mut j = i + 1;
            let mut safe_to_fuse = true;
            let fused_instr = loop {
                if j >= n || remove[j] {
                    safe_to_fuse = false;
                    break None;
                }

                let next = &block.instructions[j];

                if let DtalInstr::BinOp {
                    op,
                    dst: binop_dst,
                    lhs,
                    rhs,
                    ty: binop_ty,
                } = next
                {
                    if *op == BinaryOp::Add && *lhs == load_dst {
                        if *rhs == load_dst {
                            safe_to_fuse = false;
                            break None;
                        }
                        break Some(DtalInstr::LoadOp {
                            op: *op,
                            dst: *binop_dst,
                            base,
                            offset,
                            other: *rhs,
                            ty: binop_ty.clone(),
                        });
                    }
                    safe_to_fuse = false;
                    break None;
                }

                match next {
                    DtalInstr::TypeAnnotation { reg, .. } => {
                        if *reg != load_dst {}
                        j += 1;
                        continue;
                    }
                    DtalInstr::ConstraintAssert { .. } => {
                        j += 1;
                        continue;
                    }
                    DtalInstr::Store { .. }
                    | DtalInstr::Call { .. }
                    | DtalInstr::Load { .. }
                    | DtalInstr::LoadOp { .. }
                    | DtalInstr::Jmp { .. }
                    | DtalInstr::Branch { .. }
                    | DtalInstr::Ret
                    | DtalInstr::Push { .. }
                    | DtalInstr::Pop { .. } => {
                        safe_to_fuse = false;
                        break None;
                    }
                    _ => {
                        if let Some(def) = instruction_def(next) {
                            let def_reg = Reg::Virtual(def);
                            if def_reg == base || def_reg == offset {
                                safe_to_fuse = false;
                                break None;
                            }
                        }
                        j += 1;
                        continue;
                    }
                }
            };

            if safe_to_fuse && let Some(fused) = fused_instr {
                if let DtalInstr::LoadOp { other, .. } = &fused {
                    let other_reg = *other;
                    let mut other_redefined = false;
                    for k in (i + 1)..j {
                        if let Some(def) = instruction_def(&block.instructions[k])
                            && Reg::Virtual(def) == other_reg
                        {
                            other_redefined = true;
                            break;
                        }
                    }
                    if other_redefined {
                        i += 1;
                        continue;
                    }
                }

                block.instructions[i] = fused;
                remove[j] = true;
                #[allow(clippy::needless_range_loop)]
                for k in (i + 1)..j {
                    if let DtalInstr::TypeAnnotation { reg, .. } = &block.instructions[k]
                        && *reg == load_dst
                    {
                        remove[k] = true;
                    }
                }

                changed = true;
                i = j + 1;
                continue;
            }

            let _ = safe_to_fuse;
        }

        i += 1;
    }

    if changed {
        let mut idx = 0;
        block.instructions.retain(|_| {
            let keep = !remove[idx];
            idx += 1;
            keep
        });
    }

    changed
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

fn instruction_uses(instr: &DtalInstr) -> Vec<VirtualReg> {
    let regs: Vec<Reg> = match instr {
        DtalInstr::MovReg { src, .. } => vec![*src],
        DtalInstr::Load { base, offset, .. } => vec![*base, *offset],
        DtalInstr::LoadOp {
            base,
            offset,
            other,
            ..
        } => vec![*base, *offset, *other],
        DtalInstr::Store { base, offset, src } => vec![*base, *offset, *src],
        DtalInstr::BinOp { lhs, rhs, .. } => vec![*lhs, *rhs],
        DtalInstr::AddImm { src, .. } => vec![*src],
        DtalInstr::ShlImm { src, .. } | DtalInstr::ShrImm { src, .. } => vec![*src],
        DtalInstr::Cmp { lhs, rhs } => vec![*lhs, *rhs],
        DtalInstr::CmpImm { lhs, .. } => vec![*lhs],
        DtalInstr::Not { src, .. } | DtalInstr::Neg { src, .. } => vec![*src],
        DtalInstr::Push { src, .. } => vec![*src],
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
    use crate::dtal::instr::{DtalBlock, DtalFunction, TypeState};
    use crate::dtal::regs::VirtualReg;
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

    fn test_basic_load_add_fusion() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(changed);

        assert_eq!(func.blocks[0].instructions.len(), 3);
        if let DtalInstr::LoadOp {
            op,
            dst,
            base,
            offset,
            other,
            ..
        } = &func.blocks[0].instructions[0]
        {
            assert_eq!(*op, BinaryOp::Add);
            assert_eq!(*dst, vreg(4));
            assert_eq!(*base, vreg(1));
            assert_eq!(*offset, vreg(2));
            assert_eq!(*other, vreg(5));
        } else {
            panic!("Expected LoadOp, got {:?}", &func.blocks[0].instructions[0]);
        }
    }
    #[test]

    fn test_no_fusion_when_sub_load_is_lhs() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Sub,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(!changed);

        assert!(matches!(
            &func.blocks[0].instructions[0],
            DtalInstr::Load { .. }
        ));
    }
    #[test]

    fn test_no_fusion_when_mul() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Mul,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(!changed);
    }
    #[test]

    fn test_no_fusion_when_load_dst_used_twice() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(3),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(!changed, "Should not fuse when load dst has multiple uses");
    }
    #[test]

    fn test_no_fusion_when_self_use() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(3),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(!changed);
    }
    #[test]

    fn test_no_fusion_store_between() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::Store {
                base: vreg(10),
                offset: vreg(11),
                src: vreg(12),
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(!changed, "Should not fuse across a Store");
    }
    #[test]

    fn test_fusion_skips_type_annotation() {
        let mut func = make_func(vec![
            DtalInstr::Load {
                dst: vreg(3),
                base: vreg(1),
                offset: vreg(2),
                ty: DtalType::Int,
            },
            DtalInstr::TypeAnnotation {
                reg: vreg(3),
                ty: DtalType::Int,
            },
            DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: vreg(4),
                lhs: vreg(3),
                rhs: vreg(5),
                ty: DtalType::Int,
            },
            DtalInstr::Push {
                src: vreg(4),
                ty: DtalType::Int,
            },
            DtalInstr::Ret,
        ]);

        let changed = fuse_loads_function(&mut func);
        assert!(changed);

        let has_type_anno = func.blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, DtalInstr::TypeAnnotation { reg, .. } if *reg == vreg(3)));
        assert!(!has_type_anno);
    }
}
