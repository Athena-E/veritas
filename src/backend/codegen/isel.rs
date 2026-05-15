//! Instruction Selection
//!
//! This module converts TIR instructions to DTAL instructions.
//! At this stage, we use virtual registers (physical register allocation
//! happens in a later phase).

use crate::common::ownership::{OwnershipMode, ParameterKind};
use crate::dtal::constraints::IndexExpr;
use crate::dtal::instr::{BinaryOp as DtalBinaryOp, DtalInstr};
use crate::dtal::regs::{PhysicalReg, Reg};
use crate::dtal::types::DtalType;
use crate::middle::tir::instr::TirInstr;
use crate::middle::tir::types::{BinaryOp as TirBinaryOp, UnaryOp as TirUnaryOp};

/// Lower a TIR instruction to DTAL instructions
///
/// May emit multiple DTAL instructions for a single TIR instruction.
/// `bare_metal` selects between hosted function-local region allocation and
/// stack allocation (bare-metal, via `Alloca`) for arrays.
pub fn lower_instruction<'src>(
    instrs: &mut Vec<DtalInstr>,
    tir_instr: &TirInstr<'src>,
    bare_metal: bool,
) {
    match tir_instr {
        TirInstr::LoadImm { dst, value, ty } => {
            instrs.push(DtalInstr::MovImm {
                dst: Reg::Virtual(*dst),
                imm: *value as i128,
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::Copy { dst, src, ty } => {
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Virtual(*dst),
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::MoveOwned { dst, src, ty } => {
            instrs.push(DtalInstr::MoveOwned {
                dst: Reg::Virtual(*dst),
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::DropOwned { src, ty } => {
            instrs.push(DtalInstr::DropOwned {
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::BorrowShared {
            lifetime,
            dst,
            src,
            ty,
        } => {
            instrs.push(DtalInstr::AliasBorrow {
                lifetime: *lifetime,
                dst: Reg::Virtual(*dst),
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::BorrowMut {
            lifetime,
            dst,
            src,
            ty,
        } => {
            instrs.push(DtalInstr::BorrowMut {
                lifetime: *lifetime,
                dst: Reg::Virtual(*dst),
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::BorrowEnd { lifetime, src, ty } => {
            instrs.push(DtalInstr::BorrowEnd {
                lifetime: *lifetime,
                src: Reg::Virtual(*src),
                ty: DtalType::from_itype(ty),
            });
        }

        TirInstr::BinOp {
            dst,
            op,
            lhs,
            rhs,
            ty,
        } => {
            lower_binop(instrs, *dst, *op, *lhs, *rhs, ty);
        }

        TirInstr::UnaryOp {
            dst,
            op,
            operand,
            ty,
        } => {
            lower_unaryop(instrs, *dst, *op, *operand, ty);
        }

        TirInstr::ArrayLoad {
            dst,
            base,
            index,
            element_ty,
            bounds_constraint,
        } => {
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: bounds_constraint.clone(),
            });
            instrs.push(DtalInstr::Load {
                dst: Reg::Virtual(*dst),
                base: Reg::Virtual(*base),
                offset: Reg::Virtual(*index),
                ty: DtalType::from_itype(element_ty),
            });
        }

        TirInstr::ArrayStore {
            base,
            index,
            value,
            bounds_constraint,
        } => {
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: bounds_constraint.clone(),
            });
            instrs.push(DtalInstr::Store {
                base: Reg::Virtual(*base),
                offset: Reg::Virtual(*index),
                src: Reg::Virtual(*value),
            });
        }

        TirInstr::Call {
            dst,
            func,
            args,
            arg_types,
            arg_kinds,
            ownership,
            result_ty,
        } => {
            lower_call(
                instrs,
                LowerCall {
                    dst: dst.as_ref().copied(),
                    func,
                    args,
                    arg_types,
                    arg_kinds,
                    ownership: *ownership,
                    result_ty,
                },
            );
        }

        TirInstr::AllocArray {
            dst,
            element_ty,
            size,
            region,
        } => {
            use crate::dtal::regs::PhysicalReg;
            use std::sync::Arc;

            let element_size = 8u32;
            let total_size = element_size * (*size as u32);

            // Arrays store mutable elements, so singleton refinements are widened.
            let element_dtal_ty = widen_to_base(DtalType::from_itype(element_ty));
            let array_ty = DtalType::Array {
                element_type: Arc::new(element_dtal_ty),
                size: IndexExpr::Const(*size as i128),
            };

            if bare_metal {
                // Bare-metal has no hosted allocator.
                instrs.push(DtalInstr::Alloca {
                    dst: Reg::Virtual(*dst),
                    size: total_size,
                    ty: array_ty,
                });
            } else {
                // Runtime ABI: region in r0, size in r1, result in r0.
                instrs.push(DtalInstr::MovImm {
                    dst: Reg::Physical(PhysicalReg::R1),
                    imm: total_size as i128,
                    ty: DtalType::Int,
                });
                let region_src = region
                    .map(Reg::Virtual)
                    .unwrap_or(Reg::Physical(PhysicalReg::R12));
                instrs.push(DtalInstr::MovReg {
                    dst: Reg::Physical(PhysicalReg::R0),
                    src: region_src,
                    ty: DtalType::Int,
                });
                instrs.push(DtalInstr::Call {
                    target: crate::backend::runtime::RT_REGION_ALLOC.to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: OwnershipMode::FreshOwned,
                });
                instrs.push(DtalInstr::MoveOwned {
                    dst: Reg::Virtual(*dst),
                    src: Reg::Physical(PhysicalReg::R0),
                    ty: array_ty.clone(),
                });
                instrs.push(DtalInstr::TypeAnnotation {
                    reg: Reg::Virtual(*dst),
                    ty: array_ty,
                });
            }
        }

        TirInstr::RegionEnter { dst } => {
            if !bare_metal {
                instrs.push(DtalInstr::Call {
                    target: crate::backend::runtime::RT_REGION_ENTER.to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: OwnershipMode::Plain,
                });
                instrs.push(DtalInstr::MovReg {
                    dst: Reg::Virtual(*dst),
                    src: Reg::Physical(PhysicalReg::R0),
                    ty: DtalType::Int,
                });
            }
        }

        TirInstr::RegionLeave { region } => {
            if !bare_metal {
                instrs.push(DtalInstr::MovReg {
                    dst: Reg::Physical(PhysicalReg::R0),
                    src: Reg::Virtual(*region),
                    ty: DtalType::Int,
                });
                instrs.push(DtalInstr::Call {
                    target: crate::backend::runtime::RT_REGION_LEAVE.to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Unit,
                    ownership: OwnershipMode::Plain,
                });
            }
        }

        // AssumeConstraint: the verifier derives constraints independently
        // from branch conditions and existential types — no need to emit.
        TirInstr::AssumeConstraint { .. } => {}

        TirInstr::AssertConstraint { constraint } => {
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: constraint.clone(),
            });
        }
    }
}

/// Lower a binary operation
fn lower_binop<'src>(
    instrs: &mut Vec<DtalInstr>,
    dst: crate::dtal::VirtualReg,
    op: TirBinaryOp,
    lhs: crate::dtal::VirtualReg,
    rhs: crate::dtal::VirtualReg,
    ty: &crate::common::types::IType<'src>,
) {
    let dtal_ty = DtalType::from_itype(ty);
    match op {
        TirBinaryOp::Add => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Add,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Sub => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Sub,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Mul => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Mul,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Div => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Div,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Mod => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Mod,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }

        TirBinaryOp::Eq => lower_comparison(instrs, dst, lhs, rhs, "eq"),
        TirBinaryOp::Ne => lower_comparison(instrs, dst, lhs, rhs, "ne"),
        TirBinaryOp::Lt => lower_comparison(instrs, dst, lhs, rhs, "lt"),
        TirBinaryOp::Le => lower_comparison(instrs, dst, lhs, rhs, "le"),
        TirBinaryOp::Gt => lower_comparison(instrs, dst, lhs, rhs, "gt"),
        TirBinaryOp::Ge => lower_comparison(instrs, dst, lhs, rhs, "ge"),

        TirBinaryOp::BitAnd => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::BitAnd,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::BitOr => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::BitOr,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::BitXor => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::BitXor,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Shl => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Shl,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Shr => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Shr,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }

        TirBinaryOp::And => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::And,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
        TirBinaryOp::Or => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Or,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: dtal_ty,
            });
        }
    }
}

/// Lower a comparison operation
fn lower_comparison(
    instrs: &mut Vec<DtalInstr>,
    dst: crate::dtal::VirtualReg,
    lhs: crate::dtal::VirtualReg,
    rhs: crate::dtal::VirtualReg,
    cmp_kind: &str,
) {
    use crate::dtal::instr::CmpOp;

    instrs.push(DtalInstr::Cmp {
        lhs: Reg::Virtual(lhs),
        rhs: Reg::Virtual(rhs),
    });

    let cond = match cmp_kind {
        "eq" => CmpOp::Eq,
        "ne" => CmpOp::Ne,
        "lt" => CmpOp::Lt,
        "le" => CmpOp::Le,
        "gt" => CmpOp::Gt,
        "ge" => CmpOp::Ge,
        _ => panic!("Unknown comparison kind: {}", cmp_kind),
    };

    instrs.push(DtalInstr::SetCC {
        dst: Reg::Virtual(dst),
        cond,
    });

    instrs.push(DtalInstr::TypeAnnotation {
        reg: Reg::Virtual(dst),
        ty: DtalType::Bool,
    });
}

/// Lower a unary operation
fn lower_unaryop<'src>(
    instrs: &mut Vec<DtalInstr>,
    dst: crate::dtal::VirtualReg,
    op: TirUnaryOp,
    operand: crate::dtal::VirtualReg,
    ty: &crate::common::types::IType<'src>,
) {
    let dtal_ty = DtalType::from_itype(ty);
    match op {
        TirUnaryOp::Not => {
            instrs.push(DtalInstr::Not {
                dst: Reg::Virtual(dst),
                src: Reg::Virtual(operand),
                ty: dtal_ty,
            });
        }
        TirUnaryOp::Neg => {
            instrs.push(DtalInstr::MovImm {
                dst: Reg::Virtual(dst),
                imm: 0,
                ty: dtal_ty.clone(),
            });
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Sub,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(dst),
                rhs: Reg::Virtual(operand),
                ty: dtal_ty,
            });
        }
    }
}

struct LowerCall<'a, 'src> {
    dst: Option<crate::dtal::VirtualReg>,
    func: &'a str,
    args: &'a [crate::dtal::VirtualReg],
    arg_types: &'a [crate::common::types::IType<'src>],
    arg_kinds: &'a [ParameterKind],
    ownership: OwnershipMode,
    result_ty: &'a crate::common::types::IType<'src>,
}

/// Lower a function call
fn lower_call<'src>(instrs: &mut Vec<DtalInstr>, call: LowerCall<'_, 'src>) {
    use crate::dtal::regs::PhysicalReg;

    let LowerCall {
        dst,
        func,
        args,
        arg_types,
        arg_kinds,
        ownership,
        result_ty,
    } = call;

    let dtal_result_ty = DtalType::from_itype(result_ty);

    for (i, ((arg, arg_ty), arg_kind)) in args
        .iter()
        .zip(arg_types.iter())
        .zip(arg_kinds.iter())
        .enumerate()
    {
        if i < 8 {
            let param_reg = match i {
                0 => PhysicalReg::R0,
                1 => PhysicalReg::R1,
                2 => PhysicalReg::R2,
                3 => PhysicalReg::R3,
                4 => PhysicalReg::R4,
                5 => PhysicalReg::R5,
                6 => PhysicalReg::R6,
                7 => PhysicalReg::R7,
                _ => unreachable!(),
            };
            if arg_kind.is_plain_value() {
                instrs.push(DtalInstr::MovReg {
                    dst: Reg::Physical(param_reg),
                    src: Reg::Virtual(*arg),
                    ty: DtalType::Int,
                });
            } else if arg_kind.is_owned_value() {
                instrs.push(DtalInstr::MoveOwned {
                    dst: Reg::Physical(param_reg),
                    src: Reg::Virtual(*arg),
                    ty: DtalType::Int,
                });
            } else if arg_kind.is_shared_borrow() {
                if matches!(arg_ty, crate::common::types::IType::Ref(_)) {
                    instrs.push(DtalInstr::MovReg {
                        dst: Reg::Physical(param_reg),
                        src: Reg::Virtual(*arg),
                        ty: DtalType::Int,
                    });
                    continue;
                }
                instrs.push(DtalInstr::AliasBorrow {
                    lifetime: None,
                    dst: Reg::Physical(param_reg),
                    src: Reg::Virtual(*arg),
                    ty: DtalType::Int,
                });
            } else {
                if matches!(arg_ty, crate::common::types::IType::RefMut(_)) {
                    instrs.push(DtalInstr::MovReg {
                        dst: Reg::Physical(param_reg),
                        src: Reg::Virtual(*arg),
                        ty: DtalType::Int,
                    });
                    continue;
                }
                instrs.push(DtalInstr::BorrowMut {
                    lifetime: None,
                    dst: Reg::Physical(param_reg),
                    src: Reg::Virtual(*arg),
                    ty: DtalType::Int,
                });
            }
        } else {
            instrs.push(DtalInstr::Push {
                src: Reg::Virtual(*arg),
                ty: DtalType::Int,
            });
        }
    }

    instrs.push(DtalInstr::Call {
        target: func.to_string(),
        arg_kinds: arg_kinds.to_vec(),
        return_ty: dtal_result_ty.clone(),
        ownership,
    });

    if let Some(dst_reg) = dst {
        if ownership.produces_owned_output() {
            instrs.push(DtalInstr::MoveOwned {
                dst: Reg::Virtual(dst_reg),
                src: Reg::Physical(PhysicalReg::R0),
                ty: dtal_result_ty,
            });
        } else {
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Virtual(dst_reg),
                src: Reg::Physical(PhysicalReg::R0),
                ty: dtal_result_ty,
            });
        }
    }
}

/// Widen a type to its base form for array element types.
///
/// `SingletonInt(n)` → `Int`. Refined types are preserved since they
/// carry meaningful constraints (e.g., `{v: int | v > 0}` for positive arrays).
/// Other types are returned unchanged.
fn widen_to_base(ty: DtalType) -> DtalType {
    match ty {
        DtalType::SingletonInt(_) => DtalType::Int,
        other => other,
    }
}
