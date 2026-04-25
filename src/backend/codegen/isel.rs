//! Instruction Selection
//!
//! This module converts TIR instructions to DTAL instructions.
//! At this stage, we use virtual registers (physical register allocation
//! happens in a later phase).

use crate::backend::dtal::constraints::IndexExpr;
use crate::backend::dtal::instr::{BinaryOp as DtalBinaryOp, DtalInstr};
use crate::backend::dtal::regs::{PhysicalReg, Reg};
use crate::backend::dtal::types::DtalType;
use crate::backend::tir::instr::TirInstr;
use crate::backend::tir::types::{BinaryOp as TirBinaryOp, UnaryOp as TirUnaryOp};

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
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Virtual(*dst),
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
            arg_ownerships: _,
            ownership: _,
            result_ty,
        } => {
            lower_call(instrs, dst.as_ref().copied(), func, args, result_ty);
        }

        TirInstr::AllocArray {
            dst,
            element_ty,
            size,
            region,
        } => {
            use crate::backend::dtal::regs::PhysicalReg;
            use std::sync::Arc;

            // Calculate total size in bytes (assuming 8 bytes per element)
            let element_size = 8u32;
            let total_size = element_size * (*size as u32);

            // Create array type — widen element type to base (Int/Bool)
            // since different values will be stored into the array.
            let element_dtal_ty = widen_to_base(DtalType::from_itype(element_ty));
            let array_ty = DtalType::Array {
                element_type: Arc::new(element_dtal_ty),
                size: IndexExpr::Const(*size as i128),
            };

            if bare_metal {
                // Bare-metal: stack alloc (no kernel mmap available).
                // A bump allocator for unikernels is planned separately.
                instrs.push(DtalInstr::Alloca {
                    dst: Reg::Virtual(*dst),
                    size: total_size,
                    ty: array_ty,
                });
            } else {
                // Hosted Linux: allocate from the current function-local region.
                // Pass region in r0 and size in r1; result returns in r0.
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
                    return_ty: DtalType::Int,
                });
                instrs.push(DtalInstr::MovReg {
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
                    return_ty: DtalType::Int,
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
                    return_ty: DtalType::Unit,
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
    dst: crate::backend::dtal::VirtualReg,
    op: TirBinaryOp,
    lhs: crate::backend::dtal::VirtualReg,
    rhs: crate::backend::dtal::VirtualReg,
    ty: &crate::common::types::IType<'src>,
) {
    let dtal_ty = DtalType::from_itype(ty);
    match op {
        // Arithmetic operations map directly
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

        // Comparison operations
        TirBinaryOp::Eq => lower_comparison(instrs, dst, lhs, rhs, "eq"),
        TirBinaryOp::Ne => lower_comparison(instrs, dst, lhs, rhs, "ne"),
        TirBinaryOp::Lt => lower_comparison(instrs, dst, lhs, rhs, "lt"),
        TirBinaryOp::Le => lower_comparison(instrs, dst, lhs, rhs, "le"),
        TirBinaryOp::Gt => lower_comparison(instrs, dst, lhs, rhs, "gt"),
        TirBinaryOp::Ge => lower_comparison(instrs, dst, lhs, rhs, "ge"),

        // Bitwise operations (integer operands, integer result)
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

        // Logical operations
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
    dst: crate::backend::dtal::VirtualReg,
    lhs: crate::backend::dtal::VirtualReg,
    rhs: crate::backend::dtal::VirtualReg,
    cmp_kind: &str,
) {
    use crate::backend::dtal::instr::CmpOp;

    // Emit comparison to set CPU flags
    instrs.push(DtalInstr::Cmp {
        lhs: Reg::Virtual(lhs),
        rhs: Reg::Virtual(rhs),
    });

    // Map comparison kind to CmpOp
    let cond = match cmp_kind {
        "eq" => CmpOp::Eq,
        "ne" => CmpOp::Ne,
        "lt" => CmpOp::Lt,
        "le" => CmpOp::Le,
        "gt" => CmpOp::Gt,
        "ge" => CmpOp::Ge,
        _ => panic!("Unknown comparison kind: {}", cmp_kind),
    };

    // Emit SetCC to materialize comparison result (0 or 1)
    instrs.push(DtalInstr::SetCC {
        dst: Reg::Virtual(dst),
        cond,
    });

    // Type annotation for verification
    instrs.push(DtalInstr::TypeAnnotation {
        reg: Reg::Virtual(dst),
        ty: DtalType::Bool,
    });
}

/// Lower a unary operation
fn lower_unaryop<'src>(
    instrs: &mut Vec<DtalInstr>,
    dst: crate::backend::dtal::VirtualReg,
    op: TirUnaryOp,
    operand: crate::backend::dtal::VirtualReg,
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

/// Lower a function call
fn lower_call<'src>(
    instrs: &mut Vec<DtalInstr>,
    dst: Option<crate::backend::dtal::VirtualReg>,
    func: &str,
    args: &[crate::backend::dtal::VirtualReg],
    result_ty: &crate::common::types::IType<'src>,
) {
    use crate::backend::dtal::regs::PhysicalReg;

    let dtal_result_ty = DtalType::from_itype(result_ty);

    // Move arguments to parameter registers (r0, r1, r2, ...)
    for (i, arg) in args.iter().enumerate() {
        if i < 8 {
            // Use physical parameter registers
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
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Physical(param_reg),
                src: Reg::Virtual(*arg),
                ty: DtalType::Int, // TODO: track actual argument types
            });
        } else {
            // Push extra arguments onto stack (not implemented yet)
            instrs.push(DtalInstr::Push {
                src: Reg::Virtual(*arg),
                ty: DtalType::Int,
            });
        }
    }

    // Emit call instruction
    instrs.push(DtalInstr::Call {
        target: func.to_string(),
        return_ty: dtal_result_ty.clone(),
    });

    // Move result from r0 to destination
    if let Some(dst_reg) = dst {
        instrs.push(DtalInstr::MovReg {
            dst: Reg::Virtual(dst_reg),
            src: Reg::Physical(PhysicalReg::R0),
            ty: dtal_result_ty,
        });
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
