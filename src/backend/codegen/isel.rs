//! Instruction Selection
//!
//! This module converts TIR instructions to DTAL instructions.
//! At this stage, we use virtual registers (physical register allocation
//! happens in a later phase).

use crate::backend::dtal::instr::{BinaryOp as DtalBinaryOp, DtalInstr};
use crate::backend::dtal::regs::Reg;
use crate::backend::tir::instr::TirInstr;
use crate::backend::tir::types::{BinaryOp as TirBinaryOp, UnaryOp as TirUnaryOp};
use crate::common::types::IType;

/// Lower a TIR instruction to DTAL instructions
///
/// May emit multiple DTAL instructions for a single TIR instruction.
pub fn lower_instruction<'src>(instrs: &mut Vec<DtalInstr<'src>>, tir_instr: &TirInstr<'src>) {
    match tir_instr {
        TirInstr::LoadImm { dst, value, ty } => {
            instrs.push(DtalInstr::MovImm {
                dst: Reg::Virtual(*dst),
                imm: *value,
                ty: ty.clone(),
            });
        }

        TirInstr::Copy { dst, src, ty } => {
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Virtual(*dst),
                src: Reg::Virtual(*src),
                ty: ty.clone(),
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
            bounds_proof,
        } => {
            // Emit bounds proof as a constraint assertion
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: bounds_proof.constraint.clone(),
                msg: format!("array bounds check: {:?}", bounds_proof.justification),
            });

            // Emit load instruction
            instrs.push(DtalInstr::Load {
                dst: Reg::Virtual(*dst),
                base: Reg::Virtual(*base),
                offset: Reg::Virtual(*index),
                ty: element_ty.clone(),
            });
        }

        TirInstr::ArrayStore {
            base,
            index,
            value,
            bounds_proof,
        } => {
            // Emit bounds proof as a constraint assertion
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: bounds_proof.constraint.clone(),
                msg: format!("array bounds check: {:?}", bounds_proof.justification),
            });

            // Emit store instruction
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
            result_ty,
        } => {
            lower_call(instrs, dst.as_ref().copied(), func, args, result_ty);
        }

        TirInstr::AllocArray {
            dst,
            element_ty,
            size,
        } => {
            use crate::common::types::IValue;
            use std::sync::Arc;

            // Calculate total size in bytes (assuming 8 bytes per element)
            let element_size = 8u32; // Simplified: all elements are 8 bytes
            let total_size = element_size * (*size as u32);

            // Create array type for annotation
            let array_ty = IType::Array {
                element_type: Arc::new(element_ty.clone()),
                size: IValue::Int(*size),
            };

            instrs.push(DtalInstr::Alloca {
                dst: Reg::Virtual(*dst),
                size: total_size,
                ty: array_ty,
            });
        }

        TirInstr::AssumeConstraint { constraint } => {
            instrs.push(DtalInstr::ConstraintAssume {
                constraint: constraint.clone(),
            });
        }
    }
}

/// Lower a binary operation
fn lower_binop<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    dst: crate::backend::dtal::VirtualReg,
    op: TirBinaryOp,
    lhs: crate::backend::dtal::VirtualReg,
    rhs: crate::backend::dtal::VirtualReg,
    ty: &IType<'src>,
) {
    match op {
        // Arithmetic operations map directly
        TirBinaryOp::Add => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Add,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: ty.clone(),
            });
        }
        TirBinaryOp::Sub => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Sub,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: ty.clone(),
            });
        }
        TirBinaryOp::Mul => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Mul,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: ty.clone(),
            });
        }

        // Comparison operations: emit cmp followed by set (using branch pattern)
        // For simplicity, we set the result based on comparison
        TirBinaryOp::Eq => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "eq");
        }
        TirBinaryOp::Ne => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "ne");
        }
        TirBinaryOp::Lt => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "lt");
        }
        TirBinaryOp::Le => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "le");
        }
        TirBinaryOp::Gt => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "gt");
        }
        TirBinaryOp::Ge => {
            lower_comparison(instrs, dst, lhs, rhs, ty, "ge");
        }

        // Logical operations
        TirBinaryOp::And => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::And,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: ty.clone(),
            });
        }
        TirBinaryOp::Or => {
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Or,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(lhs),
                rhs: Reg::Virtual(rhs),
                ty: ty.clone(),
            });
        }
    }
}

/// Lower a comparison operation
///
/// Compares lhs and rhs, then sets dst to 1 if condition is true, else 0.
/// Uses: cmp lhs, rhs; setcc dst
fn lower_comparison<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    dst: crate::backend::dtal::VirtualReg,
    lhs: crate::backend::dtal::VirtualReg,
    rhs: crate::backend::dtal::VirtualReg,
    _ty: &IType<'src>,
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
        ty: IType::Bool,
    });

    // Preserve constraint for verifier
    use crate::backend::dtal::Constraint;
    use crate::backend::dtal::IndexExpr;

    let constraint = match cmp_kind {
        "eq" => Constraint::Eq(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        "ne" => Constraint::Ne(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        "lt" => Constraint::Lt(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        "le" => Constraint::Le(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        "gt" => Constraint::Gt(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        "ge" => Constraint::Ge(
            IndexExpr::Var(format!("v{}", lhs.0)),
            IndexExpr::Var(format!("v{}", rhs.0)),
        ),
        _ => Constraint::True,
    };

    instrs.push(DtalInstr::ConstraintAssume { constraint });
}

/// Lower a unary operation
fn lower_unaryop<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    dst: crate::backend::dtal::VirtualReg,
    op: TirUnaryOp,
    operand: crate::backend::dtal::VirtualReg,
    ty: &IType<'src>,
) {
    match op {
        TirUnaryOp::Not => {
            instrs.push(DtalInstr::Not {
                dst: Reg::Virtual(dst),
                src: Reg::Virtual(operand),
                ty: ty.clone(),
            });
        }
        TirUnaryOp::Neg => {
            // Negate: dst = 0 - operand
            // First load 0, then subtract
            // For simplicity, we emit: dst = 0 - operand as sub instruction
            // This requires a temporary, but we can use the destination
            instrs.push(DtalInstr::MovImm {
                dst: Reg::Virtual(dst),
                imm: 0,
                ty: ty.clone(),
            });
            instrs.push(DtalInstr::BinOp {
                op: DtalBinaryOp::Sub,
                dst: Reg::Virtual(dst),
                lhs: Reg::Virtual(dst),
                rhs: Reg::Virtual(operand),
                ty: ty.clone(),
            });
        }
    }
}

/// Lower a function call
fn lower_call<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    dst: Option<crate::backend::dtal::VirtualReg>,
    func: &str,
    args: &[crate::backend::dtal::VirtualReg],
    result_ty: &IType<'src>,
) {
    use crate::backend::dtal::regs::PhysicalReg;

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
                ty: IType::Int, // TODO: track actual argument types
            });
        } else {
            // Push extra arguments onto stack (not implemented yet)
            instrs.push(DtalInstr::Push {
                src: Reg::Virtual(*arg),
                ty: IType::Int,
            });
        }
    }

    // Emit call instruction
    instrs.push(DtalInstr::Call {
        target: func.to_string(),
    });

    // Move result from r0 to destination
    if let Some(dst_reg) = dst {
        instrs.push(DtalInstr::MovReg {
            dst: Reg::Virtual(dst_reg),
            src: Reg::Physical(PhysicalReg::R0),
            ty: result_ty.clone(),
        });
    }
}
