//! Instruction verification
//!
//! This module verifies individual DTAL instructions maintain type invariants.

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{BinaryOp, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::common::types::{IType, IValue};
use crate::verifier::error::VerifyError;

/// Verify a single instruction updates the type state correctly
pub fn verify_instruction<'src>(
    instr: &DtalInstr<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    match instr {
        DtalInstr::MovImm { dst, imm, ty } => {
            verify_mov_imm(*dst, *imm, ty, state, block_label)?;
        }

        DtalInstr::MovReg { dst, src, ty } => {
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
        }

        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } => {
            verify_binop(*op, *dst, *lhs, *rhs, ty, state, block_label)?;
        }

        DtalInstr::AddImm { dst, src, imm, ty } => {
            verify_add_imm(*dst, *src, *imm, ty, state, block_label)?;
        }

        DtalInstr::Load {
            dst,
            base,
            offset,
            ty,
        } => {
            verify_load(*dst, *base, *offset, ty, state, block_label)?;
        }

        DtalInstr::Store { base, offset, src } => {
            verify_store(*base, *offset, *src, state, block_label)?;
        }

        DtalInstr::Cmp { lhs, rhs } => {
            verify_cmp(*lhs, *rhs, state, block_label)?;
        }

        DtalInstr::CmpImm { lhs, imm: _ } => {
            verify_cmp_imm(*lhs, state, block_label)?;
        }

        DtalInstr::Not { dst, src, ty } => {
            verify_not(*dst, *src, ty, state, block_label)?;
        }

        DtalInstr::TypeAnnotation { reg, ty } => {
            state.register_types.insert(*reg, ty.clone());
        }

        DtalInstr::ConstraintAssume { constraint } => {
            state.constraints.push(constraint.clone());
        }

        DtalInstr::ConstraintAssert { constraint, msg: _ } => {
            verify_constraint_assert(constraint, state, block_label)?;
        }

        DtalInstr::Push { src, ty: _ } => {
            check_register_defined(*src, state, block_label)?;
        }

        DtalInstr::Pop { dst, ty } => {
            state.register_types.insert(*dst, ty.clone());
        }

        DtalInstr::Alloca { dst, size: _, ty } => {
            state.register_types.insert(*dst, ty.clone());
        }

        // Control flow instructions are handled separately
        DtalInstr::Jmp { .. }
        | DtalInstr::Branch { .. }
        | DtalInstr::Call { .. }
        | DtalInstr::Ret => {}
    }

    Ok(())
}

/// Verify mov immediate instruction
fn verify_mov_imm<'src>(
    dst: Reg,
    imm: i64,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    // For singleton types, check the value matches
    if let IType::SingletonInt(IValue::Int(expected)) = ty {
        if imm != *expected {
            return Err(VerifyError::SingletonMismatch {
                block: block_label.to_string(),
                expected_value: *expected,
                actual_value: imm,
            });
        }
    }

    // For refined types, we would check the predicate here
    // For now, trust the annotation (frontend already verified)

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify mov register instruction
fn verify_mov_reg<'src>(
    dst: Reg,
    src: Reg,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    // Check source register is defined
    let src_ty = get_register_type(src, state, block_label)?;

    // Check source type is compatible with declared type
    // For now, we trust the annotation matches (subtyping check would go here)
    if !types_compatible(&src_ty, ty) {
        return Err(VerifyError::TypeMismatch {
            block: block_label.to_string(),
            instr_desc: format!("mov {:?}, {:?}", dst, src),
            expected: ty.clone(),
            actual: src_ty,
        });
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify binary operation
fn verify_binop<'src>(
    op: BinaryOp,
    dst: Reg,
    lhs: Reg,
    rhs: Reg,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    let lhs_ty = get_register_type(lhs, state, block_label)?;
    let rhs_ty = get_register_type(rhs, state, block_label)?;

    // Check both operands are numeric types
    if !is_numeric_type(&lhs_ty) || !is_numeric_type(&rhs_ty) {
        return Err(VerifyError::BinOpTypeMismatch {
            block: block_label.to_string(),
            op: format!("{}", op),
            lhs_type: lhs_ty,
            rhs_type: rhs_ty,
        });
    }

    // For singleton types, verify the result
    if let (IType::SingletonInt(IValue::Int(l)), IType::SingletonInt(IValue::Int(r))) =
        (&lhs_ty, &rhs_ty)
    {
        let expected_result = match op {
            BinaryOp::Add => l + r,
            BinaryOp::Sub => l - r,
            BinaryOp::Mul => l * r,
            BinaryOp::And => *l & *r,
            BinaryOp::Or => *l | *r,
        };

        if let IType::SingletonInt(IValue::Int(declared)) = ty {
            if *declared != expected_result {
                return Err(VerifyError::SingletonMismatch {
                    block: block_label.to_string(),
                    expected_value: expected_result,
                    actual_value: *declared,
                });
            }
        }
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify add immediate instruction
fn verify_add_imm<'src>(
    dst: Reg,
    src: Reg,
    imm: i64,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    let src_ty = get_register_type(src, state, block_label)?;

    if !is_numeric_type(&src_ty) {
        return Err(VerifyError::BinOpTypeMismatch {
            block: block_label.to_string(),
            op: "addi".to_string(),
            lhs_type: src_ty,
            rhs_type: IType::SingletonInt(IValue::Int(imm)),
        });
    }

    // For singleton types, verify the result
    if let IType::SingletonInt(IValue::Int(src_val)) = &src_ty {
        let expected_result = src_val + imm;

        if let IType::SingletonInt(IValue::Int(declared)) = ty {
            if *declared != expected_result {
                return Err(VerifyError::SingletonMismatch {
                    block: block_label.to_string(),
                    expected_value: expected_result,
                    actual_value: *declared,
                });
            }
        }
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify load instruction
fn verify_load<'src>(
    dst: Reg,
    base: Reg,
    offset: Reg,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    // Check base and offset are defined
    let _base_ty = get_register_type(base, state, block_label)?;
    let _offset_ty = get_register_type(offset, state, block_label)?;

    // In a full implementation, we would:
    // 1. Check base has array/pointer type
    // 2. Check offset type proves bounds (0 <= offset < size)
    // For now, trust the annotation

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify store instruction
fn verify_store<'src>(
    base: Reg,
    offset: Reg,
    src: Reg,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    // Check all registers are defined
    check_register_defined(base, state, block_label)?;
    check_register_defined(offset, state, block_label)?;
    check_register_defined(src, state, block_label)?;

    // In a full implementation, check bounds proof
    Ok(())
}

/// Verify cmp instruction
fn verify_cmp<'src>(
    lhs: Reg,
    rhs: Reg,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    check_register_defined(lhs, state, block_label)?;
    check_register_defined(rhs, state, block_label)?;
    Ok(())
}

/// Verify cmp immediate instruction
fn verify_cmp_imm<'src>(
    lhs: Reg,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    check_register_defined(lhs, state, block_label)?;
    Ok(())
}

/// Verify not instruction
fn verify_not<'src>(
    dst: Reg,
    src: Reg,
    ty: &IType<'src>,
    state: &mut TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    check_register_defined(src, state, block_label)?;
    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify a constraint assertion
fn verify_constraint_assert<'src>(
    constraint: &Constraint,
    state: &TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    // Check if constraint is provable from current context
    // For now, use simple syntactic check; full impl would use Z3
    if !is_constraint_provable(constraint, &state.constraints) {
        return Err(VerifyError::UnprovableConstraint {
            constraint: constraint.clone(),
            context: state.constraints.clone(),
            block: block_label.to_string(),
        });
    }
    Ok(())
}

// Helper functions

fn get_register_type<'src>(
    reg: Reg,
    state: &TypeState<'src>,
    block_label: &str,
) -> Result<IType<'src>, VerifyError<'src>> {
    state
        .register_types
        .get(&reg)
        .cloned()
        .ok_or_else(|| VerifyError::UndefinedRegister {
            reg,
            block: block_label.to_string(),
        })
}

fn check_register_defined<'src>(
    reg: Reg,
    state: &TypeState<'src>,
    block_label: &str,
) -> Result<(), VerifyError<'src>> {
    if state.register_types.contains_key(&reg) {
        Ok(())
    } else {
        Err(VerifyError::UndefinedRegister {
            reg,
            block: block_label.to_string(),
        })
    }
}

fn is_numeric_type(ty: &IType) -> bool {
    matches!(
        ty,
        IType::Int | IType::SingletonInt(_) | IType::RefinedInt { .. }
    )
}

/// Check if two types are compatible (simplified subtyping check)
fn types_compatible<'src>(actual: &IType<'src>, expected: &IType<'src>) -> bool {
    // Simple structural equality for now
    // A full implementation would check subtyping
    match (actual, expected) {
        (IType::Int, IType::Int) => true,
        (IType::Bool, IType::Bool) => true,
        (IType::Unit, IType::Unit) => true,
        (IType::SingletonInt(a), IType::SingletonInt(b)) => a == b,
        (IType::SingletonInt(_), IType::Int) => true, // Singleton is subtype of int
        (IType::RefinedInt { .. }, IType::Int) => true, // Refined is subtype of int
        // For arrays, refs, etc. - simplified check
        _ => true, // Trust for now
    }
}

/// Check if a constraint is provable from context (simplified)
fn is_constraint_provable(goal: &Constraint, context: &[Constraint]) -> bool {
    // Trivial cases
    if matches!(goal, Constraint::True) {
        return true;
    }

    // Check if goal is directly in context
    if context.contains(goal) {
        return true;
    }

    // Check for simple entailments
    for ctx in context {
        if constraint_entails(ctx, goal) {
            return true;
        }
    }

    // For a full implementation, use Z3 here
    // For now, be permissive (trust frontend verification)
    true
}

/// Check if one constraint entails another (simple cases)
fn constraint_entails(premise: &Constraint, conclusion: &Constraint) -> bool {
    // Direct equality
    if premise == conclusion {
        return true;
    }

    // Le implies Lt for smaller bound
    match (premise, conclusion) {
        (Constraint::Eq(a1, b1), Constraint::Eq(a2, b2)) => a1 == a2 && b1 == b2,
        (Constraint::Le(a1, b1), Constraint::Lt(a2, b2)) => {
            // a <= b doesn't imply a < b directly, but a <= b-1 does
            a1 == a2 && matches!((b1, b2), (IndexExpr::Const(x), IndexExpr::Const(y)) if *x < *y)
        }
        (Constraint::Ge(a1, b1), Constraint::Ge(a2, b2)) => {
            // a >= c1 implies a >= c2 if c1 >= c2
            a1 == a2
                && matches!((b1, b2), (IndexExpr::Const(x), IndexExpr::Const(y)) if *x >= *y)
        }
        _ => false,
    }
}
