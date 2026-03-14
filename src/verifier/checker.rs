//! Instruction verification
//!
//! This module verifies individual DTAL instructions maintain type invariants.

#![allow(clippy::result_large_err)]

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{BinaryOp, CmpOp, CmpOperands, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::{DtalType, DtalValue};
use crate::verifier::error::VerifyError;

/// Verify a single instruction updates the type state correctly
pub fn verify_instruction(
    instr: &DtalInstr,
    state: &mut TypeState,
    block_label: &str,
    program: &crate::backend::dtal::instr::DtalProgram,
) -> Result<(), VerifyError> {
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

        DtalInstr::CmpImm { lhs, imm } => {
            verify_cmp_imm(*lhs, *imm, state, block_label)?;
        }

        DtalInstr::SetCC { dst, cond: _ } => {
            // SetCC defines dst as Bool (0 or 1)
            state.register_types.insert(*dst, DtalType::Bool);
        }

        DtalInstr::Not { dst, src, ty } => {
            verify_not(*dst, *src, ty, state, block_label)?;
        }

        DtalInstr::TypeAnnotation { reg, ty } => {
            verify_type_annotation(*reg, ty, state, block_label)?;
        }

        DtalInstr::ConstraintAssume { .. } => {
            // Ignore compiler-emitted constraint assumptions.
            // The verifier derives constraints from Cmp+Branch sequences instead.
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

        // Call defines r0 with the return type
        DtalInstr::Call {
            target, return_ty, ..
        } => {
            // Check callee's precondition if available
            if let Some(callee) = program.functions.iter().find(|f| &f.name == target)
                && let Some(precond) = &callee.precondition
                && !is_constraint_provable(precond, &state.constraints)
            {
                return Err(VerifyError::PreconditionFailed {
                    block: block_label.to_string(),
                    callee: target.clone(),
                    constraint: precond.clone(),
                    context: state.constraints.clone(),
                });
            }
            use crate::backend::dtal::regs::PhysicalReg;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R0), return_ty.clone());
        }

        DtalInstr::Branch { cond, .. } => {
            // On the fall-through path, add the negated branch constraint.
            if let Some(constraint) = constraint_from_cmp_op(*cond, &state.last_cmp) {
                let negated = negate_cmp_op_constraint(*cond, &state.last_cmp);
                if let Some(neg) = negated {
                    state.constraints.push(neg);
                }
                let _ = constraint;
            }
        }

        // Control flow instructions are handled separately
        DtalInstr::Jmp { .. } | DtalInstr::Ret => {}
    }

    Ok(())
}

/// Verify mov immediate instruction
fn verify_mov_imm(
    dst: Reg,
    imm: i64,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    // For singleton types, check the value matches
    if let DtalType::SingletonInt(DtalValue::Int(expected)) = ty
        && imm != *expected
    {
        return Err(VerifyError::SingletonMismatch {
            block: block_label.to_string(),
            expected_value: *expected,
            actual_value: imm,
        });
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify mov register instruction
fn verify_mov_reg(
    dst: Reg,
    src: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    // Check source register is defined
    let src_ty = get_register_type(src, state, block_label)?;

    // Check source type is compatible with declared type
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
fn verify_binop(
    op: BinaryOp,
    dst: Reg,
    lhs: Reg,
    rhs: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let lhs_ty = get_register_type(lhs, state, block_label)?;
    let rhs_ty = get_register_type(rhs, state, block_label)?;

    // Check operand types based on operation
    match op {
        // Logical operations require boolean operands
        BinaryOp::And | BinaryOp::Or => {
            if !matches!(lhs_ty, DtalType::Bool) || !matches!(rhs_ty, DtalType::Bool) {
                return Err(VerifyError::BinOpTypeMismatch {
                    block: block_label.to_string(),
                    op: format!("{}", op),
                    lhs_type: lhs_ty,
                    rhs_type: rhs_ty,
                });
            }
        }
        // Arithmetic operations require numeric operands
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            if !is_numeric_type(&lhs_ty) || !is_numeric_type(&rhs_ty) {
                return Err(VerifyError::BinOpTypeMismatch {
                    block: block_label.to_string(),
                    op: format!("{}", op),
                    lhs_type: lhs_ty,
                    rhs_type: rhs_ty,
                });
            }

            // For singleton types, verify the result
            if let (
                DtalType::SingletonInt(DtalValue::Int(l)),
                DtalType::SingletonInt(DtalValue::Int(r)),
            ) = (&lhs_ty, &rhs_ty)
            {
                let expected_result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Sub => l - r,
                    BinaryOp::Mul => l * r,
                    BinaryOp::Div => {
                        if *r != 0 {
                            l / r
                        } else {
                            0
                        }
                    }
                    _ => unreachable!(),
                };

                if let DtalType::SingletonInt(DtalValue::Int(declared)) = ty
                    && *declared != expected_result
                {
                    return Err(VerifyError::SingletonMismatch {
                        block: block_label.to_string(),
                        expected_value: expected_result,
                        actual_value: *declared,
                    });
                }
            }
        }
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify add immediate instruction
fn verify_add_imm(
    dst: Reg,
    src: Reg,
    imm: i64,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let src_ty = get_register_type(src, state, block_label)?;

    if !is_numeric_type(&src_ty) {
        return Err(VerifyError::BinOpTypeMismatch {
            block: block_label.to_string(),
            op: "addi".to_string(),
            lhs_type: src_ty,
            rhs_type: DtalType::SingletonInt(DtalValue::Int(imm)),
        });
    }

    // For singleton types, verify the result
    if let DtalType::SingletonInt(DtalValue::Int(src_val)) = &src_ty {
        let expected_result = src_val + imm;

        if let DtalType::SingletonInt(DtalValue::Int(declared)) = ty
            && *declared != expected_result
        {
            return Err(VerifyError::SingletonMismatch {
                block: block_label.to_string(),
                expected_value: expected_result,
                actual_value: *declared,
            });
        }
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify load instruction
fn verify_load(
    dst: Reg,
    base: Reg,
    offset: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let base_ty = get_register_type(base, state, block_label)?;
    let _offset_ty = get_register_type(offset, state, block_label)?;

    // If base has an array type, perform bounds checking
    if let DtalType::Array {
        element_type,
        size: DtalValue::Int(array_size),
    } = &base_ty
    {
        // Construct bounds constraint: 0 <= offset < size
        let offset_expr = reg_to_index_expr(&offset);
        let bounds_constraint = Constraint::And(
            Box::new(Constraint::Ge(offset_expr.clone(), IndexExpr::Const(0))),
            Box::new(Constraint::Lt(offset_expr, IndexExpr::Const(*array_size))),
        );

        if !is_constraint_provable(&bounds_constraint, &state.constraints) {
            return Err(VerifyError::BoundsCheckFailed {
                block: block_label.to_string(),
                instr_desc: format!("load {:?}, [{:?} + {:?}]", dst, base, offset),
                constraint: bounds_constraint,
                context: state.constraints.clone(),
            });
        }

        // Derive element type from base, don't trust the annotation
        let derived_ty = element_type.as_ref().clone();
        if !types_compatible(&derived_ty, ty) {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("load {:?}, [{:?} + {:?}]", dst, base, offset),
                expected: ty.clone(),
                actual: derived_ty,
            });
        }
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify store instruction
fn verify_store(
    base: Reg,
    offset: Reg,
    src: Reg,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let base_ty = get_register_type(base, state, block_label)?;
    let _offset_ty = get_register_type(offset, state, block_label)?;
    let src_ty = get_register_type(src, state, block_label)?;

    // If base has an array type, perform bounds checking
    if let DtalType::Array {
        element_type,
        size: DtalValue::Int(array_size),
    } = &base_ty
    {
        // Construct bounds constraint: 0 <= offset < size
        let offset_expr = reg_to_index_expr(&offset);
        let bounds_constraint = Constraint::And(
            Box::new(Constraint::Ge(offset_expr.clone(), IndexExpr::Const(0))),
            Box::new(Constraint::Lt(offset_expr, IndexExpr::Const(*array_size))),
        );

        if !is_constraint_provable(&bounds_constraint, &state.constraints) {
            return Err(VerifyError::BoundsCheckFailed {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                constraint: bounds_constraint,
                context: state.constraints.clone(),
            });
        }

        // Check stored value type is compatible with array element type
        if !types_compatible(&src_ty, element_type.as_ref()) {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                expected: element_type.as_ref().clone(),
                actual: src_ty,
            });
        }
    }

    Ok(())
}

/// Verify cmp instruction
fn verify_cmp(
    lhs: Reg,
    rhs: Reg,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(lhs, state, block_label)?;
    check_register_defined(rhs, state, block_label)?;
    state.last_cmp = Some(CmpOperands::RegReg(lhs, rhs));
    Ok(())
}

/// Verify cmp immediate instruction
fn verify_cmp_imm(
    lhs: Reg,
    imm: i64,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(lhs, state, block_label)?;
    state.last_cmp = Some(CmpOperands::RegImm(lhs, imm));
    Ok(())
}

/// Verify not instruction
fn verify_not(
    dst: Reg,
    src: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(src, state, block_label)?;
    state.register_types.insert(dst, ty.clone());
    Ok(())
}

/// Verify a type annotation
fn verify_type_annotation(
    reg: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let Some(existing_ty) = state.register_types.get(&reg) {
        // Register already has a type -- verify compatibility
        if !types_compatible(existing_ty, ty) {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("type_annotation {:?} : {}", reg, ty),
                expected: ty.clone(),
                actual: existing_ty.clone(),
            });
        }
    }
    // Set the type (either first definition via phi or verified annotation)
    state.register_types.insert(reg, ty.clone());
    Ok(())
}

/// Verify a constraint assertion
fn verify_constraint_assert(
    constraint: &Constraint,
    state: &TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    // Check if constraint is provable from current context (syntactic fast-path + Z3)
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

fn get_register_type(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
) -> Result<DtalType, VerifyError> {
    state
        .register_types
        .get(&reg)
        .cloned()
        .ok_or_else(|| VerifyError::UndefinedRegister {
            reg,
            block: block_label.to_string(),
        })
}

fn check_register_defined(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if state.register_types.contains_key(&reg) {
        Ok(())
    } else {
        Err(VerifyError::UndefinedRegister {
            reg,
            block: block_label.to_string(),
        })
    }
}

fn is_numeric_type(ty: &DtalType) -> bool {
    matches!(
        ty,
        DtalType::Int | DtalType::SingletonInt(_) | DtalType::RefinedInt { .. }
    )
}

/// Check if actual type is a subtype of (or equal to) expected type.
pub fn types_compatible(actual: &DtalType, expected: &DtalType) -> bool {
    match (actual, expected) {
        (DtalType::Int, DtalType::Int) => true,
        (DtalType::Bool, DtalType::Bool) => true,
        (DtalType::Unit, DtalType::Unit) => true,
        (DtalType::SingletonInt(a), DtalType::SingletonInt(b)) => a == b,
        (DtalType::SingletonInt(_), DtalType::Int) => true,
        (DtalType::RefinedInt { .. }, DtalType::Int) => true,
        (DtalType::Int, DtalType::SingletonInt(_)) => false,
        (DtalType::Int, DtalType::RefinedInt { .. }) => false,
        (
            DtalType::Array {
                element_type: e1,
                size: s1,
            },
            DtalType::Array {
                element_type: e2,
                size: s2,
            },
        ) => types_compatible(e1.as_ref(), e2.as_ref()) && s1 == s2,
        (DtalType::Ref(a), DtalType::Ref(b)) => {
            types_compatible(a.as_ref(), b.as_ref()) && types_compatible(b.as_ref(), a.as_ref())
        }
        (DtalType::RefMut(a), DtalType::RefMut(b)) => {
            types_compatible(a.as_ref(), b.as_ref()) && types_compatible(b.as_ref(), a.as_ref())
        }
        (DtalType::Master(a), DtalType::Master(b)) => {
            types_compatible(a.as_ref(), b.as_ref()) && types_compatible(b.as_ref(), a.as_ref())
        }
        _ => false,
    }
}

/// Check if a constraint is provable from context
pub fn is_constraint_provable(goal: &Constraint, context: &[Constraint]) -> bool {
    if matches!(goal, Constraint::True) {
        return true;
    }

    if matches!(goal, Constraint::False) {
        return false;
    }

    if context.contains(goal) {
        return true;
    }

    for ctx in context {
        if constraint_entails(ctx, goal) {
            return true;
        }
    }

    // Use Z3 for all non-trivial cases
    crate::verifier::smt::ConstraintOracle::is_provable(goal, context)
}

/// Check if one constraint entails another (simple cases)
fn constraint_entails(premise: &Constraint, conclusion: &Constraint) -> bool {
    if premise == conclusion {
        return true;
    }

    match (premise, conclusion) {
        (Constraint::Eq(a1, b1), Constraint::Eq(a2, b2)) => a1 == a2 && b1 == b2,
        (Constraint::Le(a1, b1), Constraint::Lt(a2, b2)) => {
            a1 == a2 && matches!((b1, b2), (IndexExpr::Const(x), IndexExpr::Const(y)) if *x < *y)
        }
        (Constraint::Ge(a1, b1), Constraint::Ge(a2, b2)) => {
            a1 == a2 && matches!((b1, b2), (IndexExpr::Const(x), IndexExpr::Const(y)) if *x >= *y)
        }
        _ => false,
    }
}

/// Convert a register to an IndexExpr variable
pub fn reg_to_index_expr(reg: &Reg) -> IndexExpr {
    IndexExpr::Var(format!("{}", reg))
}

/// Negate a CmpOp (Lt <-> Ge, Le <-> Gt, Eq <-> Ne)
pub fn negate_cmp_op(op: CmpOp) -> CmpOp {
    match op {
        CmpOp::Eq => CmpOp::Ne,
        CmpOp::Ne => CmpOp::Eq,
        CmpOp::Lt => CmpOp::Ge,
        CmpOp::Ge => CmpOp::Lt,
        CmpOp::Le => CmpOp::Gt,
        CmpOp::Gt => CmpOp::Le,
    }
}

/// Construct a constraint from a CmpOp and comparison operands
pub fn constraint_from_cmp_op(op: CmpOp, last_cmp: &Option<CmpOperands>) -> Option<Constraint> {
    let (lhs_expr, rhs_expr) = match last_cmp {
        Some(CmpOperands::RegReg(lhs, rhs)) => (reg_to_index_expr(lhs), reg_to_index_expr(rhs)),
        Some(CmpOperands::RegImm(lhs, imm)) => (reg_to_index_expr(lhs), IndexExpr::Const(*imm)),
        None => return None,
    };

    Some(match op {
        CmpOp::Eq => Constraint::Eq(lhs_expr, rhs_expr),
        CmpOp::Ne => Constraint::Ne(lhs_expr, rhs_expr),
        CmpOp::Lt => Constraint::Lt(lhs_expr, rhs_expr),
        CmpOp::Le => Constraint::Le(lhs_expr, rhs_expr),
        CmpOp::Gt => Constraint::Gt(lhs_expr, rhs_expr),
        CmpOp::Ge => Constraint::Ge(lhs_expr, rhs_expr),
    })
}

/// Construct the negated constraint from a CmpOp and comparison operands
pub fn negate_cmp_op_constraint(op: CmpOp, last_cmp: &Option<CmpOperands>) -> Option<Constraint> {
    constraint_from_cmp_op(negate_cmp_op(op), last_cmp)
}
