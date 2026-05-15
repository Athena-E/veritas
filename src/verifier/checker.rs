//! Instruction-level DTAL verification.
//!
//! The checker derives post-instruction type and ownership state from each
//! DTAL instruction. It rejects annotations that disagree with derivation and
//! records constraints needed by later verifier checks.
//!
//! # State Transition
//!
//! ```text
//! (instruction, pre-state)
//!        |
//!        v
//! derive result type + ownership changes
//!        |
//!        v
//! compare explicit annotations and update post-state
//! ```
//!
//! # Design Notes
//!
//! The checker does not trust DTAL type annotations. It derives the type that
//! should result from each instruction, then verifies that any annotation is
//! compatible under the current constraint context. Ownership is modeled as
//! explicit object identity so moves, borrows, drops, and joins can be checked
//! without relying on register names alone.
//!
//! # Errors
//!
//! Instruction checks return [`VerifyError`] variants for undefined registers,
//! consumed values, type mismatches, unprovable constraints, failed bounds
//! checks, contract failures, and ownership violations.
//!
//! # Related Modules
//!
//! The `dataflow` module uses checker-like derivation for fallback analysis,
//! and [`crate::verifier::smt`] discharges constraint implications.

#![allow(clippy::result_large_err)]

use crate::common::ownership::LifetimeId;
use crate::dtal::constraints::{Constraint, IndexExpr};
use crate::dtal::instr::{BinaryOp, CmpOp, CmpOperands, DtalInstr, TypeState};
use crate::dtal::regs::Reg;
use crate::dtal::types::DtalType;
use crate::verifier::error::VerifyError;

pub fn verify_instruction(
    instr: &DtalInstr,
    state: &mut TypeState,
    block_label: &str,
    program: &crate::dtal::instr::DtalProgram,
) -> Result<(), VerifyError> {
    match instr {
        DtalInstr::MovImm { dst, imm, ty } => {
            verify_mov_imm(*dst, *imm, ty, state, block_label)?;
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::MovReg { dst, src, ty } => {
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            verify_plain_mov_does_not_duplicate_owned(*src, *dst, state, block_label)?;
            verify_plain_mov_does_not_duplicate_mutable_borrow(*src, *dst, state, block_label)?;
            preserve_plain_mov_alias_ownership(*src, *dst, state);
            preserve_plain_mov_shared_borrow(*src, *dst, state);
            preserve_plain_mov_mutable_borrow(*src, *dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::AliasBorrow {
            lifetime,
            dst,
            src,
            ty,
        } => {
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "alias_borrow")?;
            clear_owned(*dst, state);
            assign_shared_borrow_from(*src, *dst, *lifetime, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::BorrowMut {
            lifetime,
            dst,
            src,
            ty,
        } => {
            verify_owned_available(*src, state, block_label, "borrow_mut")?;
            verify_object_not_shared_borrowed(*src, state, block_label, "borrow_mut")?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "borrow_mut")?;
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            assign_mutable_borrow_from(*src, *dst, *lifetime, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::BorrowEnd { lifetime, src, .. } => {
            verify_borrow_available(*src, state, block_label, "borrow_end")?;
            verify_borrow_lifetime_matches(*src, *lifetime, state, block_label)?;
            clear_shared_borrow(*src, state);
            clear_mutable_borrow(*src, state);
        }

        DtalInstr::MoveOwned { dst, src, ty } => {
            verify_owned_available(*src, state, block_label, "move_owned")?;
            verify_object_not_shared_borrowed(*src, state, block_label, "move_owned")?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "move_owned")?;
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            transfer_owned(*src, *dst, state);
            consume_reg(*src, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } => {
            verify_binop(*op, *dst, *lhs, *rhs, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::AddImm { dst, src, imm, ty } => {
            verify_add_imm(*dst, *src, *imm, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::Load {
            dst,
            base,
            offset,
            ty,
        } => {
            verify_load(*dst, *base, *offset, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::LoadOp {
            dst,
            base,
            offset,
            other,
            ty,
            ..
        } => {
            verify_load_op(*dst, *base, *offset, *other, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
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
            state.register_types.insert(*dst, DtalType::Bool);
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::Not { dst, src, ty } => {
            verify_not(*dst, *src, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::Neg { dst, src, ty } => {
            verify_neg(*dst, *src, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::ShlImm { dst, src, ty, .. } | DtalInstr::ShrImm { dst, src, ty, .. } => {
            verify_shift_imm(*dst, *src, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::TypeAnnotation { reg, ty } => {
            verify_type_annotation(*reg, ty, state, block_label)?;
        }

        DtalInstr::ConstraintAssert { constraint } => {
            verify_constraint_assert(constraint, state, block_label)?;
        }

        DtalInstr::Push { src, ty: _ } => {
            let src_ty = get_register_type(*src, state, block_label)?;
            state.stack.push(src_ty);
            state.owned_stack.push(state.owned_registers.contains(src));
            state
                .owned_stack_object_ids
                .push(state.owned_object_ids.get(src).copied());
            state
                .shared_borrow_stack_object_ids
                .push(state.shared_borrow_object_ids.get(src).copied());
            state
                .shared_borrow_stack_lifetimes
                .push(state.shared_borrow_lifetimes.get(src).copied());
            state
                .mutable_borrow_stack_object_ids
                .push(state.mutable_borrow_object_ids.get(src).copied());
            state
                .mutable_borrow_stack_lifetimes
                .push(state.mutable_borrow_lifetimes.get(src).copied());
        }

        DtalInstr::Pop { dst, ty: _ } => {
            let popped_ty = state.stack.pop().unwrap_or(DtalType::Int);
            state.register_types.insert(*dst, popped_ty);
            if state.owned_stack.pop().unwrap_or(false) {
                if let Some(object_id) = state.owned_stack_object_ids.pop().unwrap_or(None) {
                    assign_owned_object(*dst, object_id, state);
                } else {
                    state.owned_registers.insert(*dst);
                }
            } else {
                clear_owned(*dst, state);
                let _ = state.owned_stack_object_ids.pop();
            }
            if let Some(object_id) = state.shared_borrow_stack_object_ids.pop().unwrap_or(None) {
                let lifetime = state
                    .shared_borrow_stack_lifetimes
                    .pop()
                    .unwrap_or(None)
                    .flatten();
                assign_shared_borrow_object(*dst, object_id, lifetime, state);
            } else {
                clear_shared_borrow(*dst, state);
                let _ = state.shared_borrow_stack_lifetimes.pop();
            }
            if let Some(object_id) = state.mutable_borrow_stack_object_ids.pop().unwrap_or(None) {
                let lifetime = state
                    .mutable_borrow_stack_lifetimes
                    .pop()
                    .unwrap_or(None)
                    .flatten();
                assign_mutable_borrow_object(*dst, object_id, lifetime, state);
            } else {
                clear_mutable_borrow(*dst, state);
                let _ = state.mutable_borrow_stack_lifetimes.pop();
            }
            clear_consumed(*dst, state);
        }

        DtalInstr::Alloca { dst, size: _, ty } => {
            state.register_types.insert(*dst, ty.clone());
            set_owned_from_type(*dst, ty, state, true);
            clear_consumed(*dst, state);
            if let DtalType::Array { size, .. } = ty {
                state.array_versions.insert(*dst, 0);
                let arr_name = versioned_array_name(dst, 0);
                state.constraints.push(Constraint::Forall {
                    var: "_k".to_string(),
                    lower: IndexExpr::Const(0),
                    upper: size.clone(),
                    body: Box::new(Constraint::Eq(
                        IndexExpr::Select(arr_name, Box::new(IndexExpr::Var("_k".to_string()))),
                        IndexExpr::Const(0),
                    )),
                });
            }
        }

        DtalInstr::Call {
            target,
            arg_kinds,
            ownership,
            ..
        } => {
            use crate::dtal::regs::PhysicalReg;

            let derived_return_ty = if let Some(callee) =
                program.functions.iter().find(|f| &f.name == target)
            {
                if let Some(precond) = &callee.precondition {
                    let param_regs = PhysicalReg::param_regs();
                    let param_subs: std::collections::HashMap<String, String> = callee
                        .params
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| *i < param_regs.len())
                        .map(|(i, (reg, _))| (format!("{}", reg), format!("{}", param_regs[i])))
                        .collect();
                    let mut substituted = substitute_select_names(precond, &param_subs);
                    substituted = substitute_var_names_in_constraint(&substituted, &param_subs);
                    substituted =
                        version_substitute_constraint(&substituted, &state.array_versions);

                    if !is_constraint_provable(&substituted, &state.constraints) {
                        return Err(VerifyError::PreconditionFailed {
                            block: block_label.to_string(),
                            callee: target.clone(),
                            constraint: precond.clone(),
                            context: state.constraints.clone(),
                        });
                    }
                }

                if let Some(postcond) = &callee.postcondition {
                    let r0_name = format!("{}", PhysicalReg::R0);
                    let mut postcond_subs: std::collections::HashMap<String, String> =
                        std::collections::HashMap::new();
                    postcond_subs.insert("result".to_string(), r0_name);
                    let mut substituted = substitute_select_names(postcond, &postcond_subs);
                    substituted = substitute_var_names_in_constraint(&substituted, &postcond_subs);
                    substituted =
                        version_substitute_constraint(&substituted, &state.array_versions);
                    state.constraints.push(substituted);
                }

                callee.return_type.clone()
            } else {
                return Err(VerifyError::UnknownFunction {
                    name: target.clone(),
                });
            };

            for (index, arg_kind) in arg_kinds.iter().enumerate() {
                if !arg_kind.is_owned_value() {
                    if let Some(param_reg) = PhysicalReg::param_regs().get(index).copied() {
                        let param_reg = Reg::Physical(param_reg);
                        clear_owned(param_reg, state);
                        clear_shared_borrow(param_reg, state);
                        clear_mutable_borrow(param_reg, state);
                    }
                    continue;
                }
                if let Some(param_reg) = PhysicalReg::param_regs().get(index).copied() {
                    let param_reg = Reg::Physical(param_reg);
                    verify_owned_available(param_reg, state, block_label, "call_consume_arg")?;
                    verify_object_not_shared_borrowed(
                        param_reg,
                        state,
                        block_label,
                        "call_consume_arg",
                    )?;
                    clear_owned(param_reg, state);
                    clear_shared_borrow(param_reg, state);
                    clear_mutable_borrow(param_reg, state);
                    consume_reg(param_reg, state);
                }
            }

            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R0), derived_return_ty.clone());
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::LR), derived_return_ty);
            set_reg_owned(
                Reg::Physical(PhysicalReg::R0),
                ownership.produces_owned_output(),
                state,
            );
            set_reg_owned(
                Reg::Physical(PhysicalReg::LR),
                ownership.produces_owned_output(),
                state,
            );
            if ownership.produces_owned_output() {
                let object_id = fresh_object_id(state);
                assign_owned_object(Reg::Physical(PhysicalReg::R0), object_id, state);
                assign_owned_object(Reg::Physical(PhysicalReg::LR), object_id, state);
            }
            clear_consumed(Reg::Physical(PhysicalReg::R0), state);
            clear_consumed(Reg::Physical(PhysicalReg::LR), state);
        }

        DtalInstr::Branch { cond, .. } => {
            if let Some(constraint) = constraint_from_cmp_op(*cond, &state.last_cmp) {
                let negated = negate_cmp_op_constraint(*cond, &state.last_cmp);
                if let Some(neg) = negated {
                    state.constraints.push(neg);
                }
                let _ = constraint;
            }
        }

        DtalInstr::Jmp { .. } | DtalInstr::Ret => {}

        DtalInstr::PortIn { dst, port } => {
            check_register_defined(*port, state, block_label)?;
            state.register_types.insert(*dst, DtalType::Int);
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            clear_mutable_borrow(*dst, state);
            clear_consumed(*dst, state);
        }
        DtalInstr::PortOut { port, value } => {
            check_register_defined(*port, state, block_label)?;
            check_register_defined(*value, state, block_label)?;
        }

        DtalInstr::Cqo => {
            use crate::dtal::regs::PhysicalReg;
            let rax = Reg::Physical(PhysicalReg::LR);
            check_register_defined(rax, state, block_label)?;
            let rax_ty = get_register_type(rax, state, block_label)?;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R2), rax_ty);
            if let Some(object_id) = state.owned_object_ids.get(&rax).copied() {
                assign_owned_object(Reg::Physical(PhysicalReg::R2), object_id, state);
            } else {
                state
                    .owned_registers
                    .remove(&Reg::Physical(PhysicalReg::R2));
                state
                    .owned_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R2));
                state
                    .shared_borrow_object_ids
                    .remove(&Reg::Physical(PhysicalReg::R2));
            }
            clear_consumed(Reg::Physical(PhysicalReg::R2), state);
        }

        DtalInstr::Idiv { src } => {
            use crate::dtal::regs::PhysicalReg;
            let rax = Reg::Physical(PhysicalReg::LR);
            let rdx = Reg::Physical(PhysicalReg::R2);
            check_register_defined(rax, state, block_label)?;
            check_register_defined(rdx, state, block_label)?;
            check_register_defined(*src, state, block_label)?;
            state.register_types.insert(rax, DtalType::Int);
            state.register_types.insert(rdx, DtalType::Int);
            clear_owned(rax, state);
            clear_owned(rdx, state);
            clear_shared_borrow(rax, state);
            clear_shared_borrow(rdx, state);
            clear_mutable_borrow(rax, state);
            clear_mutable_borrow(rdx, state);
            clear_consumed(rax, state);
            clear_consumed(rdx, state);
        }

        DtalInstr::SpillStore { src, offset, ty } => {
            check_register_defined(*src, state, block_label)?;
            state.spill_types.insert(*offset, ty.clone());
            if let Some(object_id) = state.owned_object_ids.get(src).copied() {
                state.owned_spills.insert(*offset);
                state.owned_spill_object_ids.insert(*offset, object_id);
                clear_owned_alias_group(*src, Some(object_id), state);
                consume_owned_alias_group(*src, Some(object_id), state);
            } else {
                state.owned_spills.remove(offset);
                state.owned_spill_object_ids.remove(offset);
            }
            if let Some(object_id) = state.shared_borrow_object_ids.get(src).copied() {
                state
                    .shared_borrow_spill_object_ids
                    .insert(*offset, object_id);
                state.shared_borrow_spill_lifetimes.insert(
                    *offset,
                    state.shared_borrow_lifetimes.get(src).copied().flatten(),
                );
            } else {
                state.shared_borrow_spill_object_ids.remove(offset);
                state.shared_borrow_spill_lifetimes.remove(offset);
            }
            if let Some(object_id) = state.mutable_borrow_object_ids.get(src).copied() {
                state
                    .mutable_borrow_spill_object_ids
                    .insert(*offset, object_id);
                state.mutable_borrow_spill_lifetimes.insert(
                    *offset,
                    state.mutable_borrow_lifetimes.get(src).copied().flatten(),
                );
            } else {
                state.mutable_borrow_spill_object_ids.remove(offset);
                state.mutable_borrow_spill_lifetimes.remove(offset);
            }
        }

        DtalInstr::SpillLoad { dst, offset, ty } => {
            let derived_ty = if let Some(stored_ty) = state.spill_types.get(offset).cloned() {
                stored_ty
            } else {
                ty.clone()
            };
            state.register_types.insert(*dst, derived_ty);
            if let Some(object_id) = state.owned_spill_object_ids.get(offset).copied() {
                assign_owned_object(*dst, object_id, state);
            } else {
                clear_owned(*dst, state);
            }
            if let Some(object_id) = state.shared_borrow_spill_object_ids.get(offset).copied() {
                let lifetime = state
                    .shared_borrow_spill_lifetimes
                    .get(offset)
                    .copied()
                    .flatten();
                assign_shared_borrow_object(*dst, object_id, lifetime, state);
            } else {
                clear_shared_borrow(*dst, state);
            }
            if let Some(object_id) = state.mutable_borrow_spill_object_ids.get(offset).copied() {
                let lifetime = state
                    .mutable_borrow_spill_lifetimes
                    .get(offset)
                    .copied()
                    .flatten();
                assign_mutable_borrow_object(*dst, object_id, lifetime, state);
            } else {
                clear_mutable_borrow(*dst, state);
            }
            clear_consumed(*dst, state);
        }

        DtalInstr::Prologue { .. } => {
            use crate::dtal::regs::PhysicalReg;
            for preg in &[
                PhysicalReg::LR,
                PhysicalReg::R0,
                PhysicalReg::R1,
                PhysicalReg::R2,
                PhysicalReg::R3,
                PhysicalReg::R4,
                PhysicalReg::R5,
                PhysicalReg::R6,
                PhysicalReg::R7,
                PhysicalReg::R8,
                PhysicalReg::R9,
                PhysicalReg::R10,
                PhysicalReg::R11,
                PhysicalReg::R12,
            ] {
                state
                    .register_types
                    .entry(Reg::Physical(*preg))
                    .or_insert(DtalType::Int);
                clear_mutable_borrow(Reg::Physical(*preg), state);
                clear_consumed(Reg::Physical(*preg), state);
            }
        }

        DtalInstr::Epilogue { .. } => {}

        DtalInstr::DropOwned { src, .. } => {
            verify_owned_available(*src, state, block_label, "drop_owned")?;
            verify_object_not_shared_borrowed(*src, state, block_label, "drop_owned")?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "drop_owned")?;
            let object_id = state.owned_object_ids.get(src).copied();
            clear_owned_alias_group(*src, object_id, state);
            consume_owned_alias_group(*src, object_id, state);
        }
    }

    verify_unique_owned_objects(state, block_label, "state update")?;

    Ok(())
}

fn clear_owned(reg: Reg, state: &mut TypeState) {
    state.owned_registers.remove(&reg);
    state.owned_object_ids.remove(&reg);
}

fn clear_shared_borrow(reg: Reg, state: &mut TypeState) {
    state.shared_borrow_object_ids.remove(&reg);
    state.shared_borrow_lifetimes.remove(&reg);
}

fn clear_mutable_borrow(reg: Reg, state: &mut TypeState) {
    state.mutable_borrow_object_ids.remove(&reg);
    state.mutable_borrow_lifetimes.remove(&reg);
}

fn verify_borrow_available(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
    instr_desc: &str,
) -> Result<(), VerifyError> {
    if state.shared_borrow_object_ids.contains_key(&reg)
        || state.mutable_borrow_object_ids.contains_key(&reg)
    {
        return Ok(());
    }
    Err(VerifyError::OwnershipViolation {
        block: block_label.to_string(),
        instr_desc: instr_desc.to_string(),
        msg: format!("register {:?} does not currently hold a live borrow", reg),
    })
}

fn verify_borrow_lifetime_matches(
    reg: Reg,
    expected: Option<LifetimeId>,
    state: &TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if expected.is_none() {
        return Ok(());
    }
    let actual = state
        .shared_borrow_lifetimes
        .get(&reg)
        .or_else(|| state.mutable_borrow_lifetimes.get(&reg))
        .copied()
        .flatten();
    if actual == expected {
        return Ok(());
    }
    Err(VerifyError::OwnershipViolation {
        block: block_label.to_string(),
        instr_desc: "borrow_end".to_string(),
        msg: format!(
            "borrow lifetime mismatch for {:?}: expected {:?}, found {:?}",
            reg, expected, actual
        ),
    })
}

fn abi_owned_alias_counterpart(reg: Reg) -> Option<Reg> {
    match reg {
        Reg::Physical(crate::dtal::regs::PhysicalReg::R0) => {
            Some(Reg::Physical(crate::dtal::regs::PhysicalReg::LR))
        }
        Reg::Physical(crate::dtal::regs::PhysicalReg::LR) => {
            Some(Reg::Physical(crate::dtal::regs::PhysicalReg::R0))
        }
        _ => None,
    }
}

fn clear_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    clear_owned(reg, state);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            clear_owned(counterpart, state);
        }
    }
}

fn consume_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    consume_reg(reg, state);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            consume_reg(counterpart, state);
        }
    }
}

fn consume_reg(reg: Reg, state: &mut TypeState) {
    state.consumed_registers.insert(reg);
}

fn clear_consumed(reg: Reg, state: &mut TypeState) {
    state.consumed_registers.remove(&reg);
}

fn set_reg_owned(reg: Reg, owned: bool, state: &mut TypeState) {
    if owned {
        state.owned_registers.insert(reg);
    } else {
        clear_owned(reg, state);
    }
}

fn transfer_owned(src: Reg, dst: Reg, state: &mut TypeState) {
    let object_id = state.owned_object_ids.get(&src).copied();
    clear_owned_alias_group(src, object_id, state);
    if let Some(object_id) = object_id {
        assign_owned_object(dst, object_id, state);
    } else {
        clear_owned(dst, state);
    }
}

fn preserve_plain_mov_alias_ownership(src: Reg, dst: Reg, state: &mut TypeState) {
    let is_abi_return_alias = matches!(
        (src, dst),
        (
            Reg::Physical(crate::dtal::regs::PhysicalReg::LR),
            Reg::Physical(crate::dtal::regs::PhysicalReg::R0)
        ) | (
            Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
            Reg::Physical(crate::dtal::regs::PhysicalReg::LR)
        )
    );
    if is_abi_return_alias && let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_owned_object(dst, object_id, state);
    } else {
        clear_owned(dst, state);
    }
}

fn preserve_plain_mov_shared_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        let lifetime = state.shared_borrow_lifetimes.get(&src).copied().flatten();
        assign_shared_borrow_object(dst, object_id, lifetime, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

fn preserve_plain_mov_mutable_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if src == dst {
        if let Some(object_id) = state.mutable_borrow_object_ids.get(&src).copied() {
            let lifetime = state.mutable_borrow_lifetimes.get(&src).copied().flatten();
            assign_mutable_borrow_object(dst, object_id, lifetime, state);
        } else {
            clear_mutable_borrow(dst, state);
        }
    } else {
        clear_mutable_borrow(dst, state);
    }
}

fn assign_shared_borrow_from(
    src: Reg,
    dst: Reg,
    lifetime: Option<LifetimeId>,
    state: &mut TypeState,
) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, lifetime, state);
    } else if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        let inherited_lifetime =
            lifetime.or_else(|| state.shared_borrow_lifetimes.get(&src).copied().flatten());
        assign_shared_borrow_object(dst, object_id, inherited_lifetime, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

fn assign_shared_borrow_object(
    reg: Reg,
    object_id: u32,
    lifetime: Option<LifetimeId>,
    state: &mut TypeState,
) {
    clear_owned(reg, state);
    clear_mutable_borrow(reg, state);
    state.shared_borrow_object_ids.insert(reg, object_id);
    state.shared_borrow_lifetimes.insert(reg, lifetime);
}

fn assign_mutable_borrow_from(
    src: Reg,
    dst: Reg,
    lifetime: Option<LifetimeId>,
    state: &mut TypeState,
) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_mutable_borrow_object(dst, object_id, lifetime, state);
    } else {
        clear_mutable_borrow(dst, state);
    }
}

fn assign_mutable_borrow_object(
    reg: Reg,
    object_id: u32,
    lifetime: Option<LifetimeId>,
    state: &mut TypeState,
) {
    clear_owned(reg, state);
    clear_shared_borrow(reg, state);
    state.mutable_borrow_object_ids.insert(reg, object_id);
    state.mutable_borrow_lifetimes.insert(reg, lifetime);
}

fn set_owned_from_type(reg: Reg, ty: &DtalType, state: &mut TypeState, fresh: bool) {
    if matches!(ty, DtalType::Array { .. }) {
        let object_id = if fresh {
            fresh_object_id(state)
        } else {
            state
                .owned_object_ids
                .get(&reg)
                .copied()
                .unwrap_or_else(|| fresh_object_id(state))
        };
        assign_owned_object(reg, object_id, state);
    } else {
        clear_owned(reg, state);
        clear_shared_borrow(reg, state);
        clear_mutable_borrow(reg, state);
    }
}

fn verify_owned_available(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
    instr_desc: &str,
) -> Result<(), VerifyError> {
    check_register_defined(reg, state, block_label)?;
    if !state.owned_registers.contains(&reg) {
        return Err(VerifyError::OwnershipViolation {
            block: block_label.to_string(),
            instr_desc: instr_desc.to_string(),
            msg: format!("register {:?} does not currently own a value", reg),
        });
    }
    Ok(())
}

fn verify_plain_mov_does_not_duplicate_owned(
    src: Reg,
    dst: Reg,
    state: &TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        let same_alias_pair = matches!(
            (src, dst),
            (
                Reg::Physical(crate::dtal::regs::PhysicalReg::LR),
                Reg::Physical(crate::dtal::regs::PhysicalReg::R0)
            ) | (
                Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
                Reg::Physical(crate::dtal::regs::PhysicalReg::LR)
            )
        ) && state.owned_object_ids.get(&dst).copied() == Some(object_id);
        let same_register = src == dst;
        if !same_alias_pair && !same_register {
            return Err(VerifyError::OwnershipViolation {
                block: block_label.to_string(),
                instr_desc: "mov".to_string(),
                msg: format!(
                    "plain mov would duplicate ownership of object o{} from {:?} to {:?}",
                    object_id, src, dst
                ),
            });
        }
    }
    Ok(())
}

fn verify_plain_mov_does_not_duplicate_mutable_borrow(
    src: Reg,
    dst: Reg,
    state: &TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let Some(object_id) = state.mutable_borrow_object_ids.get(&src).copied()
        && src != dst
    {
        return Err(VerifyError::OwnershipViolation {
            block: block_label.to_string(),
            instr_desc: "mov".to_string(),
            msg: format!(
                "plain mov would duplicate mutable borrow of object o{} from {:?} to {:?}",
                object_id, src, dst
            ),
        });
    }
    Ok(())
}

fn fresh_object_id(state: &mut TypeState) -> u32 {
    let object_id = state.next_object_id;
    state.next_object_id += 1;
    object_id
}

fn assign_owned_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_shared_borrow(reg, state);
    clear_mutable_borrow(reg, state);
    state.owned_registers.insert(reg);
    state.owned_object_ids.insert(reg, object_id);
}

fn verify_object_not_shared_borrowed(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
    instr_desc: &str,
) -> Result<(), VerifyError> {
    let Some(object_id) = state.owned_object_ids.get(&reg).copied() else {
        return Ok(());
    };

    let active_borrowers: Vec<Reg> = state
        .shared_borrow_object_ids
        .iter()
        .filter_map(|(borrow_reg, borrowed_object)| {
            if *borrowed_object == object_id {
                Some(*borrow_reg)
            } else {
                None
            }
        })
        .collect();

    if active_borrowers.is_empty() {
        return Ok(());
    }

    Err(VerifyError::OwnershipViolation {
        block: block_label.to_string(),
        instr_desc: instr_desc.to_string(),
        msg: format!(
            "object o{} still has active shared borrows in {:?}",
            object_id, active_borrowers
        ),
    })
}

fn verify_object_not_mutably_borrowed(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
    instr_desc: &str,
) -> Result<(), VerifyError> {
    let Some(object_id) = state.owned_object_ids.get(&reg).copied() else {
        return Ok(());
    };

    let active_borrowers: Vec<Reg> = state
        .mutable_borrow_object_ids
        .iter()
        .filter_map(|(borrow_reg, borrowed_object)| {
            if *borrowed_object == object_id {
                Some(*borrow_reg)
            } else {
                None
            }
        })
        .collect();

    if active_borrowers.is_empty() {
        return Ok(());
    }

    Err(VerifyError::OwnershipViolation {
        block: block_label.to_string(),
        instr_desc: instr_desc.to_string(),
        msg: format!(
            "object o{} still has active mutable borrows in {:?}",
            object_id, active_borrowers
        ),
    })
}

pub(crate) fn is_allowed_owned_alias_pair(lhs: Reg, rhs: Reg) -> bool {
    matches!(
        (lhs, rhs),
        (
            Reg::Physical(crate::dtal::regs::PhysicalReg::LR),
            Reg::Physical(crate::dtal::regs::PhysicalReg::R0)
        ) | (
            Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
            Reg::Physical(crate::dtal::regs::PhysicalReg::LR)
        )
    )
}

pub(crate) fn verify_unique_owned_objects(
    state: &TypeState,
    block_label: &str,
    instr_desc: &str,
) -> Result<(), VerifyError> {
    let mut regs: Vec<(Reg, u32)> = state
        .owned_object_ids
        .iter()
        .map(|(reg, object_id)| (*reg, *object_id))
        .collect();
    regs.sort_by_key(|(reg, object_id)| (format!("{:?}", reg), *object_id));

    for (idx, (lhs_reg, lhs_object)) in regs.iter().enumerate() {
        for (rhs_reg, rhs_object) in regs.iter().skip(idx + 1) {
            if lhs_object == rhs_object && !is_allowed_owned_alias_pair(*lhs_reg, *rhs_reg) {
                return Err(VerifyError::OwnershipViolation {
                    block: block_label.to_string(),
                    instr_desc: instr_desc.to_string(),
                    msg: format!(
                        "object o{} has multiple live owners: {:?} and {:?}",
                        lhs_object, lhs_reg, rhs_reg
                    ),
                });
            }
        }
    }

    Ok(())
}

fn verify_mov_imm(
    dst: Reg,
    imm: i128,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let DtalType::SingletonInt(IndexExpr::Const(expected)) = ty
        && imm != *expected
    {
        return Err(VerifyError::SingletonMismatch {
            block: block_label.to_string(),
            expected_value: *expected,
            actual_value: imm,
        });
    }

    if let DtalType::RefinedInt {
        var, constraint, ..
    } = ty
    {
        let substituted = substitute_const_in_constraint(constraint, var, imm);
        if !is_constraint_provable(&substituted, &state.constraints) {
            return Err(VerifyError::UnprovableConstraint {
                constraint: substituted,
                context: state.constraints.clone(),
                block: block_label.to_string(),
            });
        }
    }

    let derived_ty = DtalType::SingletonInt(IndexExpr::Const(imm));
    let derived_idx = IndexExpr::Const(imm);
    state.register_types.insert(dst, derived_ty);
    add_register_index_constraint(dst, &derived_idx, state);
    Ok(())
}

fn substitute_const_in_constraint(c: &Constraint, var: &str, value: i128) -> Constraint {
    substitute_const_in_constraint_inner(c, var, &IndexExpr::Const(value))
}

fn substitute_const_in_constraint_inner(
    c: &Constraint,
    var: &str,
    replacement: &IndexExpr,
) -> Constraint {
    match c {
        Constraint::True | Constraint::False => c.clone(),
        Constraint::Eq(l, r) => {
            Constraint::Eq(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::Lt(l, r) => {
            Constraint::Lt(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::Le(l, r) => {
            Constraint::Le(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::Gt(l, r) => {
            Constraint::Gt(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::Ge(l, r) => {
            Constraint::Ge(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::Ne(l, r) => {
            Constraint::Ne(sub_idx(l, var, replacement), sub_idx(r, var, replacement))
        }
        Constraint::And(l, r) => Constraint::And(
            Box::new(substitute_const_in_constraint_inner(l, var, replacement)),
            Box::new(substitute_const_in_constraint_inner(r, var, replacement)),
        ),
        Constraint::Or(l, r) => Constraint::Or(
            Box::new(substitute_const_in_constraint_inner(l, var, replacement)),
            Box::new(substitute_const_in_constraint_inner(r, var, replacement)),
        ),
        Constraint::Not(inner) => Constraint::Not(Box::new(substitute_const_in_constraint_inner(
            inner,
            var,
            replacement,
        ))),
        Constraint::Implies(l, r) => Constraint::Implies(
            Box::new(substitute_const_in_constraint_inner(l, var, replacement)),
            Box::new(substitute_const_in_constraint_inner(r, var, replacement)),
        ),
        Constraint::Forall {
            var: bound,
            lower,
            upper,
            body,
        } => {
            if bound == var {
                c.clone()
            } else {
                Constraint::Forall {
                    var: bound.clone(),
                    lower: sub_idx(lower, var, replacement),
                    upper: sub_idx(upper, var, replacement),
                    body: Box::new(substitute_const_in_constraint_inner(body, var, replacement)),
                }
            }
        }
        Constraint::Exists {
            var: bound,
            lower,
            upper,
            body,
        } => {
            if bound == var {
                c.clone()
            } else {
                Constraint::Exists {
                    var: bound.clone(),
                    lower: sub_idx(lower, var, replacement),
                    upper: sub_idx(upper, var, replacement),
                    body: Box::new(substitute_const_in_constraint_inner(body, var, replacement)),
                }
            }
        }
    }
}

fn sub_idx(expr: &IndexExpr, var: &str, replacement: &IndexExpr) -> IndexExpr {
    match expr {
        IndexExpr::Const(_) => expr.clone(),
        IndexExpr::Var(name) if name == var => replacement.clone(),
        IndexExpr::Var(_) => expr.clone(),
        IndexExpr::Add(l, r) => IndexExpr::Add(
            Box::new(sub_idx(l, var, replacement)),
            Box::new(sub_idx(r, var, replacement)),
        ),
        IndexExpr::Sub(l, r) => IndexExpr::Sub(
            Box::new(sub_idx(l, var, replacement)),
            Box::new(sub_idx(r, var, replacement)),
        ),
        IndexExpr::Mul(l, r) => IndexExpr::Mul(
            Box::new(sub_idx(l, var, replacement)),
            Box::new(sub_idx(r, var, replacement)),
        ),
        IndexExpr::Div(l, r) => IndexExpr::Div(
            Box::new(sub_idx(l, var, replacement)),
            Box::new(sub_idx(r, var, replacement)),
        ),
        IndexExpr::Mod(l, r) => IndexExpr::Mod(
            Box::new(sub_idx(l, var, replacement)),
            Box::new(sub_idx(r, var, replacement)),
        ),
        IndexExpr::Select(name, idx) => {
            IndexExpr::Select(name.clone(), Box::new(sub_idx(idx, var, replacement)))
        }
    }
}

fn verify_mov_reg(
    dst: Reg,
    src: Reg,
    _ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let src_ty = get_register_type(src, state, block_label)?;
    let derived_ty = src_ty.clone();

    match &derived_ty {
        DtalType::SingletonInt(idx) => {
            add_register_index_constraint(dst, idx, state);
        }
        DtalType::Int
        | DtalType::I64
        | DtalType::U64
        | DtalType::RefinedInt { .. }
        | DtalType::Bool => {
            let src_idx = extract_index(&derived_ty, &src);
            add_register_index_constraint(dst, &src_idx, state);
        }
        DtalType::ExistentialInt {
            witness_var,
            constraint,
        } => {
            let dst_name = format!("{}", dst);
            let subs = std::collections::HashMap::from([(witness_var.clone(), dst_name)]);
            let opened = substitute_var_names_in_constraint(constraint, &subs);
            let opened = substitute_select_names(&opened, &subs);
            state.constraints.push(opened);
        }
        DtalType::Array { size, .. } => {
            let src_version = state.array_versions.get(&src).copied().unwrap_or(0);
            state.array_versions.insert(dst, 0);
            let src_name = versioned_array_name(&src, src_version);
            let dst_name = versioned_array_name(&dst, 0);
            state.constraints.push(Constraint::Forall {
                var: "_k".to_string(),
                lower: IndexExpr::Const(0),
                upper: size.clone(),
                body: Box::new(Constraint::Eq(
                    IndexExpr::Select(dst_name, Box::new(IndexExpr::Var("_k".to_string()))),
                    IndexExpr::Select(src_name, Box::new(IndexExpr::Var("_k".to_string()))),
                )),
            });
        }
        _ => {}
    }

    state.register_types.insert(dst, derived_ty);
    Ok(())
}

pub fn extract_index(ty: &DtalType, reg: &Reg) -> IndexExpr {
    match ty {
        DtalType::SingletonInt(idx) => idx.clone(),
        DtalType::RefinedInt { var, .. } => IndexExpr::Var(var.clone()),
        DtalType::ExistentialInt { .. } => reg_to_index_expr(reg),
        _ => reg_to_index_expr(reg),
    }
}

pub(crate) fn pointer_arithmetic_result_type(
    base_ty: &DtalType,
    annotated_ty: &DtalType,
) -> Option<DtalType> {
    match base_ty {
        DtalType::Array { element_type, .. }
        | DtalType::Ref(element_type)
        | DtalType::RefMut(element_type)
            if matches!(element_type.as_ref(), DtalType::Array { .. }) =>
        {
            let pointed_ty = element_type.as_ref();
            if let DtalType::Array { element_type, .. } = pointed_ty {
                Some(match annotated_ty {
                    DtalType::Array { .. } => annotated_ty.clone(),
                    _ => element_type.as_ref().clone(),
                })
            } else {
                None
            }
        }
        DtalType::Array { element_type, .. } => Some(match annotated_ty {
            DtalType::Array { .. } => annotated_ty.clone(),
            _ => element_type.as_ref().clone(),
        }),
        _ => None,
    }
}

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

    if op == BinaryOp::Add {
        if is_numeric_type(&rhs_ty)
            && let Some(derived_ty) = pointer_arithmetic_result_type(&lhs_ty, ty)
        {
            state.register_types.insert(dst, derived_ty);
            return Ok(());
        }
        if is_numeric_type(&lhs_ty)
            && let Some(derived_ty) = pointer_arithmetic_result_type(&rhs_ty, ty)
        {
            state.register_types.insert(dst, derived_ty);
            return Ok(());
        }
    }

    let derived_ty = match op {
        BinaryOp::And | BinaryOp::Or => {
            if !is_bool_compatible(&lhs_ty) || !is_bool_compatible(&rhs_ty) {
                return Err(VerifyError::BinOpTypeMismatch {
                    block: block_label.to_string(),
                    op: format!("{}", op),
                    lhs_type: lhs_ty,
                    rhs_type: rhs_ty,
                });
            }
            DtalType::Bool
        }
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Shl
        | BinaryOp::Shr => {
            if !is_numeric_type(&lhs_ty) || !is_numeric_type(&rhs_ty) {
                return Err(VerifyError::BinOpTypeMismatch {
                    block: block_label.to_string(),
                    op: format!("{}", op),
                    lhs_type: lhs_ty,
                    rhs_type: rhs_ty,
                });
            }

            let lhs_idx = extract_index(&lhs_ty, &lhs);
            let rhs_idx = extract_index(&rhs_ty, &rhs);

            if matches!(op, BinaryOp::Div | BinaryOp::Mod) {
                let divisor_idx = match &rhs_ty {
                    DtalType::SingletonInt(idx) => idx.clone(),
                    _ => reg_to_index_expr(&rhs),
                };
                let divisor_nonzero = Constraint::Ne(divisor_idx.clone(), IndexExpr::Const(0));
                let mut divisor_context = state.constraints.clone();
                if let DtalType::RefinedInt {
                    var, constraint, ..
                } = &rhs_ty
                {
                    let reg_name = format!("{}", rhs);
                    let subs = std::collections::HashMap::from([(var.clone(), reg_name)]);
                    divisor_context.push(substitute_var_names_in_constraint(constraint, &subs));
                }
                if !is_constraint_provable(&divisor_nonzero, &divisor_context) {
                    return Err(VerifyError::UnprovableConstraint {
                        constraint: divisor_nonzero,
                        context: divisor_context,
                        block: block_label.to_string(),
                    });
                }
            }

            match op {
                BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr => DtalType::Int,
                _ => {
                    let result_idx = match op {
                        BinaryOp::Add => IndexExpr::Add(Box::new(lhs_idx), Box::new(rhs_idx)),
                        BinaryOp::Sub => IndexExpr::Sub(Box::new(lhs_idx), Box::new(rhs_idx)),
                        BinaryOp::Mul => IndexExpr::Mul(Box::new(lhs_idx), Box::new(rhs_idx)),
                        BinaryOp::Div => IndexExpr::Div(Box::new(lhs_idx), Box::new(rhs_idx)),
                        BinaryOp::Mod => IndexExpr::Mod(Box::new(lhs_idx), Box::new(rhs_idx)),
                        _ => unreachable!(),
                    };
                    DtalType::SingletonInt(result_idx)
                }
            }
        }
    };

    let is_i64_op = matches!(ty, DtalType::I64)
        || matches!(ty, DtalType::RefinedInt { base, .. } if matches!(base.as_ref(), DtalType::I64));
    let both_have_constraints = is_concrete_const(&lhs_ty) && is_concrete_const(&rhs_ty);
    if is_i64_op
        && both_have_constraints
        && !matches!(
            op,
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::Shl | BinaryOp::Shr
        )
    {
        let lhs_reg_idx = reg_to_index_expr(&lhs);
        let rhs_reg_idx = reg_to_index_expr(&rhs);
        let overflow_idx = match op {
            BinaryOp::Add => IndexExpr::Add(Box::new(lhs_reg_idx), Box::new(rhs_reg_idx)),
            BinaryOp::Sub => IndexExpr::Sub(Box::new(lhs_reg_idx), Box::new(rhs_reg_idx)),
            BinaryOp::Mul => IndexExpr::Mul(Box::new(lhs_reg_idx), Box::new(rhs_reg_idx)),
            _ => IndexExpr::Var("_unknown".to_string()),
        };
        let overflow_ok = check_i64_overflow_constraint(&overflow_idx, &state.constraints);
        if !overflow_ok {
            return Err(VerifyError::ArithmeticOverflow {
                block: block_label.to_string(),
                op: format!("{}", op),
                context: state.constraints.clone(),
            });
        }
    }

    if let DtalType::SingletonInt(ref idx) = derived_ty {
        add_register_index_constraint(dst, idx, state);
    }

    state.register_types.insert(dst, derived_ty);
    Ok(())
}

fn is_concrete_const(ty: &DtalType) -> bool {
    matches!(ty, DtalType::SingletonInt(IndexExpr::Const(_)))
}

pub fn check_i64_overflow_constraint(result_idx: &IndexExpr, constraints: &[Constraint]) -> bool {
    let lower = Constraint::Ge(result_idx.clone(), IndexExpr::Const(i64::MIN as i128));
    let upper = Constraint::Le(result_idx.clone(), IndexExpr::Const(i64::MAX as i128));
    is_constraint_provable(&lower, constraints) && is_constraint_provable(&upper, constraints)
}

fn verify_add_imm(
    dst: Reg,
    src: Reg,
    imm: i128,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let src_ty = get_register_type(src, state, block_label)?;

    if let Some(derived_ty) = pointer_arithmetic_result_type(&src_ty, ty) {
        state.register_types.insert(dst, derived_ty);
        return Ok(());
    }

    if !is_numeric_type(&src_ty) {
        return Err(VerifyError::BinOpTypeMismatch {
            block: block_label.to_string(),
            op: "addi".to_string(),
            lhs_type: src_ty,
            rhs_type: DtalType::SingletonInt(IndexExpr::Const(imm)),
        });
    }

    let src_idx = extract_index(&src_ty, &src);
    let result_idx = IndexExpr::Add(Box::new(src_idx), Box::new(IndexExpr::Const(imm)));
    let derived_ty = DtalType::SingletonInt(result_idx.clone());

    add_register_index_constraint(dst, &result_idx, state);
    state.register_types.insert(dst, derived_ty);
    Ok(())
}

fn verify_load_op(
    dst: Reg,
    base: Reg,
    offset: Reg,
    other: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(base, state, block_label)?;
    check_register_defined(offset, state, block_label)?;
    check_register_defined(other, state, block_label)?;

    let base_ty = get_register_type(base, state, block_label)?;
    let array_view = match &base_ty {
        DtalType::Array { size, .. } => Some(size.clone()),
        DtalType::Ref(inner) | DtalType::RefMut(inner) => match inner.as_ref() {
            DtalType::Array { size, .. } => Some(size.clone()),
            _ => None,
        },
        _ => None,
    };

    if let Some(size) = array_view {
        let offset_expr = reg_to_index_expr(&offset);
        let bounds_constraint = Constraint::And(
            Box::new(Constraint::Ge(offset_expr.clone(), IndexExpr::Const(0))),
            Box::new(Constraint::Lt(offset_expr, size)),
        );

        if !is_constraint_provable(&bounds_constraint, &state.constraints) {
            return Err(VerifyError::BoundsCheckFailed {
                block: block_label.to_string(),
                instr_desc: format!("loadop {:?}, [{:?} + {:?}], {:?}", dst, base, offset, other),
                constraint: bounds_constraint,
                context: state.constraints.clone(),
            });
        }
    } else {
        return Err(VerifyError::TypeMismatch {
            block: block_label.to_string(),
            instr_desc: format!("loadop {:?}, [{:?} + {:?}], {:?}", dst, base, offset, other),
            expected: DtalType::Array {
                element_type: std::sync::Arc::new(DtalType::Int),
                size: IndexExpr::Var("?".to_string()),
            },
            actual: base_ty,
        });
    }

    state.register_types.insert(dst, ty.clone());
    Ok(())
}

fn verify_load(
    dst: Reg,
    base: Reg,
    offset: Reg,
    _ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let base_ty = get_register_type(base, state, block_label)?;
    let _offset_ty = get_register_type(offset, state, block_label)?;

    let array_view = match &base_ty {
        DtalType::Array { element_type, size } => {
            Some((element_type.as_ref().clone(), size.clone()))
        }
        DtalType::Ref(inner) | DtalType::RefMut(inner) => match inner.as_ref() {
            DtalType::Array { element_type, size } => {
                Some((element_type.as_ref().clone(), size.clone()))
            }
            _ => None,
        },
        _ => None,
    };
    if let Some((derived_ty, size)) = array_view {
        let offset_expr = reg_to_index_expr(&offset);

        let bounds_constraint = Constraint::And(
            Box::new(Constraint::Ge(offset_expr.clone(), IndexExpr::Const(0))),
            Box::new(Constraint::Lt(offset_expr.clone(), size.clone())),
        );

        if !is_constraint_provable(&bounds_constraint, &state.constraints) {
            return Err(VerifyError::BoundsCheckFailed {
                block: block_label.to_string(),
                instr_desc: format!("load {:?}, [{:?} + {:?}]", dst, base, offset),
                constraint: bounds_constraint,
                context: state.constraints.clone(),
            });
        }

        state.register_types.insert(dst, derived_ty);

        let current_version = state.array_versions.get(&base).copied().unwrap_or(0);
        let arr_name = versioned_array_name(&base, current_version);
        let dst_expr = reg_to_index_expr(&dst);
        state.constraints.push(Constraint::Eq(
            dst_expr,
            IndexExpr::Select(arr_name, Box::new(offset_expr)),
        ));
    } else {
        return Err(VerifyError::TypeMismatch {
            block: block_label.to_string(),
            instr_desc: format!("load {:?}, [{:?} + {:?}]", dst, base, offset),
            expected: DtalType::Array {
                element_type: std::sync::Arc::new(DtalType::Int),
                size: IndexExpr::Var("?".to_string()),
            },
            actual: base_ty,
        });
    }

    Ok(())
}

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

    let array_view = match &base_ty {
        DtalType::Array { element_type, size } => {
            Some((element_type.as_ref().clone(), size.clone()))
        }
        DtalType::RefMut(inner) => match inner.as_ref() {
            DtalType::Array { element_type, size } => {
                Some((element_type.as_ref().clone(), size.clone()))
            }
            _ => None,
        },
        DtalType::Ref(_) => {
            return Err(VerifyError::OwnershipViolation {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                msg: "cannot store through a shared reference".to_string(),
            });
        }
        _ => None,
    };
    if let Some((element_type, size)) = array_view {
        let offset_expr = reg_to_index_expr(&offset);

        let bounds_constraint = Constraint::And(
            Box::new(Constraint::Ge(offset_expr.clone(), IndexExpr::Const(0))),
            Box::new(Constraint::Lt(offset_expr.clone(), size.clone())),
        );

        if !is_constraint_provable(&bounds_constraint, &state.constraints) {
            return Err(VerifyError::BoundsCheckFailed {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                constraint: bounds_constraint,
                context: state.constraints.clone(),
            });
        }

        if !types_compatible_with_constraints(&src_ty, &element_type, &state.constraints) {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                expected: element_type.clone(),
                actual: src_ty.clone(),
            });
        }

        let old_version = state.array_versions.get(&base).copied().unwrap_or(0);
        let old_name = versioned_array_name(&base, old_version);
        let new_version = old_version + 1;
        let new_name = versioned_array_name(&base, new_version);
        state.array_versions.insert(base, new_version);

        let src_idx = extract_index(&src_ty, &src);

        state.constraints.push(Constraint::Eq(
            IndexExpr::Select(new_name.clone(), Box::new(offset_expr.clone())),
            src_idx,
        ));

        state.constraints.push(Constraint::Forall {
            var: "_k".to_string(),
            lower: IndexExpr::Const(0),
            upper: size.clone(),
            body: Box::new(Constraint::Implies(
                Box::new(Constraint::Ne(
                    IndexExpr::Var("_k".to_string()),
                    offset_expr,
                )),
                Box::new(Constraint::Eq(
                    IndexExpr::Select(new_name, Box::new(IndexExpr::Var("_k".to_string()))),
                    IndexExpr::Select(old_name, Box::new(IndexExpr::Var("_k".to_string()))),
                )),
            )),
        });
    }

    Ok(())
}

fn versioned_array_name(reg: &Reg, version: u32) -> String {
    format!("{}_{}", reg, version)
}

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

fn verify_cmp_imm(
    lhs: Reg,
    imm: i128,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(lhs, state, block_label)?;
    state.last_cmp = Some(CmpOperands::RegImm(lhs, imm));
    Ok(())
}

fn verify_not(
    dst: Reg,
    src: Reg,
    _ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(src, state, block_label)?;
    state.register_types.insert(dst, DtalType::Bool);
    Ok(())
}

fn verify_neg(
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

fn verify_shift_imm(
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

fn verify_type_annotation(
    reg: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let Some(existing_ty) = state.register_types.get(&reg) {
        let is_existential_narrowing = matches!(ty, DtalType::ExistentialInt { .. })
            && matches!(
                existing_ty,
                DtalType::Int
                    | DtalType::I64
                    | DtalType::U64
                    | DtalType::SingletonInt(_)
                    | DtalType::ExistentialInt { .. }
            );

        let is_pointer_refinement =
            matches!(existing_ty, DtalType::Int | DtalType::I64 | DtalType::U64)
                && matches!(
                    ty,
                    DtalType::Array { .. }
                        | DtalType::Ref(_)
                        | DtalType::RefMut(_)
                        | DtalType::Master(_)
                );

        let is_prologue_refinement = matches!(
            existing_ty,
            DtalType::Int | DtalType::I64 | DtalType::U64 | DtalType::SingletonInt(_)
        ) && matches!(reg, Reg::Physical(_));

        if !is_existential_narrowing
            && !is_pointer_refinement
            && !is_prologue_refinement
            && !types_compatible_with_constraints(existing_ty, ty, &state.constraints)
        {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("type_annotation {:?} : {}", reg, ty),
                expected: ty.clone(),
                actual: existing_ty.clone(),
            });
        }
    }
    state.register_types.insert(reg, ty.clone());

    if let DtalType::RefinedInt {
        var, constraint, ..
    } = ty
    {
        let reg_name = format!("{}", reg);
        let subs = std::collections::HashMap::from([(var.clone(), reg_name)]);
        let projected = substitute_var_names_in_constraint(constraint, &subs);
        state.constraints.push(projected);
    }

    Ok(())
}

fn verify_constraint_assert(
    constraint: &Constraint,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    let mut full_context = state.constraints.clone();
    full_context.extend(state.proven_assertions.iter().cloned());

    if !is_constraint_provable(constraint, &full_context) {
        return Err(VerifyError::UnprovableConstraint {
            constraint: constraint.clone(),
            context: state.constraints.clone(),
            block: block_label.to_string(),
        });
    }
    state.constraints.push(constraint.clone());
    Ok(())
}

fn get_register_type(
    reg: Reg,
    state: &TypeState,
    block_label: &str,
) -> Result<DtalType, VerifyError> {
    check_register_defined(reg, state, block_label)?;
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
    if state.consumed_registers.contains(&reg) {
        return Err(VerifyError::ConsumedRegister {
            reg,
            block: block_label.to_string(),
        });
    }
    if state.register_types.contains_key(&reg) {
        Ok(())
    } else {
        Err(VerifyError::UndefinedRegister {
            reg,
            block: block_label.to_string(),
        })
    }
}

pub(crate) fn is_numeric_type(ty: &DtalType) -> bool {
    matches!(
        ty,
        DtalType::Int
            | DtalType::I64
            | DtalType::U64
            | DtalType::SingletonInt(_)
            | DtalType::RefinedInt { .. }
            | DtalType::ExistentialInt { .. }
    )
}

fn is_bool_compatible(ty: &DtalType) -> bool {
    matches!(
        ty,
        DtalType::Bool
            | DtalType::SingletonInt(IndexExpr::Const(0))
            | DtalType::SingletonInt(IndexExpr::Const(1))
    )
}

pub fn types_compatible_with_constraints(
    actual: &DtalType,
    expected: &DtalType,
    constraints: &[Constraint],
) -> bool {
    match (actual, expected) {
        (DtalType::Int, DtalType::Int) => true,
        (DtalType::I64, DtalType::I64) => true,
        (DtalType::U64, DtalType::U64) => true,
        (DtalType::I64, DtalType::Int) => true,
        (DtalType::U64, DtalType::Int) => true,
        (DtalType::Bool, DtalType::Bool) => true,
        (DtalType::Unit, DtalType::Unit) => true,
        (DtalType::SingletonInt(a), DtalType::SingletonInt(b)) => {
            a == b || is_constraint_provable(&Constraint::Eq(a.clone(), b.clone()), constraints)
        }
        (DtalType::SingletonInt(_), DtalType::Int) => true,
        (DtalType::SingletonInt(_), DtalType::I64) => true,
        (DtalType::SingletonInt(_), DtalType::U64) => true,
        (DtalType::SingletonInt(IndexExpr::Const(0 | 1)), DtalType::Bool) => true,
        (DtalType::Bool, DtalType::SingletonInt(IndexExpr::Const(0 | 1))) => true,
        (DtalType::RefinedInt { .. }, DtalType::Int) => true,
        (DtalType::RefinedInt { base, .. }, DtalType::I64) => {
            types_compatible_with_constraints(base, &DtalType::I64, constraints)
        }
        (DtalType::RefinedInt { base, .. }, DtalType::U64) => {
            types_compatible_with_constraints(base, &DtalType::U64, constraints)
        }
        (DtalType::SingletonInt(_), DtalType::RefinedInt { base, .. }) => {
            types_compatible_with_constraints(actual, base.as_ref(), constraints)
        }
        (
            DtalType::RefinedInt {
                base: b1,
                var: v1,
                constraint: c1,
            },
            DtalType::RefinedInt {
                base: b2,
                var: v2,
                constraint: c2,
            },
        ) => {
            (v1 == v2 && c1 == c2 && types_compatible_with_constraints(b1, b2, constraints))
                || types_compatible_with_constraints(b1, b2, constraints)
        }
        (
            DtalType::SingletonInt(idx),
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            },
        ) => {
            let witness_eq = Constraint::Eq(IndexExpr::Var(witness_var.clone()), idx.clone());
            let mut augmented_ctx: Vec<Constraint> = constraints.to_vec();
            augmented_ctx.push(witness_eq);
            is_constraint_provable(constraint, &augmented_ctx)
        }
        (DtalType::ExistentialInt { .. }, DtalType::Int) => true,
        (DtalType::ExistentialInt { .. }, DtalType::I64) => true,
        (DtalType::ExistentialInt { .. }, DtalType::U64) => true,
        (
            DtalType::ExistentialInt {
                witness_var: w,
                constraint: c,
            },
            DtalType::RefinedInt {
                base: _,
                var: rv,
                constraint: rc,
            },
        ) => {
            let subs: std::collections::HashMap<String, String> =
                std::collections::HashMap::from([(rv.clone(), w.clone())]);
            let rc_renamed = substitute_var_names_in_constraint(rc, &subs);
            is_constraint_provable(
                &Constraint::Implies(Box::new(c.clone()), Box::new(rc_renamed)),
                constraints,
            )
        }
        (
            DtalType::ExistentialInt {
                witness_var: w1,
                constraint: c1,
            },
            DtalType::ExistentialInt {
                witness_var: w2,
                constraint: c2,
            },
        ) => {
            if w1 == w2 && c1 == c2 {
                return true;
            }
            let fresh = "_existential_check".to_string();
            let subs1: std::collections::HashMap<String, String> =
                std::collections::HashMap::from([(w1.clone(), fresh.clone())]);
            let subs2: std::collections::HashMap<String, String> =
                std::collections::HashMap::from([(w2.clone(), fresh.clone())]);
            let c1_sub = substitute_var_names_in_constraint(c1, &subs1);
            let c2_sub = substitute_var_names_in_constraint(c2, &subs2);
            is_constraint_provable(
                &Constraint::Implies(Box::new(c1_sub), Box::new(c2_sub)),
                constraints,
            )
        }
        (
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            },
            DtalType::SingletonInt(k),
        ) => {
            let eq_constraint = Constraint::Eq(IndexExpr::Var(witness_var.clone()), k.clone());
            let implication =
                Constraint::Implies(Box::new(constraint.clone()), Box::new(eq_constraint));
            is_constraint_provable(&implication, constraints)
        }
        (DtalType::Int, DtalType::ExistentialInt { .. }) => false,
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
        ) => {
            types_compatible_with_constraints(e1.as_ref(), e2.as_ref(), constraints)
                && (s1 == s2
                    || is_constraint_provable(&Constraint::Eq(s1.clone(), s2.clone()), constraints))
        }
        (DtalType::Ref(a), DtalType::Ref(b)) => {
            types_compatible_with_constraints(a.as_ref(), b.as_ref(), constraints)
                && types_compatible_with_constraints(b.as_ref(), a.as_ref(), constraints)
        }
        (DtalType::RefMut(a), DtalType::RefMut(b)) => {
            types_compatible_with_constraints(a.as_ref(), b.as_ref(), constraints)
                && types_compatible_with_constraints(b.as_ref(), a.as_ref(), constraints)
        }
        (DtalType::Master(a), DtalType::Master(b)) => {
            types_compatible_with_constraints(a.as_ref(), b.as_ref(), constraints)
                && types_compatible_with_constraints(b.as_ref(), a.as_ref(), constraints)
        }
        _ => false,
    }
}

#[allow(dead_code)]
pub fn types_compatible(actual: &DtalType, expected: &DtalType) -> bool {
    types_compatible_with_constraints(actual, expected, &[])
}

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

    crate::verifier::smt::ConstraintOracle::is_provable(goal, context)
}

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

pub fn reg_to_index_expr(reg: &Reg) -> IndexExpr {
    IndexExpr::Var(format!("{}", reg))
}

fn add_register_index_constraint(reg: Reg, idx: &IndexExpr, state: &mut TypeState) {
    let reg_expr = reg_to_index_expr(&reg);
    if *idx != reg_expr {
        state
            .constraints
            .push(Constraint::Eq(reg_expr, idx.clone()));
    }
}

pub fn version_substitute_constraint(
    constraint: &Constraint,
    array_versions: &std::collections::HashMap<Reg, u32>,
) -> Constraint {
    let subs: std::collections::HashMap<String, String> = array_versions
        .iter()
        .map(|(reg, ver)| (format!("{}", reg), format!("{}_{}", reg, ver)))
        .collect();
    substitute_select_names(constraint, &subs)
}

pub fn substitute_select_names(
    constraint: &Constraint,
    subs: &std::collections::HashMap<String, String>,
) -> Constraint {
    match constraint {
        Constraint::True | Constraint::False => constraint.clone(),
        Constraint::Eq(l, r) => Constraint::Eq(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::Lt(l, r) => Constraint::Lt(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::Le(l, r) => Constraint::Le(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::Gt(l, r) => Constraint::Gt(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::Ge(l, r) => Constraint::Ge(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::Ne(l, r) => Constraint::Ne(
            substitute_select_in_index(l, subs),
            substitute_select_in_index(r, subs),
        ),
        Constraint::And(l, r) => Constraint::And(
            Box::new(substitute_select_names(l, subs)),
            Box::new(substitute_select_names(r, subs)),
        ),
        Constraint::Or(l, r) => Constraint::Or(
            Box::new(substitute_select_names(l, subs)),
            Box::new(substitute_select_names(r, subs)),
        ),
        Constraint::Not(c) => Constraint::Not(Box::new(substitute_select_names(c, subs))),
        Constraint::Implies(l, r) => Constraint::Implies(
            Box::new(substitute_select_names(l, subs)),
            Box::new(substitute_select_names(r, subs)),
        ),
        Constraint::Forall {
            var,
            lower,
            upper,
            body,
        } => Constraint::Forall {
            var: var.clone(),
            lower: substitute_select_in_index(lower, subs),
            upper: substitute_select_in_index(upper, subs),
            body: Box::new(substitute_select_names(body, subs)),
        },
        Constraint::Exists {
            var,
            lower,
            upper,
            body,
        } => Constraint::Exists {
            var: var.clone(),
            lower: substitute_select_in_index(lower, subs),
            upper: substitute_select_in_index(upper, subs),
            body: Box::new(substitute_select_names(body, subs)),
        },
    }
}

fn substitute_select_in_index(
    expr: &IndexExpr,
    subs: &std::collections::HashMap<String, String>,
) -> IndexExpr {
    match expr {
        IndexExpr::Const(_) | IndexExpr::Var(_) => expr.clone(),
        IndexExpr::Add(l, r) => IndexExpr::Add(
            Box::new(substitute_select_in_index(l, subs)),
            Box::new(substitute_select_in_index(r, subs)),
        ),
        IndexExpr::Sub(l, r) => IndexExpr::Sub(
            Box::new(substitute_select_in_index(l, subs)),
            Box::new(substitute_select_in_index(r, subs)),
        ),
        IndexExpr::Mul(l, r) => IndexExpr::Mul(
            Box::new(substitute_select_in_index(l, subs)),
            Box::new(substitute_select_in_index(r, subs)),
        ),
        IndexExpr::Div(l, r) => IndexExpr::Div(
            Box::new(substitute_select_in_index(l, subs)),
            Box::new(substitute_select_in_index(r, subs)),
        ),
        IndexExpr::Mod(l, r) => IndexExpr::Mod(
            Box::new(substitute_select_in_index(l, subs)),
            Box::new(substitute_select_in_index(r, subs)),
        ),
        IndexExpr::Select(name, idx) => {
            let new_name = subs.get(name).cloned().unwrap_or_else(|| name.clone());
            IndexExpr::Select(new_name, Box::new(substitute_select_in_index(idx, subs)))
        }
    }
}

pub fn substitute_var_names_in_constraint(
    constraint: &Constraint,
    subs: &std::collections::HashMap<String, String>,
) -> Constraint {
    match constraint {
        Constraint::True | Constraint::False => constraint.clone(),
        Constraint::Eq(l, r) => Constraint::Eq(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::Lt(l, r) => Constraint::Lt(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::Le(l, r) => Constraint::Le(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::Gt(l, r) => Constraint::Gt(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::Ge(l, r) => Constraint::Ge(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::Ne(l, r) => Constraint::Ne(
            substitute_var_in_index(l, subs),
            substitute_var_in_index(r, subs),
        ),
        Constraint::And(l, r) => Constraint::And(
            Box::new(substitute_var_names_in_constraint(l, subs)),
            Box::new(substitute_var_names_in_constraint(r, subs)),
        ),
        Constraint::Or(l, r) => Constraint::Or(
            Box::new(substitute_var_names_in_constraint(l, subs)),
            Box::new(substitute_var_names_in_constraint(r, subs)),
        ),
        Constraint::Not(c) => {
            Constraint::Not(Box::new(substitute_var_names_in_constraint(c, subs)))
        }
        Constraint::Implies(l, r) => Constraint::Implies(
            Box::new(substitute_var_names_in_constraint(l, subs)),
            Box::new(substitute_var_names_in_constraint(r, subs)),
        ),
        Constraint::Forall {
            var,
            lower,
            upper,
            body,
        } => {
            let filtered: std::collections::HashMap<String, String> = subs
                .iter()
                .filter(|(k, _)| *k != var)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Constraint::Forall {
                var: var.clone(),
                lower: substitute_var_in_index(lower, subs),
                upper: substitute_var_in_index(upper, subs),
                body: Box::new(substitute_var_names_in_constraint(body, &filtered)),
            }
        }
        Constraint::Exists {
            var,
            lower,
            upper,
            body,
        } => {
            let filtered: std::collections::HashMap<String, String> = subs
                .iter()
                .filter(|(k, _)| *k != var)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Constraint::Exists {
                var: var.clone(),
                lower: substitute_var_in_index(lower, subs),
                upper: substitute_var_in_index(upper, subs),
                body: Box::new(substitute_var_names_in_constraint(body, &filtered)),
            }
        }
    }
}

fn substitute_var_in_index(
    expr: &IndexExpr,
    subs: &std::collections::HashMap<String, String>,
) -> IndexExpr {
    match expr {
        IndexExpr::Const(_) => expr.clone(),
        IndexExpr::Var(name) => {
            if let Some(new_name) = subs.get(name) {
                IndexExpr::Var(new_name.clone())
            } else {
                expr.clone()
            }
        }
        IndexExpr::Add(l, r) => IndexExpr::Add(
            Box::new(substitute_var_in_index(l, subs)),
            Box::new(substitute_var_in_index(r, subs)),
        ),
        IndexExpr::Sub(l, r) => IndexExpr::Sub(
            Box::new(substitute_var_in_index(l, subs)),
            Box::new(substitute_var_in_index(r, subs)),
        ),
        IndexExpr::Mul(l, r) => IndexExpr::Mul(
            Box::new(substitute_var_in_index(l, subs)),
            Box::new(substitute_var_in_index(r, subs)),
        ),
        IndexExpr::Div(l, r) => IndexExpr::Div(
            Box::new(substitute_var_in_index(l, subs)),
            Box::new(substitute_var_in_index(r, subs)),
        ),
        IndexExpr::Mod(l, r) => IndexExpr::Mod(
            Box::new(substitute_var_in_index(l, subs)),
            Box::new(substitute_var_in_index(r, subs)),
        ),
        IndexExpr::Select(name, idx) => {
            let new_name = subs.get(name).cloned().unwrap_or_else(|| name.clone());
            IndexExpr::Select(new_name, Box::new(substitute_var_in_index(idx, subs)))
        }
    }
}

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

pub fn negate_cmp_op_constraint(op: CmpOp, last_cmp: &Option<CmpOperands>) -> Option<Constraint> {
    constraint_from_cmp_op(negate_cmp_op(op), last_cmp)
}
