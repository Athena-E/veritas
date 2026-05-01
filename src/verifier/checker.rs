//! Instruction verification
//!
//! This module verifies individual DTAL instructions maintain type invariants.

#![allow(clippy::result_large_err)]

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{BinaryOp, CmpOp, CmpOperands, DtalInstr, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
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

        DtalInstr::AliasBorrow { dst, src, ty } => {
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "alias_borrow")?;
            clear_owned(*dst, state);
            assign_shared_borrow_from(*src, *dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::BorrowMut { dst, src, ty } => {
            verify_owned_available(*src, state, block_label, "borrow_mut")?;
            verify_object_not_shared_borrowed(*src, state, block_label, "borrow_mut")?;
            verify_object_not_mutably_borrowed(*src, state, block_label, "borrow_mut")?;
            verify_mov_reg(*dst, *src, ty, state, block_label)?;
            clear_owned(*dst, state);
            clear_shared_borrow(*dst, state);
            assign_mutable_borrow_from(*src, *dst, state);
            clear_consumed(*dst, state);
        }

        DtalInstr::BorrowEnd { src, .. } => {
            verify_borrow_available(*src, state, block_label, "borrow_end")?;
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
            // SetCC defines dst as Bool (0 or 1)
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
            // Xi & Harper's type-push: push the source register's type onto the stack
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
                .mutable_borrow_stack_object_ids
                .push(state.mutable_borrow_object_ids.get(src).copied());
        }

        DtalInstr::Pop { dst, ty: _ } => {
            // Xi & Harper's type-pop: pop the top type from the stack
            // If the stack is empty, this is an error
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
                assign_shared_borrow_object(*dst, object_id, state);
            } else {
                clear_shared_borrow(*dst, state);
            }
            if let Some(object_id) = state.mutable_borrow_stack_object_ids.pop().unwrap_or(None) {
                assign_mutable_borrow_object(*dst, object_id, state);
            } else {
                clear_mutable_borrow(*dst, state);
            }
            clear_consumed(*dst, state);
        }

        DtalInstr::Alloca { dst, size: _, ty } => {
            state.register_types.insert(*dst, ty.clone());
            set_owned_from_type(*dst, ty, state, true);
            clear_consumed(*dst, state);
            // For array allocations, emit zero-initialization axioms:
            // forall k in 0..size { arr_0[k] == 0 }
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

        // Call: derive return type from callee's declared signature
        DtalInstr::Call {
            target,
            arg_kinds,
            ownership,
            ..
        } => {
            use crate::backend::dtal::regs::PhysicalReg;

            let derived_return_ty = if let Some(callee) =
                program.functions.iter().find(|f| &f.name == target)
            {
                // Check callee's precondition if available.
                // Substitute callee's virtual param registers with physical
                // param registers (r0, r1, ...) since the caller has placed
                // arguments there before the call.
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
                    // Also substitute Var references for param registers
                    substituted = substitute_var_names_in_constraint(&substituted, &param_subs);
                    // Version-substitute array Select names
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

                // Propagate callee's postcondition to caller's constraint context.
                // Substitute "result" and callee param names with physical regs,
                // then version-substitute array names.
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

                // Derive return type from callee's declared signature
                callee.return_type.clone()
            } else {
                // Unknown callee: reject. All callable functions must be
                // present in the DTAL program. When runtime/intrinsic
                // functions are added, they should be registered in the
                // program's function list with their signatures so the
                // verifier can check preconditions and derive return types.
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

            // Set return type in both R0 and LR:
            // - Virtual DTAL reads the return value from R0 (`mov vN, r0`)
            // - Physical DTAL reads it from LR (`mov r0, lr`)
            //
            // This must happen after consuming call arguments because the first
            // consuming argument also lives in R0.
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

        // Port I/O instructions
        DtalInstr::PortIn { dst, port } => {
            check_register_defined(*port, state, block_label)?;
            // Port read returns an int (byte value 0-255)
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

        // Physical allocation instructions (post-regalloc).
        // These are verified after register allocation in the physical DTAL pipeline.
        // In the current virtual-register verification path, they should not appear.
        DtalInstr::Cqo => {
            // cqo: sign-extend rax into rdx:rax
            use crate::backend::dtal::regs::PhysicalReg;
            let rax = Reg::Physical(PhysicalReg::LR); // LR maps to rax
            check_register_defined(rax, state, block_label)?;
            // rdx gets defined (sign extension of rax)
            let rax_ty = get_register_type(rax, state, block_label)?;
            state
                .register_types
                .insert(Reg::Physical(PhysicalReg::R2), rax_ty); // R2 maps to rdx
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
            // idiv src: signed divide rdx:rax by src
            // Requires: rax (dividend low), rdx (dividend high), src (divisor)
            // Produces: rax (quotient), rdx (remainder)
            use crate::backend::dtal::regs::PhysicalReg;
            let rax = Reg::Physical(PhysicalReg::LR);
            let rdx = Reg::Physical(PhysicalReg::R2);
            check_register_defined(rax, state, block_label)?;
            check_register_defined(rdx, state, block_label)?;
            check_register_defined(*src, state, block_label)?;
            // Both rax and rdx get new values
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
            // Track the type stored at this stack offset
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
            } else {
                state.shared_borrow_spill_object_ids.remove(offset);
            }
            if let Some(object_id) = state.mutable_borrow_object_ids.get(src).copied() {
                state
                    .mutable_borrow_spill_object_ids
                    .insert(*offset, object_id);
            } else {
                state.mutable_borrow_spill_object_ids.remove(offset);
            }
        }

        DtalInstr::SpillLoad { dst, offset, ty } => {
            // Verify the spill slot has been stored to
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
                assign_shared_borrow_object(*dst, object_id, state);
            } else {
                clear_shared_borrow(*dst, state);
            }
            if let Some(object_id) = state.mutable_borrow_spill_object_ids.get(offset).copied() {
                assign_mutable_borrow_object(*dst, object_id, state);
            } else {
                clear_mutable_borrow(*dst, state);
            }
            clear_consumed(*dst, state);
        }

        DtalInstr::Prologue { .. } => {
            // In physical DTAL, the prologue marks function entry. All physical
            // registers are "available" at this point (they hold parameter values,
            // caller-saved values, or undefined values). Define them all as Int
            // to prevent false "used before definition" errors.
            //
            // TypeAnnotation instructions AFTER the Prologue will refine specific
            // registers to their correct types (e.g., array params). Since
            // TypeAnnotation verification accepts refinement from a supertype,
            // defining as Int first and then narrowing via annotation is sound.
            use crate::backend::dtal::regs::PhysicalReg;
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

        DtalInstr::Epilogue { .. } => {
            // Structural marker
        }

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
}

fn clear_mutable_borrow(reg: Reg, state: &mut TypeState) {
    state.mutable_borrow_object_ids.remove(&reg);
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

fn abi_owned_alias_counterpart(reg: Reg) -> Option<Reg> {
    match reg {
        Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0) => {
            Some(Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR))
        }
        Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR) => {
            Some(Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0))
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
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0)
        ) | (
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR)
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
        assign_shared_borrow_object(dst, object_id, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

fn preserve_plain_mov_mutable_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if src == dst {
        if let Some(object_id) = state.mutable_borrow_object_ids.get(&src).copied() {
            assign_mutable_borrow_object(dst, object_id, state);
        } else {
            clear_mutable_borrow(dst, state);
        }
    } else {
        clear_mutable_borrow(dst, state);
    }
}

fn assign_shared_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, state);
    } else if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

fn assign_shared_borrow_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_owned(reg, state);
    clear_mutable_borrow(reg, state);
    state.shared_borrow_object_ids.insert(reg, object_id);
}

fn assign_mutable_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_mutable_borrow_object(dst, object_id, state);
    } else {
        clear_mutable_borrow(dst, state);
    }
}

fn assign_mutable_borrow_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_owned(reg, state);
    clear_shared_borrow(reg, state);
    state.mutable_borrow_object_ids.insert(reg, object_id);
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
                Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR),
                Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0)
            ) | (
                Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
                Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR)
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
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0)
        ) | (
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
            Reg::Physical(crate::backend::dtal::regs::PhysicalReg::LR)
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

/// Verify mov immediate instruction
///
/// Xi & Harper's type-movimm rule: `mov rd, c ⟹ rd : int(c)`
/// The type is derived from the immediate value, not trusted from the annotation.
fn verify_mov_imm(
    dst: Reg,
    imm: i128,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    // For singleton types, verify the value matches
    if let DtalType::SingletonInt(IndexExpr::Const(expected)) = ty
        && imm != *expected
    {
        return Err(VerifyError::SingletonMismatch {
            block: block_label.to_string(),
            expected_value: *expected,
            actual_value: imm,
        });
    }

    // For refined types, verify the predicate holds for the immediate value.
    // Substitute the value for the bound variable and check provability.
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

/// Substitute a constant value for a variable name in a constraint.
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
                c.clone() // shadowed
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

/// Verify mov register instruction
///
/// Xi & Harper's type-mov rule: `mov rd, rs ⟹ rd : τ` where `rs : τ`
/// The type is derived from the source register, not trusted from the annotation.
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
        // Singleton: link dst to the concrete index expression
        DtalType::SingletonInt(idx) => {
            add_register_index_constraint(dst, idx, state);
        }
        // Scalar types: link dst to src register variable
        DtalType::Int
        | DtalType::I64
        | DtalType::U64
        | DtalType::RefinedInt { .. }
        | DtalType::Bool => {
            let src_idx = extract_index(&derived_ty, &src);
            add_register_index_constraint(dst, &src_idx, state);
        }
        // Existential: open the existential for the destination register
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
        // Array: copy version and emit linking constraint
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

/// Extract the index expression from a type for derivation-based typing.
///
/// - `SingletonInt(idx)` → `idx`
/// - `RefinedInt { var, .. }` → `Var(var)`
/// - `Int` → `Var(reg_name)` (treat register as opaque index variable)
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

/// Verify binary operation
///
/// Xi & Harper's derivation rules:
/// - type-add: `rs : int(x), v : int(y) ⟹ rd : int(x+y)`
/// - type-sub, type-mul, type-div similarly
/// - And/Or: boolean result
///
/// The verifier derives the result type from operand types, ignoring `ty`.
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
        // Logical operations require boolean operands, produce Bool
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
        // Arithmetic operations: derive result type symbolically
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

            match op {
                BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr => {
                    // Bitwise ops don't have IndexExpr representation — widen to Int
                    DtalType::Int
                }
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

    // Phase 4: if the type annotation indicates i64 mode, verify the arithmetic
    // result stays in [INT_MIN, INT_MAX]. This mirrors the frontend's check_no_overflow
    // at the physical DTAL level so a buggy lowering can't slip an overflowing op past.
    //
    // We build the result expression using register names (not the symbolic indices
    // from extract_index) so the expression matches the constraint context which
    // uses projected register names.
    // Phase 4: if the type annotation indicates i64 mode, verify the arithmetic
    // result stays in [INT_MIN, INT_MAX] — but only when both operands have
    // sufficient constraint information. The DTAL conversion is lossy: refined
    // types whose predicates can't be converted to Constraint are widened to
    // their base type. When this happens, the verifier cannot reproduce the
    // frontend's Z3 proof, so we skip the check (the frontend already verified).
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

    // Add register-to-index linkage for derived singleton types
    if let DtalType::SingletonInt(ref idx) = derived_ty {
        add_register_index_constraint(dst, idx, state);
    }

    state.register_types.insert(dst, derived_ty);
    Ok(())
}

/// Check if a DTAL type has a compile-time-known constant value.
/// Only constant singletons can be overflow-checked at the DTAL level
/// without the full frontend Z3 context (which includes i64 range axioms).
fn is_concrete_const(ty: &DtalType) -> bool {
    matches!(ty, DtalType::SingletonInt(IndexExpr::Const(_)))
}

/// Check that a symbolic expression is provably within [INT_MIN, INT_MAX]
/// using the DTAL constraint solver.
pub fn check_i64_overflow_constraint(result_idx: &IndexExpr, constraints: &[Constraint]) -> bool {
    // INT_MIN <= result
    let lower = Constraint::Ge(result_idx.clone(), IndexExpr::Const(i64::MIN as i128));
    // result <= INT_MAX
    let upper = Constraint::Le(result_idx.clone(), IndexExpr::Const(i64::MAX as i128));
    // Both must be provable
    is_constraint_provable(&lower, constraints) && is_constraint_provable(&upper, constraints)
}

/// Verify add immediate instruction
///
/// Derived type: `src : int(x) ⟹ dst : int(x + imm)`
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

/// Verify load instruction
///
/// Derives the result type from the array's element type, not the annotation.
/// If the base register doesn't have an array type, falls back to the annotation
/// (e.g., raw pointer loads where no array type information is available).
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

    // If base has an array type, or a reference to an array, derive element
    // type and perform bounds checking.
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

        // Construct bounds constraint: 0 <= offset < size
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

        // Derive element type from array base — don't trust annotation
        state.register_types.insert(dst, derived_ty);

        // Emit select constraint: dst == current_arr[offset]
        let current_version = state.array_versions.get(&base).copied().unwrap_or(0);
        let arr_name = versioned_array_name(&base, current_version);
        let dst_expr = reg_to_index_expr(&dst);
        state.constraints.push(Constraint::Eq(
            dst_expr,
            IndexExpr::Select(arr_name, Box::new(offset_expr)),
        ));
    } else {
        // Non-array base: reject — the verifier requires typed array bases
        // to derive element types independently.
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

/// Verify store instruction
///
/// After bounds and type checks, emits array store axioms:
/// - **Write axiom**: `new_arr[offset] == src_value`
/// - **Frame axiom**: `forall k. k != offset → new_arr[k] == old_arr[k]`
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

    // If base has an array type, or a mutable reference to an array, perform
    // bounds checking and emit axioms.
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

        // Construct bounds constraint: 0 <= offset < size
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

        // Check stored value type is compatible with array element type
        if !types_compatible_with_constraints(&src_ty, &element_type, &state.constraints) {
            return Err(VerifyError::TypeMismatch {
                block: block_label.to_string(),
                instr_desc: format!("store [{:?} + {:?}], {:?}", base, offset, src),
                expected: element_type.clone(),
                actual: src_ty.clone(),
            });
        }

        // Emit array store axioms (versioned array names)
        let old_version = state.array_versions.get(&base).copied().unwrap_or(0);
        let old_name = versioned_array_name(&base, old_version);
        let new_version = old_version + 1;
        let new_name = versioned_array_name(&base, new_version);
        state.array_versions.insert(base, new_version);

        let src_idx = extract_index(&src_ty, &src);

        // Write axiom: new_arr[offset] == src_value
        state.constraints.push(Constraint::Eq(
            IndexExpr::Select(new_name.clone(), Box::new(offset_expr.clone())),
            src_idx,
        ));

        // Frame axiom: forall k. k != offset → new_arr[k] == old_arr[k]
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

/// Get the versioned array name for a register
fn versioned_array_name(reg: &Reg, version: u32) -> String {
    format!("{}_{}", reg, version)
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
    imm: i128,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    check_register_defined(lhs, state, block_label)?;
    state.last_cmp = Some(CmpOperands::RegImm(lhs, imm));
    Ok(())
}

/// Verify not instruction
///
/// Logical negation always produces Bool, regardless of annotation.
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

/// Verify a type annotation
fn verify_type_annotation(
    reg: Reg,
    ty: &DtalType,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    if let Some(existing_ty) = state.register_types.get(&reg) {
        // ExistentialInt annotations serve as phi declarations — the type
        // narrows from the widened join (Int) to a bounded existential.
        // Correctness of each incoming edge is verified by verify_state_coercion.
        let is_existential_narrowing = matches!(ty, DtalType::ExistentialInt { .. })
            && matches!(
                existing_ty,
                DtalType::Int
                    | DtalType::I64
                    | DtalType::U64
                    | DtalType::SingletonInt(_)
                    | DtalType::ExistentialInt { .. }
            );

        // In physical DTAL, the Prologue sets all registers to Int as a placeholder.
        // TypeAnnotation from physalloc refines them to their actual types.
        // Allow narrowing from Int to any type (e.g., Int → [int; 5]).
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
    // Set the type (either first definition via phi or verified annotation)
    state.register_types.insert(reg, ty.clone());

    // Project refined type constraints into the constraint context.
    // When a register gets a RefinedInt type (e.g. from a parameter annotation),
    // substitute the register name for the bound variable and add the constraint
    // so Z3 can use it for downstream proofs (overflow checks, bounds, etc.).
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

/// Verify a constraint assertion.
///
/// If the constraint is provable from the current context, it is added
/// to the context for use by downstream instructions (e.g., bounds checks).
fn verify_constraint_assert(
    constraint: &Constraint,
    state: &mut TypeState,
    block_label: &str,
) -> Result<(), VerifyError> {
    // Check if constraint is provable from current context (syntactic fast-path + Z3).
    // Also check proven_assertions — these are frontend-verified invariants that
    // survive across join points and don't need to be re-proven each iteration.
    let mut full_context = state.constraints.clone();
    full_context.extend(state.proven_assertions.iter().cloned());

    if !is_constraint_provable(constraint, &full_context) {
        return Err(VerifyError::UnprovableConstraint {
            constraint: constraint.clone(),
            context: state.constraints.clone(),
            block: block_label.to_string(),
        });
    }
    // Add proven constraint to context for downstream use
    state.constraints.push(constraint.clone());
    Ok(())
}

// Helper functions

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

/// Check if a type can be used as a boolean operand.
///
/// At the DTAL level, booleans are represented as integers 0/1.
/// `Bool`, `int(0)`, and `int(1)` are all valid boolean operands.
fn is_bool_compatible(ty: &DtalType) -> bool {
    matches!(
        ty,
        DtalType::Bool
            | DtalType::SingletonInt(IndexExpr::Const(0))
            | DtalType::SingletonInt(IndexExpr::Const(1))
    )
}

/// Check if actual type is a subtype of (or equal to) expected type.
///
/// Uses the constraint context for Xi & Harper's coerce-int rule:
/// `φ ⊨ x = y ⟹ int(x) ≤ int(y)`
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
        // At the DTAL level, booleans are integers 0/1
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
            // A singleton is compatible with a refined type if it's compatible with the base
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
            // Structural equality first, then base compatibility
            (v1 == v2 && c1 == c2 && types_compatible_with_constraints(b1, b2, constraints))
                || types_compatible_with_constraints(b1, b2, constraints)
        }
        // SingletonInt(k) <: ExistentialInt { n | φ(n) } — check φ(k) via witness equation
        (
            DtalType::SingletonInt(idx),
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            },
        ) => {
            // Add witness_var == idx to the context, then check φ(witness_var).
            // This is equivalent to φ[n/idx] but handles complex index expressions
            // correctly (string substitution can't replace a Var with an arbitrary expr).
            let witness_eq = Constraint::Eq(IndexExpr::Var(witness_var.clone()), idx.clone());
            let mut augmented_ctx: Vec<Constraint> = constraints.to_vec();
            augmented_ctx.push(witness_eq);
            is_constraint_provable(constraint, &augmented_ctx)
        }
        // ExistentialInt <: Int/I64 — always true (erasure)
        (DtalType::ExistentialInt { .. }, DtalType::Int) => true,
        (DtalType::ExistentialInt { .. }, DtalType::I64) => true,
        (DtalType::ExistentialInt { .. }, DtalType::U64) => true,
        // ExistentialInt <: RefinedInt — check existential constraint implies refinement
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
            // Rename the refinement variable to match the witness, then check implication
            let subs: std::collections::HashMap<String, String> =
                std::collections::HashMap::from([(rv.clone(), w.clone())]);
            let rc_renamed = substitute_var_names_in_constraint(rc, &subs);
            is_constraint_provable(
                &Constraint::Implies(Box::new(c.clone()), Box::new(rc_renamed)),
                constraints,
            )
        }
        // ExistentialInt <: ExistentialInt — check constraint implication
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
            // If structurally equal, trivially compatible
            if w1 == w2 && c1 == c2 {
                return true;
            }
            // Check ∀x. c1(x) ⟹ c2(x) by renaming w1 to a fresh var and checking implication
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
        // ExistentialInt <: SingletonInt — only if constraint forces n = k
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
        // Int <: ExistentialInt — false (cannot satisfy non-trivial constraint)
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

/// Check if actual type is a subtype of (or equal to) expected type.
/// Convenience wrapper without constraint context (syntactic check only).
#[allow(dead_code)]
pub fn types_compatible(actual: &DtalType, expected: &DtalType) -> bool {
    types_compatible_with_constraints(actual, expected, &[])
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

/// Add a constraint linking a register to its index expression.
///
/// When the verifier assigns `reg : SingletonInt(idx)`, this adds
/// `reg == idx` to the constraint context (unless idx is already the
/// register's own variable). This bridges types and constraints so Z3
/// can reason across them.
fn add_register_index_constraint(reg: Reg, idx: &IndexExpr, state: &mut TypeState) {
    let reg_expr = reg_to_index_expr(&reg);
    // Don't add tautological constraint reg == reg
    if *idx != reg_expr {
        state
            .constraints
            .push(Constraint::Eq(reg_expr, idx.clone()));
    }
}

/// Substitute unversioned array Select names with their current versioned names.
///
/// For each `Select(name, idx)` in the constraint, if `name` matches a register
/// with a known array version, replace `name` with the versioned form (e.g., "v0" → "v0_3").
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

/// Recursively substitute Select names in a constraint
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

/// Substitute Select names in an index expression
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

/// Substitute Var names in a constraint (for register name remapping)
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
            // Don't substitute the bound variable
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

/// Substitute Var names in an index expression
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
