use crate::dtal::instr::TypeState;
use crate::dtal::regs::{PhysicalReg, Reg};
use crate::verifier::error::VerifyError;

pub(crate) fn transfer_owned(src: Reg, dst: Reg, state: &mut TypeState) {
    let object_id = state.owned_object_ids.get(&src).copied();
    clear_owned_alias_group(src, object_id, state);
    if let Some(object_id) = object_id {
        assign_owned_object(dst, object_id, state);
    } else {
        clear_owned(dst, state);
        clear_shared_borrow(dst, state);
        clear_mutable_borrow(dst, state);
    }
}

fn abi_owned_alias_counterpart(reg: Reg) -> Option<Reg> {
    match reg {
        Reg::Physical(PhysicalReg::R0) => Some(Reg::Physical(PhysicalReg::LR)),
        Reg::Physical(PhysicalReg::LR) => Some(Reg::Physical(PhysicalReg::R0)),
        _ => None,
    }
}

pub(crate) fn clear_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    clear_owned(reg, state);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            clear_owned(counterpart, state);
        }
    }
}

pub(crate) fn consume_owned_alias_group(reg: Reg, object_id: Option<u32>, state: &mut TypeState) {
    state.consumed_registers.insert(reg);
    if let Some(counterpart) = abi_owned_alias_counterpart(reg) {
        let same_object = object_id
            .is_some_and(|owned| state.owned_object_ids.get(&counterpart).copied() == Some(owned));
        if same_object {
            state.consumed_registers.insert(counterpart);
        }
    }
}

pub(crate) fn preserve_plain_mov_alias_ownership(src: Reg, dst: Reg, state: &mut TypeState) {
    let is_abi_return_alias = is_allowed_owned_alias_pair(src, dst);
    if is_abi_return_alias && let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_owned_object(dst, object_id, state);
    } else {
        clear_owned(dst, state);
    }
}

pub(crate) fn preserve_plain_mov_shared_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

pub(crate) fn preserve_plain_mov_mutable_borrow(src: Reg, dst: Reg, state: &mut TypeState) {
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

pub(crate) fn assign_shared_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, state);
    } else if let Some(object_id) = state.shared_borrow_object_ids.get(&src).copied() {
        assign_shared_borrow_object(dst, object_id, state);
    } else {
        clear_shared_borrow(dst, state);
    }
}

pub(crate) fn assign_mutable_borrow_from(src: Reg, dst: Reg, state: &mut TypeState) {
    if let Some(object_id) = state.owned_object_ids.get(&src).copied() {
        assign_mutable_borrow_object(dst, object_id, state);
    } else {
        clear_mutable_borrow(dst, state);
    }
}

pub(crate) fn fresh_object_id(state: &mut TypeState) -> u32 {
    let object_id = state.next_object_id;
    state.next_object_id += 1;
    object_id
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

fn assign_owned_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_shared_borrow(reg, state);
    clear_mutable_borrow(reg, state);
    state.owned_registers.insert(reg);
    state.owned_object_ids.insert(reg, object_id);
}

fn assign_shared_borrow_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_owned(reg, state);
    clear_mutable_borrow(reg, state);
    state.shared_borrow_object_ids.insert(reg, object_id);
}

fn assign_mutable_borrow_object(reg: Reg, object_id: u32, state: &mut TypeState) {
    clear_owned(reg, state);
    clear_shared_borrow(reg, state);
    state.mutable_borrow_object_ids.insert(reg, object_id);
}

pub(crate) fn is_allowed_owned_alias_pair(lhs: Reg, rhs: Reg) -> bool {
    matches!(
        (lhs, rhs),
        (
            Reg::Physical(PhysicalReg::LR),
            Reg::Physical(PhysicalReg::R0)
        ) | (
            Reg::Physical(PhysicalReg::R0),
            Reg::Physical(PhysicalReg::LR)
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
