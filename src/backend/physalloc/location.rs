use super::x86_to_dtal_reg;
use crate::backend::regalloc::allocator::AllocationResult;
use crate::backend::x86_64::regs::Location;
use crate::dtal::regs::{Reg, VirtualReg};

pub(super) enum PhysLoc {
    Reg(Reg),
    Spill(i32),
}

pub(super) fn try_resolve(vreg: VirtualReg, alloc: &AllocationResult) -> Option<PhysLoc> {
    match alloc.allocation.get(&vreg) {
        Some(Location::Reg(x86)) => Some(PhysLoc::Reg(x86_to_dtal_reg(*x86))),
        Some(Location::Stack(offset)) => Some(PhysLoc::Spill(*offset)),
        None => None,
    }
}

fn resolve(vreg: VirtualReg, alloc: &AllocationResult) -> PhysLoc {
    try_resolve(vreg, alloc).unwrap_or_else(|| panic!("Unallocated virtual register v{}", vreg.0))
}

pub(super) fn resolve_reg(reg: Reg, alloc: &AllocationResult) -> PhysLoc {
    match reg {
        Reg::Virtual(vreg) => resolve(vreg, alloc),
        Reg::Physical(_) => PhysLoc::Reg(reg),
    }
}

pub(super) fn resolve_reg_opt(reg: Reg, alloc: &AllocationResult) -> Option<PhysLoc> {
    match reg {
        Reg::Virtual(vreg) => try_resolve(vreg, alloc),
        Reg::Physical(_) => Some(PhysLoc::Reg(reg)),
    }
}
