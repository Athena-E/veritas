//! Physical allocation for DTAL.
//!
//! This pass applies register-allocation results to virtual-register DTAL,
//! producing physical-register DTAL with explicit spill loads and stores. The
//! pass is intentionally untrusted: the DTAL verifier checks the allocated
//! output before code emission.
//!
//! # Pipeline Position
//!
//! ```text
//! DTAL with virtual registers
//!        |
//!        v
//! liveness + register allocation
//!        |
//!        v
//! DTAL with physical registers and spill slots
//!        |
//!        v
//! verifier -> encoder
//! ```
//!
//! # Design Notes
//!
//! Spill code is emitted as DTAL, not directly as x86-64, so the verifier can
//! still check the resulting type and ownership state. Function contracts are
//! remapped from virtual parameters to ABI physical registers after allocation.
//!
//! # Error Behavior
//!
//! Dead virtual registers may be skipped when they have no physical location.
//! A live unallocated virtual register is treated as an allocator invariant
//! violation and panics.
//!
//! # Related Modules
//!
//! - [`crate::backend::regalloc`] computes register and stack locations.
//! - [`crate::backend::direct_encode`] encodes the verified physical DTAL.
//! - [`crate::verifier`] rechecks the transformed program.

use crate::backend::regalloc::allocator::AllocationResult;
use crate::backend::regalloc::liveness::LivenessAnalysis;
use crate::backend::x86_64::regs::{Location, X86Reg};
use crate::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use crate::dtal::regs::{PhysicalReg, Reg, VirtualReg};
use crate::dtal::types::DtalType;
use std::collections::HashSet;

mod location;
mod remap;

use location::{PhysLoc, resolve_reg, resolve_reg_opt, try_resolve};
use remap::{remap_constraint, remap_constraint_vars, remap_constraint_vars_in_type};

/// Map an x86 register to the corresponding DTAL physical register.
fn x86_to_dtal_reg(x86: X86Reg) -> Reg {
    let preg = match x86 {
        X86Reg::Rdi => PhysicalReg::R0,
        X86Reg::Rsi => PhysicalReg::R1,
        X86Reg::Rdx => PhysicalReg::R2,
        X86Reg::Rcx => PhysicalReg::R3,
        X86Reg::R8 => PhysicalReg::R4,
        X86Reg::R9 => PhysicalReg::R5,
        X86Reg::R10 => PhysicalReg::R6,
        X86Reg::R11 => PhysicalReg::R7,
        X86Reg::Rbx => PhysicalReg::R8,
        X86Reg::R12 => PhysicalReg::R9,
        X86Reg::R13 => PhysicalReg::R10,
        X86Reg::R14 => PhysicalReg::R11,
        X86Reg::R15 => PhysicalReg::R12,
        X86Reg::Rax => PhysicalReg::LR,
        X86Reg::Rsp => PhysicalReg::SP,
        X86Reg::Rbp => PhysicalReg::FP,
    };
    Reg::Physical(preg)
}

const RAX: Reg = Reg::Physical(PhysicalReg::LR);
const RDX: Reg = Reg::Physical(PhysicalReg::R2);
const R11: Reg = Reg::Physical(PhysicalReg::R7);

fn function_uses_reserved_region_reg(func: &DtalFunction) -> bool {
    func.blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| match instr {
            DtalInstr::BinOp { dst, lhs, rhs, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(lhs, Reg::Physical(PhysicalReg::R12))
                    || matches!(rhs, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::AddImm { dst, src, .. }
            | DtalInstr::MovReg { dst, src, .. }
            | DtalInstr::AliasBorrow { dst, src, .. }
            | DtalInstr::BorrowMut { dst, src, .. }
            | DtalInstr::MoveOwned { dst, src, .. }
            | DtalInstr::Neg { dst, src, .. }
            | DtalInstr::Not { dst, src, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(src, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::MovImm { dst, .. }
            | DtalInstr::SetCC { dst, .. }
            | DtalInstr::Pop { dst, .. }
            | DtalInstr::Alloca { dst, .. }
            | DtalInstr::PortIn { dst, .. }
            | DtalInstr::TypeAnnotation { reg: dst, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::Push { src, .. } => matches!(src, Reg::Physical(PhysicalReg::R12)),
            DtalInstr::Load {
                dst, base, offset, ..
            } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(base, Reg::Physical(PhysicalReg::R12))
                    || matches!(offset, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::LoadOp {
                dst,
                base,
                offset,
                other,
                ..
            } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(base, Reg::Physical(PhysicalReg::R12))
                    || matches!(offset, Reg::Physical(PhysicalReg::R12))
                    || matches!(other, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::Store { base, offset, src } => {
                matches!(base, Reg::Physical(PhysicalReg::R12))
                    || matches!(offset, Reg::Physical(PhysicalReg::R12))
                    || matches!(src, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::Cmp { lhs, rhs } => {
                matches!(lhs, Reg::Physical(PhysicalReg::R12))
                    || matches!(rhs, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::CmpImm { lhs, .. } => matches!(lhs, Reg::Physical(PhysicalReg::R12)),
            DtalInstr::PortOut { port, value } => {
                matches!(port, Reg::Physical(PhysicalReg::R12))
                    || matches!(value, Reg::Physical(PhysicalReg::R12))
            }
            _ => false,
        })
    })
}

/// Emit instructions to load a value into a specific physical register.
fn emit_load_to(instrs: &mut Vec<DtalInstr>, loc: &PhysLoc, target: Reg, ty: DtalType) {
    match loc {
        PhysLoc::Reg(r) if *r == target => {}
        PhysLoc::Reg(r) => {
            instrs.push(DtalInstr::MovReg {
                dst: target,
                src: *r,
                ty,
            });
        }
        PhysLoc::Spill(offset) => {
            instrs.push(DtalInstr::SpillLoad {
                dst: target,
                offset: *offset,
                ty,
            });
        }
    }
}

/// Emit instructions to store from a physical register to a location.
fn emit_store_from(instrs: &mut Vec<DtalInstr>, src: Reg, loc: &PhysLoc, ty: DtalType) {
    match loc {
        PhysLoc::Reg(r) if *r == src => {}
        PhysLoc::Reg(r) => {
            instrs.push(DtalInstr::MovReg { dst: *r, src, ty });
        }
        PhysLoc::Spill(offset) => {
            instrs.push(DtalInstr::SpillStore {
                src,
                offset: *offset,
                ty,
            });
        }
    }
}

fn emit_store_from_owned(instrs: &mut Vec<DtalInstr>, src: Reg, loc: &PhysLoc, ty: DtalType) {
    match loc {
        PhysLoc::Reg(r) if *r == src => {}
        PhysLoc::Reg(r) => {
            instrs.push(DtalInstr::MoveOwned { dst: *r, src, ty });
        }
        PhysLoc::Spill(offset) => {
            instrs.push(DtalInstr::SpillStore {
                src,
                offset: *offset,
                ty,
            });
        }
    }
}

fn emit_store_from_param_kind(
    instrs: &mut Vec<DtalInstr>,
    src: Reg,
    loc: &PhysLoc,
    ty: DtalType,
    param_kind: crate::common::ownership::ParameterKind,
) {
    use crate::common::ownership::ParameterKind;

    match param_kind {
        ParameterKind::OwnedValue => emit_store_from_owned(instrs, src, loc, ty),
        ParameterKind::SharedBorrow => match loc {
            PhysLoc::Reg(r) if *r == src => {}
            PhysLoc::Reg(r) => {
                instrs.push(DtalInstr::AliasBorrow {
                    lifetime: None,
                    dst: *r,
                    src,
                    ty,
                });
            }
            PhysLoc::Spill(offset) => {
                instrs.push(DtalInstr::SpillStore {
                    src,
                    offset: *offset,
                    ty,
                });
            }
        },
        ParameterKind::MutableBorrow | ParameterKind::PlainValue => {
            emit_store_from(instrs, src, loc, ty)
        }
    }
}

fn pick_scratch(avoid: &[Reg]) -> Reg {
    for reg in [RAX, R11, RDX] {
        if !avoid.contains(&reg) {
            return reg;
        }
    }
    RAX
}

/// Apply physical allocation to every non-stub function in a DTAL program.
pub fn physically_allocate(program: &DtalProgram) -> DtalProgram {
    use crate::backend::regalloc::allocator::{GraphColoringAllocator, LinearScanAllocator};

    let mut functions = Vec::new();

    for func in &program.functions {
        if func.blocks.is_empty() {
            functions.push(func.clone());
            continue;
        }

        let allocatable_regs = if function_uses_reserved_region_reg(func) {
            X86Reg::ALLOCATABLE
                .iter()
                .copied()
                .filter(|reg| *reg != X86Reg::R15)
                .collect()
        } else {
            X86Reg::ALLOCATABLE.to_vec()
        };

        let mut allocation = if std::env::var("VERITAS_LS").is_ok() {
            let mut ls = LinearScanAllocator::with_available_regs(allocatable_regs.clone());
            ls.allocate(func)
        } else {
            let gc = GraphColoringAllocator::with_available_regs(allocatable_regs);
            gc.allocate(func)
        };

        // R12/R15 is reserved for the hosted region pointer and must be saved
        // when used explicitly.
        if function_uses_reserved_region_reg(func)
            && !allocation.callee_saved_used.contains(&X86Reg::R15)
        {
            allocation.callee_saved_used.push(X86Reg::R15);
            allocation.callee_saved_used.sort();
        }

        functions.push(allocate_function(func, &allocation));
    }

    DtalProgram { functions }
}

/// Physically allocate a single function.
fn allocate_function(func: &DtalFunction, alloc: &AllocationResult) -> DtalFunction {
    let callee_saved_regs: Vec<Reg> = alloc
        .callee_saved_used
        .iter()
        .map(|x86| x86_to_dtal_reg(*x86))
        .collect();

    let caller_saved_to_save: Vec<Reg>;
    {
        let mut cs_set: std::collections::BTreeSet<X86Reg> = std::collections::BTreeSet::new();
        for loc in alloc.allocation.values() {
            if let Location::Reg(x86) = loc
                && X86Reg::CALLER_SAVED.contains(x86)
                && X86Reg::ALLOCATABLE.contains(x86)
            {
                cs_set.insert(*x86);
            }
        }
        caller_saved_to_save = cs_set.iter().map(|x86| x86_to_dtal_reg(*x86)).collect();
    }

    let caller_save_slots = caller_saved_to_save.len();
    let liveness = LivenessAnalysis::analyze(func);

    let spill_size = ((alloc.spill_slots + caller_save_slots) * 8) as u32;
    let callee_saved_size = (callee_saved_regs.len() * 8) as u32;
    // Keep the stack 16-byte aligned after `push rbp` and callee saves.
    let unaligned = 8 + callee_saved_size + spill_size;
    let frame_size = if unaligned.is_multiple_of(16) {
        spill_size
    } else {
        spill_size + (16 - (unaligned % 16))
    };

    let mut blocks = Vec::new();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        let mut instrs = Vec::new();

        if block_idx == 0 {
            instrs.push(DtalInstr::Prologue {
                frame_size,
                callee_saved: callee_saved_regs.clone(),
            });

            // ABI parameter registers need types before parameter moves.
            let param_regs_list = PhysicalReg::param_regs();
            for (i, (_param_reg, param_ty)) in func.params.iter().enumerate() {
                if i < param_regs_list.len() {
                    instrs.push(DtalInstr::TypeAnnotation {
                        reg: Reg::Physical(param_regs_list[i]),
                        ty: param_ty.clone(),
                    });
                }
            }

            // Break parameter-move cycles with R11.
            let param_regs = PhysicalReg::param_regs();
            let mut param_moves: Vec<(
                Reg,
                PhysLoc,
                DtalType,
                crate::common::ownership::ParameterKind,
            )> = Vec::new();
            for (i, ((param_reg, param_ty), param_kind)) in func
                .params
                .iter()
                .zip(func.parameter_kinds.iter())
                .enumerate()
            {
                if i < param_regs.len()
                    && let Reg::Virtual(vreg) = param_reg
                {
                    if let Some(dst_loc) = try_resolve(*vreg, alloc) {
                        let abi_reg = Reg::Physical(param_regs[i]);
                        param_moves.push((abi_reg, dst_loc, param_ty.clone(), *param_kind));
                    }
                }
            }

            let dst_regs: Vec<Option<Reg>> = param_moves
                .iter()
                .map(|(_, loc, _, _)| match loc {
                    PhysLoc::Reg(r) => Some(*r),
                    PhysLoc::Spill(_) => None,
                })
                .collect();

            let mut saved_to_scratch: Option<(Reg, Reg)> = None;
            for (i, (src, _, ty, param_kind)) in param_moves.iter().enumerate() {
                for (j, dst_r) in dst_regs.iter().enumerate() {
                    if j != i
                        && let Some(dr) = dst_r
                        && *src == *dr
                        && saved_to_scratch.is_none()
                    {
                        match param_kind {
                            crate::common::ownership::ParameterKind::OwnedValue => {
                                instrs.push(DtalInstr::MoveOwned {
                                    dst: R11,
                                    src: *src,
                                    ty: ty.clone(),
                                });
                            }
                            crate::common::ownership::ParameterKind::SharedBorrow => {
                                instrs.push(DtalInstr::AliasBorrow {
                                    lifetime: None,
                                    dst: R11,
                                    src: *src,
                                    ty: ty.clone(),
                                });
                            }
                            _ => {
                                instrs.push(DtalInstr::MovReg {
                                    dst: R11,
                                    src: *src,
                                    ty: DtalType::Int,
                                });
                            }
                        }
                        saved_to_scratch = Some((*src, R11));
                    }
                }
            }

            for (src, dst_loc, ty, param_kind) in &param_moves {
                let actual_src = if let Some((orig, scratch)) = &saved_to_scratch {
                    if src == orig { *scratch } else { *src }
                } else {
                    *src
                };
                emit_store_from_param_kind(
                    &mut instrs,
                    actual_src,
                    dst_loc,
                    ty.clone(),
                    *param_kind,
                );
            }
        }

        let live_out = liveness
            .blocks
            .get(&block.label)
            .map(|info| &info.live_out)
            .cloned()
            .unwrap_or_default();
        let instr_liveness = LivenessAnalysis::compute_instruction_liveness(block, &live_out);
        for (instr_idx, instr) in block.instructions.iter().enumerate() {
            allocate_instruction(
                &mut instrs,
                instr,
                AllocationContext {
                    alloc,
                    func,
                    callee_saved: &callee_saved_regs,
                    caller_saved: &caller_saved_to_save,
                    callee_saved_count: callee_saved_regs.len(),
                    live_after: instr_liveness.get(instr_idx),
                },
            );
        }

        blocks.push(DtalBlock {
            label: block.label.clone(),
            entry_state: TypeState::new(),
            instructions: instrs,
        });
    }

    let phys_params: Vec<(Reg, DtalType)> = func
        .params
        .iter()
        .enumerate()
        .map(|(i, (_, ty))| {
            let param_regs = PhysicalReg::param_regs();
            let reg = if i < param_regs.len() {
                Reg::Physical(param_regs[i])
            } else {
                Reg::Physical(PhysicalReg::R0)
            };
            (reg, ty.clone())
        })
        .collect();

    // Function contracts refer to ABI parameter registers after allocation.
    let mut precond_map = std::collections::HashMap::new();
    let param_regs_for_precond = PhysicalReg::param_regs();
    for (i, (param_reg, _)) in func.params.iter().enumerate() {
        if i < param_regs_for_precond.len() {
            let vname = format!("{}", param_reg);
            let pname = format!("{}", Reg::Physical(param_regs_for_precond[i]));
            precond_map.insert(vname, pname);
        }
    }
    let phys_precond = func
        .precondition
        .as_ref()
        .map(|c| remap_constraint(c, &precond_map));
    let phys_postcond = func
        .postcondition
        .as_ref()
        .map(|c| remap_constraint(c, &precond_map));

    DtalFunction {
        name: func.name.clone(),
        params: phys_params,
        parameter_kinds: func.parameter_kinds.clone(),
        return_type: func.return_type.clone(),
        precondition: phys_precond,
        postcondition: phys_postcond,
        blocks,
    }
}

/// Check whether a destination register is dead.
fn is_dead_dst(reg: &Reg, alloc: &AllocationResult) -> bool {
    if let Reg::Virtual(vreg) = reg {
        try_resolve(*vreg, alloc).is_none()
    } else {
        false
    }
}

struct AllocationContext<'a> {
    alloc: &'a AllocationResult,
    func: &'a DtalFunction,
    callee_saved: &'a [Reg],
    caller_saved: &'a [Reg],
    callee_saved_count: usize,
    live_after: Option<&'a HashSet<VirtualReg>>,
}

/// Translate one virtual-register instruction to physical DTAL.
fn allocate_instruction(
    instrs: &mut Vec<DtalInstr>,
    instr: &DtalInstr,
    ctx: AllocationContext<'_>,
) {
    let AllocationContext {
        alloc,
        func,
        callee_saved,
        caller_saved,
        callee_saved_count,
        live_after,
    } = ctx;

    match instr {
        DtalInstr::MovImm { dst, .. }
        | DtalInstr::MovReg { dst, .. }
        | DtalInstr::AliasBorrow { dst, .. }
        | DtalInstr::BorrowMut { dst, .. }
        | DtalInstr::MoveOwned { dst, .. }
        | DtalInstr::BinOp { dst, .. }
        | DtalInstr::AddImm { dst, .. }
        | DtalInstr::ShlImm { dst, .. }
        | DtalInstr::ShrImm { dst, .. }
        | DtalInstr::Load { dst, .. }
        | DtalInstr::LoadOp { dst, .. }
        | DtalInstr::SetCC { dst, .. }
        | DtalInstr::Not { dst, .. }
        | DtalInstr::Neg { dst, .. }
        | DtalInstr::Pop { dst, .. }
        | DtalInstr::Alloca { dst, .. } => {
            if is_dead_dst(dst, alloc) {
                return;
            }
        }
        _ => {}
    }

    match instr {
        DtalInstr::MovImm { dst, imm, ty } => {
            let dst_loc = resolve_reg(*dst, alloc);
            match dst_loc {
                PhysLoc::Reg(r) => {
                    instrs.push(DtalInstr::MovImm {
                        dst: r,
                        imm: *imm,
                        ty: ty.clone(),
                    });
                }
                PhysLoc::Spill(offset) => {
                    instrs.push(DtalInstr::MovImm {
                        dst: R11,
                        imm: *imm,
                        ty: ty.clone(),
                    });
                    instrs.push(DtalInstr::SpillStore {
                        src: R11,
                        offset,
                        ty: ty.clone(),
                    });
                }
            }
        }

        DtalInstr::MovReg { dst, src, ty }
        | DtalInstr::AliasBorrow { dst, src, ty, .. }
        | DtalInstr::BorrowMut { dst, src, ty, .. }
        | DtalInstr::MoveOwned { dst, src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);

            match (&src_loc, &dst_loc) {
                (PhysLoc::Reg(s), PhysLoc::Reg(d)) if s == d => {}
                (PhysLoc::Reg(s), PhysLoc::Reg(d)) => {
                    let lowered = match instr {
                        DtalInstr::MoveOwned { .. } => DtalInstr::MoveOwned {
                            dst: *d,
                            src: *s,
                            ty: ty.clone(),
                        },
                        DtalInstr::AliasBorrow { .. } => DtalInstr::AliasBorrow {
                            lifetime: match instr {
                                DtalInstr::AliasBorrow { lifetime, .. } => *lifetime,
                                _ => None,
                            },
                            dst: *d,
                            src: *s,
                            ty: ty.clone(),
                        },
                        DtalInstr::BorrowMut { .. } => DtalInstr::BorrowMut {
                            lifetime: match instr {
                                DtalInstr::BorrowMut { lifetime, .. } => *lifetime,
                                _ => None,
                            },
                            dst: *d,
                            src: *s,
                            ty: ty.clone(),
                        },
                        _ => DtalInstr::MovReg {
                            dst: *d,
                            src: *s,
                            ty: ty.clone(),
                        },
                    };
                    instrs.push(lowered);
                }
                (PhysLoc::Reg(s), PhysLoc::Spill(offset)) => {
                    instrs.push(DtalInstr::SpillStore {
                        src: *s,
                        offset: *offset,
                        ty: ty.clone(),
                    });
                }
                (PhysLoc::Spill(offset), PhysLoc::Reg(d)) => match instr {
                    DtalInstr::MoveOwned { .. }
                    | DtalInstr::AliasBorrow { .. }
                    | DtalInstr::BorrowMut { .. } => {
                        let scratch = pick_scratch(&[*d]);
                        instrs.push(DtalInstr::SpillLoad {
                            dst: scratch,
                            offset: *offset,
                            ty: ty.clone(),
                        });
                        let lowered = match instr {
                            DtalInstr::MoveOwned { .. } => DtalInstr::MoveOwned {
                                dst: *d,
                                src: scratch,
                                ty: ty.clone(),
                            },
                            DtalInstr::AliasBorrow { .. } => DtalInstr::AliasBorrow {
                                lifetime: match instr {
                                    DtalInstr::AliasBorrow { lifetime, .. } => *lifetime,
                                    _ => None,
                                },
                                dst: *d,
                                src: scratch,
                                ty: ty.clone(),
                            },
                            DtalInstr::BorrowMut { .. } => DtalInstr::BorrowMut {
                                lifetime: match instr {
                                    DtalInstr::BorrowMut { lifetime, .. } => *lifetime,
                                    _ => None,
                                },
                                dst: *d,
                                src: scratch,
                                ty: ty.clone(),
                            },
                            _ => unreachable!(),
                        };
                        instrs.push(lowered);
                    }
                    _ => {
                        instrs.push(DtalInstr::SpillLoad {
                            dst: *d,
                            offset: *offset,
                            ty: ty.clone(),
                        });
                    }
                },
                (PhysLoc::Spill(src_off), PhysLoc::Spill(dst_off)) => match instr {
                    DtalInstr::MoveOwned { .. }
                    | DtalInstr::AliasBorrow { .. }
                    | DtalInstr::BorrowMut { .. } => {
                        instrs.push(DtalInstr::SpillLoad {
                            dst: RAX,
                            offset: *src_off,
                            ty: ty.clone(),
                        });
                        let scratch = R11;
                        let lowered = match instr {
                            DtalInstr::MoveOwned { .. } => DtalInstr::MoveOwned {
                                dst: scratch,
                                src: RAX,
                                ty: ty.clone(),
                            },
                            DtalInstr::AliasBorrow { .. } => DtalInstr::AliasBorrow {
                                lifetime: match instr {
                                    DtalInstr::AliasBorrow { lifetime, .. } => *lifetime,
                                    _ => None,
                                },
                                dst: scratch,
                                src: RAX,
                                ty: ty.clone(),
                            },
                            DtalInstr::BorrowMut { .. } => DtalInstr::BorrowMut {
                                lifetime: match instr {
                                    DtalInstr::BorrowMut { lifetime, .. } => *lifetime,
                                    _ => None,
                                },
                                dst: scratch,
                                src: RAX,
                                ty: ty.clone(),
                            },
                            _ => unreachable!(),
                        };
                        instrs.push(lowered);
                        instrs.push(DtalInstr::SpillStore {
                            src: scratch,
                            offset: *dst_off,
                            ty: ty.clone(),
                        });
                    }
                    _ => {
                        instrs.push(DtalInstr::SpillLoad {
                            dst: RAX,
                            offset: *src_off,
                            ty: ty.clone(),
                        });
                        instrs.push(DtalInstr::SpillStore {
                            src: RAX,
                            offset: *dst_off,
                            ty: ty.clone(),
                        });
                    }
                },
            }
        }

        DtalInstr::DropOwned { src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            match src_loc {
                PhysLoc::Reg(r) => instrs.push(DtalInstr::DropOwned {
                    src: r,
                    ty: ty.clone(),
                }),
                PhysLoc::Spill(offset) => {
                    instrs.push(DtalInstr::SpillLoad {
                        dst: R11,
                        offset,
                        ty: ty.clone(),
                    });
                    instrs.push(DtalInstr::DropOwned {
                        src: R11,
                        ty: ty.clone(),
                    });
                }
            }
        }

        DtalInstr::BorrowEnd { lifetime, src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            match src_loc {
                PhysLoc::Reg(r) => instrs.push(DtalInstr::BorrowEnd {
                    lifetime: *lifetime,
                    src: r,
                    ty: ty.clone(),
                }),
                PhysLoc::Spill(offset) => {
                    instrs.push(DtalInstr::SpillLoad {
                        dst: R11,
                        offset,
                        ty: ty.clone(),
                    });
                    instrs.push(DtalInstr::BorrowEnd {
                        lifetime: *lifetime,
                        src: R11,
                        ty: ty.clone(),
                    });
                }
            }
        }

        DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        } => {
            let dst_loc = resolve_reg(*dst, alloc);
            let lhs_loc = resolve_reg(*lhs, alloc);
            let rhs_loc = resolve_reg(*rhs, alloc);

            match op {
                BinaryOp::Div | BinaryOp::Mod => {
                    // x86 `idiv` uses rdx:rax and returns quotient/remainder in rax/rdx.
                    emit_load_to(instrs, &rhs_loc, R11, DtalType::Int);
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    instrs.push(DtalInstr::Cqo);
                    instrs.push(DtalInstr::Idiv { src: R11 });
                    let result = if *op == BinaryOp::Mod { RDX } else { RAX };
                    emit_store_from(instrs, result, &dst_loc, ty.clone());
                }
                BinaryOp::Shl | BinaryOp::Shr => {
                    // x86 shifts require the count in CL.
                    let r3 = Reg::Physical(PhysicalReg::R3);
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    emit_load_to(instrs, &rhs_loc, r3, DtalType::Int);
                    instrs.push(DtalInstr::BinOp {
                        op: *op,
                        dst: RAX,
                        lhs: RAX,
                        rhs: r3,
                        ty: ty.clone(),
                    });
                    emit_store_from(instrs, RAX, &dst_loc, ty.clone());
                }
                _ => {
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    emit_load_to(instrs, &rhs_loc, R11, DtalType::Int);
                    instrs.push(DtalInstr::BinOp {
                        op: *op,
                        dst: RAX,
                        lhs: RAX,
                        rhs: R11,
                        ty: ty.clone(),
                    });
                    emit_store_from(instrs, RAX, &dst_loc, ty.clone());
                }
            }
        }

        DtalInstr::AddImm { dst, src, imm, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &src_loc, RAX, DtalType::Int);
            instrs.push(DtalInstr::AddImm {
                dst: RAX,
                src: RAX,
                imm: *imm,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::Cmp { lhs, rhs } => {
            let lhs_loc = resolve_reg(*lhs, alloc);
            let rhs_loc = resolve_reg(*rhs, alloc);
            // Prefer allocated registers so branch constraints name stable regs.
            let lhs_reg = match &lhs_loc {
                PhysLoc::Reg(r) => *r,
                PhysLoc::Spill(_) => {
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    RAX
                }
            };
            let rhs_reg = match &rhs_loc {
                PhysLoc::Reg(r) if *r != lhs_reg => *r,
                _ => {
                    emit_load_to(instrs, &rhs_loc, R11, DtalType::Int);
                    R11
                }
            };
            instrs.push(DtalInstr::Cmp {
                lhs: lhs_reg,
                rhs: rhs_reg,
            });
        }

        DtalInstr::CmpImm { lhs, imm } => {
            let lhs_loc = resolve_reg(*lhs, alloc);
            let lhs_reg = match &lhs_loc {
                PhysLoc::Reg(r) => *r,
                PhysLoc::Spill(_) => {
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    RAX
                }
            };
            instrs.push(DtalInstr::CmpImm {
                lhs: lhs_reg,
                imm: *imm,
            });
        }

        DtalInstr::SetCC { dst, cond } => {
            // Route through rax for byte-register encoding.
            instrs.push(DtalInstr::SetCC {
                dst: RAX,
                cond: *cond,
            });
            let dst_loc = resolve_reg(*dst, alloc);
            emit_store_from(instrs, RAX, &dst_loc, DtalType::Bool);
        }

        DtalInstr::Not { dst, src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &src_loc, RAX, ty.clone());
            instrs.push(DtalInstr::Not {
                dst: RAX,
                src: RAX,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::Neg { dst, src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &src_loc, RAX, ty.clone());
            instrs.push(DtalInstr::Neg {
                dst: RAX,
                src: RAX,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::ShlImm { dst, src, imm, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &src_loc, RAX, ty.clone());
            instrs.push(DtalInstr::ShlImm {
                dst: RAX,
                src: RAX,
                imm: *imm,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }
        DtalInstr::ShrImm { dst, src, imm, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &src_loc, RAX, ty.clone());
            instrs.push(DtalInstr::ShrImm {
                dst: RAX,
                src: RAX,
                imm: *imm,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::Load {
            dst,
            base,
            offset,
            ty,
        } => {
            let base_loc = resolve_reg(*base, alloc);
            let offset_loc = resolve_reg(*offset, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            let mut base_reg = match &base_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            let mut offset_reg = match &offset_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            if base_reg == offset_reg {
                offset_reg = None;
            }
            if base_reg.is_none() {
                let scratch = pick_scratch(&offset_reg.iter().copied().collect::<Vec<_>>());
                emit_load_to(instrs, &base_loc, scratch, DtalType::Int);
                base_reg = Some(scratch);
            }
            if offset_reg.is_none() {
                let scratch = pick_scratch(&base_reg.iter().copied().collect::<Vec<_>>());
                emit_load_to(instrs, &offset_loc, scratch, DtalType::Int);
                offset_reg = Some(scratch);
            }
            instrs.push(DtalInstr::Load {
                dst: RAX,
                base: base_reg.unwrap(),
                offset: offset_reg.unwrap(),
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::LoadOp {
            op,
            dst,
            base,
            offset,
            other,
            ty,
        } => {
            let base_loc = resolve_reg(*base, alloc);
            let offset_loc = resolve_reg(*offset, alloc);
            let other_loc = resolve_reg(*other, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            let mut base_reg = match &base_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            let mut offset_reg = match &offset_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            if base_reg == offset_reg {
                offset_reg = None;
            }
            let other_reg = match &other_loc {
                PhysLoc::Reg(r) if Some(*r) != base_reg && Some(*r) != offset_reg => *r,
                _ => {
                    let scratch = pick_scratch(
                        &base_reg
                            .iter()
                            .chain(offset_reg.iter())
                            .copied()
                            .collect::<Vec<_>>(),
                    );
                    emit_load_to(instrs, &other_loc, scratch, ty.clone());
                    scratch
                }
            };
            if base_reg.is_none() {
                let scratch = pick_scratch(
                    &offset_reg
                        .iter()
                        .chain(std::iter::once(&other_reg))
                        .copied()
                        .collect::<Vec<_>>(),
                );
                emit_load_to(instrs, &base_loc, scratch, DtalType::Int);
                base_reg = Some(scratch);
            }
            if offset_reg.is_none() {
                let scratch = pick_scratch(
                    &base_reg
                        .iter()
                        .chain(std::iter::once(&other_reg))
                        .copied()
                        .collect::<Vec<_>>(),
                );
                emit_load_to(instrs, &offset_loc, scratch, DtalType::Int);
                offset_reg = Some(scratch);
            }
            instrs.push(DtalInstr::LoadOp {
                op: *op,
                dst: other_reg,
                base: base_reg.unwrap(),
                offset: offset_reg.unwrap(),
                other: other_reg,
                ty: ty.clone(),
            });
            emit_store_from(instrs, other_reg, &dst_loc, ty.clone());
        }

        DtalInstr::Store { base, offset, src } => {
            let base_loc = resolve_reg(*base, alloc);
            let offset_loc = resolve_reg(*offset, alloc);
            let src_loc = resolve_reg(*src, alloc);
            let mut base_reg = match &base_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            let mut offset_reg = match &offset_loc {
                PhysLoc::Reg(r) => Some(*r),
                PhysLoc::Spill(_) => None,
            };
            if base_reg == offset_reg {
                offset_reg = None;
            }
            let src_reg = match &src_loc {
                PhysLoc::Reg(r) if Some(*r) != base_reg && Some(*r) != offset_reg => *r,
                _ => {
                    let scratch = pick_scratch(
                        &base_reg
                            .iter()
                            .chain(offset_reg.iter())
                            .copied()
                            .collect::<Vec<_>>(),
                    );
                    emit_load_to(instrs, &src_loc, scratch, DtalType::Int);
                    scratch
                }
            };
            if base_reg.is_none() {
                let scratch = pick_scratch(
                    &offset_reg
                        .iter()
                        .chain(std::iter::once(&src_reg))
                        .copied()
                        .collect::<Vec<_>>(),
                );
                emit_load_to(instrs, &base_loc, scratch, DtalType::Int);
                base_reg = Some(scratch);
            }
            if offset_reg.is_none() {
                let scratch = pick_scratch(
                    &base_reg
                        .iter()
                        .chain(std::iter::once(&src_reg))
                        .copied()
                        .collect::<Vec<_>>(),
                );
                emit_load_to(instrs, &offset_loc, scratch, DtalType::Int);
                offset_reg = Some(scratch);
            }
            instrs.push(DtalInstr::Store {
                base: base_reg.unwrap(),
                offset: offset_reg.unwrap(),
                src: src_reg,
            });
        }

        DtalInstr::Call {
            target,
            arg_kinds,
            return_ty,
            ownership,
        } => {
            let regs_to_save: Vec<Reg> = if let Some(live_after) = live_after {
                let mut regs = Vec::new();
                for vreg in live_after {
                    if let Some(Location::Reg(x86)) = alloc.allocation.get(vreg)
                        && X86Reg::CALLER_SAVED.contains(x86)
                        && X86Reg::ALLOCATABLE.contains(x86)
                    {
                        let reg = x86_to_dtal_reg(*x86);
                        if !regs.contains(&reg) {
                            regs.push(reg);
                        }
                    }
                }
                regs
            } else {
                caller_saved.to_vec()
            };
            // Save caller-saved registers before the already-emitted argument moves.
            let arg_regs: Vec<Reg> = PhysicalReg::param_regs()
                .iter()
                .map(|p| Reg::Physical(*p))
                .collect();

            let mut arg_move_count = 0;
            for instr in instrs.iter().rev() {
                match instr {
                    DtalInstr::MovReg { dst, .. }
                    | DtalInstr::AliasBorrow { dst, .. }
                    | DtalInstr::BorrowMut { dst, .. }
                    | DtalInstr::MoveOwned { dst, .. }
                        if arg_regs.contains(dst) =>
                    {
                        arg_move_count += 1;
                    }
                    _ => break,
                }
            }

            let split_point = instrs.len() - arg_move_count;
            let arg_moves: Vec<DtalInstr> = instrs.drain(split_point..).collect();

            // Caller-saved slots live in the prologue-allocated frame.
            for (i, &reg) in regs_to_save.iter().enumerate() {
                let offset = -(((callee_saved_count + alloc.spill_slots + 1 + i) * 8) as i32);
                instrs.push(DtalInstr::SpillStore {
                    src: reg,
                    offset,
                    ty: DtalType::Int,
                });
            }

            instrs.extend(arg_moves);

            instrs.push(DtalInstr::Call {
                target: target.clone(),
                arg_kinds: arg_kinds.clone(),
                return_ty: return_ty.clone(),
                ownership: *ownership,
            });

            // Preserve the return value in R0 for the following virtual result move.
            let r0_reg = Reg::Physical(PhysicalReg::R0);
            instrs.push(DtalInstr::MovReg {
                dst: r0_reg,
                src: RAX,
                ty: return_ty.clone(),
            });
            for (i, &reg) in regs_to_save.iter().enumerate() {
                if reg == r0_reg {
                    continue;
                }
                let offset = -(((callee_saved_count + alloc.spill_slots + 1 + i) * 8) as i32);
                instrs.push(DtalInstr::SpillLoad {
                    dst: reg,
                    offset,
                    ty: DtalType::Int,
                });
            }
        }

        DtalInstr::Ret => {
            // Unit returns carry no payload; materialize and retag a dummy scalar.
            if matches!(func.return_type, DtalType::Unit) {
                instrs.push(DtalInstr::MovImm {
                    dst: RAX,
                    imm: 0,
                    ty: DtalType::Int,
                });
                instrs.push(DtalInstr::TypeAnnotation {
                    reg: RAX,
                    ty: DtalType::Unit,
                });
            } else if matches!(func.return_type, DtalType::Array { .. }) {
                instrs.push(DtalInstr::MoveOwned {
                    dst: RAX,
                    src: Reg::Physical(PhysicalReg::R0),
                    ty: func.return_type.clone(),
                });
            } else {
                instrs.push(DtalInstr::MovReg {
                    dst: RAX,
                    src: Reg::Physical(PhysicalReg::R0),
                    ty: func.return_type.clone(),
                });
            }
            instrs.push(DtalInstr::Epilogue {
                callee_saved: callee_saved.to_vec(),
            });
            instrs.push(DtalInstr::Ret);
        }

        DtalInstr::Jmp { target } => {
            instrs.push(DtalInstr::Jmp {
                target: target.clone(),
            });
        }

        DtalInstr::Branch { cond, target } => {
            instrs.push(DtalInstr::Branch {
                cond: *cond,
                target: target.clone(),
            });
        }

        DtalInstr::Push { src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            emit_load_to(instrs, &src_loc, RAX, ty.clone());
            instrs.push(DtalInstr::Push {
                src: RAX,
                ty: ty.clone(),
            });
        }

        DtalInstr::Pop { dst, ty } => {
            instrs.push(DtalInstr::Pop {
                dst: RAX,
                ty: ty.clone(),
            });
            let dst_loc = resolve_reg(*dst, alloc);
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::Alloca { dst, size, ty } => {
            let dst_loc = resolve_reg(*dst, alloc);
            match dst_loc {
                PhysLoc::Reg(r) => {
                    instrs.push(DtalInstr::Alloca {
                        dst: r,
                        size: *size,
                        ty: ty.clone(),
                    });
                }
                PhysLoc::Spill(_) => {
                    instrs.push(DtalInstr::Alloca {
                        dst: RAX,
                        size: *size,
                        ty: ty.clone(),
                    });
                    emit_store_from(instrs, RAX, &dst_loc, ty.clone());
                }
            }
        }

        DtalInstr::TypeAnnotation { reg, ty } => {
            if let Some(loc) = resolve_reg_opt(*reg, alloc)
                && let PhysLoc::Reg(phys_reg) = loc
            {
                instrs.push(DtalInstr::TypeAnnotation {
                    reg: phys_reg,
                    ty: remap_constraint_vars_in_type(ty, alloc),
                });
            }
        }

        DtalInstr::ConstraintAssert { constraint } => {
            instrs.push(DtalInstr::ConstraintAssert {
                constraint: remap_constraint_vars(constraint, alloc),
            });
        }

        DtalInstr::PortIn { dst, port } => {
            let port_loc = resolve_reg(*port, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            emit_load_to(instrs, &port_loc, RDX, DtalType::Int);
            instrs.push(DtalInstr::PortIn {
                dst: RAX,
                port: RDX,
            });
            emit_store_from(instrs, RAX, &dst_loc, DtalType::Int);
        }

        DtalInstr::PortOut { port, value } => {
            let port_loc = resolve_reg(*port, alloc);
            let value_loc = resolve_reg(*value, alloc);
            emit_load_to(instrs, &port_loc, RDX, DtalType::Int);
            emit_load_to(instrs, &value_loc, RAX, DtalType::Int);
            instrs.push(DtalInstr::PortOut {
                port: RDX,
                value: RAX,
            });
        }

        DtalInstr::Cqo
        | DtalInstr::Idiv { .. }
        | DtalInstr::SpillStore { .. }
        | DtalInstr::SpillLoad { .. }
        | DtalInstr::Prologue { .. }
        | DtalInstr::Epilogue { .. } => {
            unreachable!("Physical instructions in virtual DTAL input")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::emit::emit_program;
    use crate::pipeline;
    use crate::verifier::verify_dtal;

    #[test]
    fn test_physalloc_simple() {
        let source = r#"
fn main() -> int {
    let x: int = 42;
    let y: int = x + 1;
    y
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let text = emit_program(&physical);
        println!("=== Physical DTAL ===\n{}", text);
    }

    #[test]
    fn test_physalloc_with_call() {
        let source = r#"
fn add(a: int, b: int) -> int {
    a + b
}
fn main() -> int {
    let x: int = add(3, 4);
    x
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let text = emit_program(&physical);
        println!("=== Physical DTAL (call) ===\n{}", text);
    }

    #[test]
    fn test_physalloc_division() {
        let source = r#"
fn main() -> int {
    let x: int = 42 / 10;
    let y: int = 42 % 10;
    x + y
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let text = emit_program(&physical);
        println!("=== Physical DTAL (div/mod) ===\n{}", text);
    }

    /// End-to-end test through ELF execution.
    #[test]
    fn test_physalloc_e2e_simple() {
        use crate::backend::direct_encode::encode_physical_dtal;
        use crate::backend::elf::generate_elf;

        let source = r#"
fn main() -> int {
    let x: int = 40;
    let y: int = 2;
    x + y
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let encoded = encode_physical_dtal(&physical);
        let elf = generate_elf(&encoded, "main");

        let path = "/tmp/veritas_physalloc_test";
        std::fs::write(path, &elf).expect("write elf");
        std::fs::set_permissions(path, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .expect("chmod");
        let status = std::process::Command::new(path).status().expect("execute");
        assert_eq!(
            status.code(),
            Some(42),
            "Expected exit code 42 (40+2), got {:?}",
            status.code()
        );
    }

    #[test]
    fn test_physalloc_e2e_function_call() {
        use crate::backend::direct_encode::encode_physical_dtal;
        use crate::backend::elf::generate_elf;

        let source = r#"
fn add(a: int, b: int) -> int {
    a + b
}
fn main() -> int {
    add(3, 4)
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let encoded = encode_physical_dtal(&physical);
        let elf = generate_elf(&encoded, "main");

        let path = "/tmp/veritas_physalloc_call_test";
        std::fs::write(path, &elf).expect("write elf");
        std::fs::set_permissions(path, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .expect("chmod");
        let status = std::process::Command::new(path).status().expect("execute");
        assert_eq!(
            status.code(),
            Some(7),
            "Expected exit code 7 (3+4), got {:?}",
            status.code()
        );
    }

    #[test]
    fn test_physalloc_e2e_division() {
        use crate::backend::direct_encode::encode_physical_dtal;
        use crate::backend::elf::generate_elf;

        let source = r#"
fn main() -> int {
    let x: int = 42 / 10;
    let y: int = 42 % 10;
    x + y
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let encoded = encode_physical_dtal(&physical);
        let elf = generate_elf(&encoded, "main");

        let path = "/tmp/veritas_physalloc_div_test";
        std::fs::write(path, &elf).expect("write elf");
        std::fs::set_permissions(path, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .expect("chmod");
        let status = std::process::Command::new(path).status().expect("execute");
        assert_eq!(
            status.code(),
            Some(6),
            "Expected exit code 6 (4+2), got {:?}",
            status.code()
        );
    }

    #[test]
    fn test_physalloc_e2e_nested_region_helper_call() {
        use crate::backend::direct_encode::encode_physical_dtal;
        use crate::backend::elf::generate_elf;

        let source = r#"
fn sum_positive(arr: [{v:int|v > 0}; 3]) -> int
    requires forall i in 0..3 { arr[i] > 0 }
{
    arr[0] + arr[1] + arr[2]
}

fn call_sum_positive() -> int {
    let arr: [{v:int|v > 0}; 3] = [1; 3];
    sum_positive(arr)
}

fn main() -> int {
    let a: int = call_sum_positive();
    let b: int = call_sum_positive();
    a + b
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        let encoded = encode_physical_dtal(&physical);
        let elf = generate_elf(&encoded, "main");

        let path = "/tmp/veritas_physalloc_nested_region_test";
        std::fs::write(path, &elf).expect("write elf");
        std::fs::set_permissions(path, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .expect("chmod");
        let status = std::process::Command::new(path).status().expect("execute");
        assert_eq!(
            status.code(),
            Some(6),
            "Expected exit code 6 from two nested region helper calls, got {:?}",
            status.code()
        );
    }

    #[test]
    fn test_physalloc_verify_shared_borrow_call() {
        let source = r#"
fn inspect(r: &[int; 1]) -> int {
    7
}

fn main() -> int {
    let arr: [int; 1] = [0; 1];
    inspect(&arr)
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        verify_dtal(&physical).expect("physical shared-borrow program should verify");
    }

    #[test]
    fn test_physalloc_verify_mutable_borrow_call() {
        let source = r#"
fn touch(r: &mut [int; 1]) -> int {
    9
}

fn main() -> int {
    let mut arr: [int; 1] = [0; 1];
    touch(&mut arr)
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        verify_dtal(&physical).expect("physical mutable-borrow program should verify");
    }

    #[test]
    fn test_physalloc_verify_shared_scalar_deref() {
        let source = r#"
fn main() -> int {
    let x: int = 7;
    let rx: &int = &x;
    *rx
}
"#;
        let output = pipeline::compile_verbose(source).expect("compile failed");
        let physical = physically_allocate(&output.dtal_program);
        verify_dtal(&physical).expect("physical shared scalar deref should verify");
    }
}
