//! Physical Allocation Pass
//!
//! Transforms a virtual-register DTAL program into a physically-allocated DTAL
//! program by applying register allocation results. The output uses only physical
//! registers and explicit stack spill/reload instructions.
//!
//! This pass is **untrusted** — the DTAL verifier checks the output independently.
//! If register allocation is incorrect (conflicting assignments, missing spills),
//! the verifier catches it.
//!
//! ## Transformations
//!
//! - Virtual registers → physical registers or spill slots
//! - `BinOp::Div/Mod` → `mov rax, lhs; cqo; idiv rhs; mov dst, rax/rdx`
//! - `Call` → push caller-saved; call; pop caller-saved
//! - Function entry → `Prologue` + parameter moves
//! - Function return → move result to rax + `Epilogue`

use crate::backend::dtal::instr::{
    BinaryOp, DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState,
};
use crate::backend::dtal::regs::{PhysicalReg, Reg, VirtualReg};
use crate::backend::dtal::types::DtalType;
use crate::backend::regalloc::allocator::AllocationResult;
use crate::backend::x86_64::regs::{Location, X86Reg};

/// Map from x86 register back to DTAL PhysicalReg
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

/// Resolve a virtual register to its physical location.
/// Returns either a physical Reg or a stack offset for spilled regs.
enum PhysLoc {
    Reg(Reg),
    Spill(i32),
}

fn try_resolve(vreg: VirtualReg, alloc: &AllocationResult) -> Option<PhysLoc> {
    match alloc.allocation.get(&vreg) {
        Some(Location::Reg(x86)) => Some(PhysLoc::Reg(x86_to_dtal_reg(*x86))),
        Some(Location::Stack(offset)) => Some(PhysLoc::Spill(*offset)),
        None => None,
    }
}

fn resolve(vreg: VirtualReg, alloc: &AllocationResult) -> PhysLoc {
    try_resolve(vreg, alloc)
        .unwrap_or_else(|| panic!("Unallocated virtual register v{}", vreg.0))
}

/// Resolve a Reg (virtual or physical) to a PhysLoc.
fn resolve_reg(reg: Reg, alloc: &AllocationResult) -> PhysLoc {
    match reg {
        Reg::Virtual(vreg) => resolve(vreg, alloc),
        Reg::Physical(_) => PhysLoc::Reg(reg),
    }
}

/// Emit instructions to load a value into a specific physical register.
fn emit_load_to(
    instrs: &mut Vec<DtalInstr>,
    loc: &PhysLoc,
    target: Reg,
    ty: DtalType,
) {
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
fn emit_store_from(
    instrs: &mut Vec<DtalInstr>,
    src: Reg,
    loc: &PhysLoc,
    ty: DtalType,
) {
    match loc {
        PhysLoc::Reg(r) if *r == src => {}
        PhysLoc::Reg(r) => {
            instrs.push(DtalInstr::MovReg {
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
    }
}

/// Physically allocate an entire DTAL program.
///
/// Runs register allocation on each function, then applies the allocation
/// to produce physical DTAL. Runtime stub functions (empty blocks) are
/// passed through unchanged.
pub fn physically_allocate(program: &DtalProgram) -> DtalProgram {
    use crate::backend::regalloc::allocator::{GraphColoringAllocator, LinearScanAllocator};

    let mut functions = Vec::new();

    for func in &program.functions {
        if func.blocks.is_empty() {
            // Runtime stub — pass through unchanged
            functions.push(func.clone());
            continue;
        }

        let allocation = if std::env::var("VERITAS_LS").is_ok() {
            let mut ls = LinearScanAllocator::new();
            ls.allocate(func)
        } else {
            let gc = GraphColoringAllocator::new();
            gc.allocate(func)
        };

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

    // Compute frame size (spill slots * 8, 16-byte aligned)
    let spill_size = (alloc.spill_slots * 8) as u32;
    let callee_saved_size = (alloc.callee_saved_used.len() * 8) as u32;
    // After push rbp (8) + callee saves (N*8), we need frame_size such that
    // total is 16-byte aligned
    let unaligned = 8 + callee_saved_size + spill_size;
    let frame_size = if unaligned % 16 == 0 {
        spill_size
    } else {
        spill_size + (16 - (unaligned % 16))
    };

    let mut blocks = Vec::new();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        let mut instrs = Vec::new();

        // First block: emit prologue and parameter moves
        if block_idx == 0 {
            instrs.push(DtalInstr::Prologue {
                frame_size,
                callee_saved: callee_saved_regs.clone(),
            });

            // Move parameters from ABI argument registers to allocated locations.
            // Must handle cycles: if param 0 (in rdi) is allocated to rsi, and
            // param 1 (in rsi) is allocated to rdi, sequential moves would clobber.
            // Strategy: first move any params whose destination conflicts with
            // another param's source to scratch (R11), then do the rest.
            let param_regs = PhysicalReg::param_regs();
            let mut param_moves: Vec<(Reg, PhysLoc, DtalType)> = Vec::new();
            for (i, (param_reg, param_ty)) in func.params.iter().enumerate() {
                if i < param_regs.len() {
                    if let Reg::Virtual(vreg) = param_reg {
                        // Skip dead parameters (not allocated because never used)
                        if let Some(dst_loc) = try_resolve(*vreg, alloc) {
                            let abi_reg = Reg::Physical(param_regs[i]);
                            param_moves.push((abi_reg, dst_loc, param_ty.clone()));
                        }
                    }
                }
            }

            // Detect if any source reg is the same as another move's destination reg.
            // If so, save it to R11 first. Simple approach: move all params to their
            // allocated location, but if dst is a register that's also a param source,
            // save that source to R11 first.
            let dst_regs: Vec<Option<Reg>> = param_moves
                .iter()
                .map(|(_, loc, _)| match loc {
                    PhysLoc::Reg(r) => Some(*r),
                    PhysLoc::Spill(_) => None,
                })
                .collect();

            // Check for conflicts: src_i == dst_j for some j > i
            let mut saved_to_scratch: Option<(Reg, Reg)> = None; // (original_src, scratch)
            for (i, (src, _, _)) in param_moves.iter().enumerate() {
                for (j, dst_r) in dst_regs.iter().enumerate() {
                    if j != i {
                        if let Some(dr) = dst_r {
                            if *src == *dr && saved_to_scratch.is_none() {
                                // Save src to scratch before it gets clobbered
                                instrs.push(DtalInstr::MovReg {
                                    dst: R11,
                                    src: *src,
                                    ty: DtalType::Int,
                                });
                                saved_to_scratch = Some((*src, R11));
                            }
                        }
                    }
                }
            }

            for (src, dst_loc, ty) in &param_moves {
                let actual_src = if let Some((orig, scratch)) = &saved_to_scratch {
                    if src == orig { *scratch } else { *src }
                } else {
                    *src
                };
                emit_store_from(&mut instrs, actual_src, dst_loc, ty.clone());
            }
        }

        // Translate each instruction
        for instr in &block.instructions {
            allocate_instruction(&mut instrs, instr, alloc, func);
        }

        blocks.push(DtalBlock {
            label: block.label.clone(),
            entry_state: TypeState::new(),
            instructions: instrs,
        });
    }

    // Convert function params to physical registers
    let phys_params: Vec<(Reg, DtalType)> = func
        .params
        .iter()
        .enumerate()
        .map(|(i, (_, ty))| {
            let param_regs = PhysicalReg::param_regs();
            let reg = if i < param_regs.len() {
                Reg::Physical(param_regs[i])
            } else {
                Reg::Physical(PhysicalReg::R0) // placeholder for stack params
            };
            (reg, ty.clone())
        })
        .collect();

    DtalFunction {
        name: func.name.clone(),
        params: phys_params,
        return_type: func.return_type.clone(),
        precondition: func.precondition.clone(),
        postcondition: func.postcondition.clone(),
        blocks,
    }
}

/// Check if a destination register is dead (not allocated).
fn is_dead_dst(reg: &Reg, alloc: &AllocationResult) -> bool {
    if let Reg::Virtual(vreg) = reg {
        try_resolve(*vreg, alloc).is_none()
    } else {
        false
    }
}

/// Translate a single virtual-register instruction to physical-register instructions.
fn allocate_instruction(
    instrs: &mut Vec<DtalInstr>,
    instr: &DtalInstr,
    alloc: &AllocationResult,
    func: &DtalFunction,
) {
    // Skip instructions with dead destinations (register not allocated = never used)
    match instr {
        DtalInstr::MovImm { dst, .. }
        | DtalInstr::MovReg { dst, .. }
        | DtalInstr::BinOp { dst, .. }
        | DtalInstr::AddImm { dst, .. }
        | DtalInstr::Load { dst, .. }
        | DtalInstr::SetCC { dst, .. }
        | DtalInstr::Not { dst, .. }
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
                    // Load imm into scratch, then spill
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

        DtalInstr::MovReg { dst, src, ty } => {
            let src_loc = resolve_reg(*src, alloc);
            let dst_loc = resolve_reg(*dst, alloc);

            match (&src_loc, &dst_loc) {
                (PhysLoc::Reg(s), PhysLoc::Reg(d)) if s == d => {} // nop
                (PhysLoc::Reg(s), PhysLoc::Reg(d)) => {
                    instrs.push(DtalInstr::MovReg {
                        dst: *d,
                        src: *s,
                        ty: ty.clone(),
                    });
                }
                (PhysLoc::Reg(s), PhysLoc::Spill(offset)) => {
                    instrs.push(DtalInstr::SpillStore {
                        src: *s,
                        offset: *offset,
                        ty: ty.clone(),
                    });
                }
                (PhysLoc::Spill(offset), PhysLoc::Reg(d)) => {
                    instrs.push(DtalInstr::SpillLoad {
                        dst: *d,
                        offset: *offset,
                        ty: ty.clone(),
                    });
                }
                (PhysLoc::Spill(src_off), PhysLoc::Spill(dst_off)) => {
                    // mem-to-mem: use scratch
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
                    // x86 idiv: dividend in rax, sign-extend to rdx:rax, divisor in any reg
                    // quotient → rax, remainder → rdx
                    emit_load_to(instrs, &rhs_loc, R11, DtalType::Int);
                    emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
                    instrs.push(DtalInstr::Cqo);
                    instrs.push(DtalInstr::Idiv { src: R11 });
                    let result = if *op == BinaryOp::Mod { RDX } else { RAX };
                    emit_store_from(instrs, result, &dst_loc, ty.clone());
                }
                _ => {
                    // General case: lhs → rax, operate with rhs, store result
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
            emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
            emit_load_to(instrs, &rhs_loc, R11, DtalType::Int);
            instrs.push(DtalInstr::Cmp { lhs: RAX, rhs: R11 });
        }

        DtalInstr::CmpImm { lhs, imm } => {
            let lhs_loc = resolve_reg(*lhs, alloc);
            emit_load_to(instrs, &lhs_loc, RAX, DtalType::Int);
            instrs.push(DtalInstr::CmpImm { lhs: RAX, imm: *imm });
        }

        DtalInstr::SetCC { dst, cond } => {
            // setcc always goes to rax first (byte register encoding safety)
            instrs.push(DtalInstr::SetCC { dst: RAX, cond: *cond });
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

        DtalInstr::Load {
            dst,
            base,
            offset,
            ty,
        } => {
            let base_loc = resolve_reg(*base, alloc);
            let offset_loc = resolve_reg(*offset, alloc);
            let dst_loc = resolve_reg(*dst, alloc);
            // Must load base and offset without clobbering each other.
            // If offset is in RAX, load it to R11 first; if base is in R11, load it to RAX first.
            let offset_in_rax = matches!(&offset_loc, PhysLoc::Reg(r) if *r == RAX);
            let base_in_r11 = matches!(&base_loc, PhysLoc::Reg(r) if *r == R11);
            if offset_in_rax || base_in_r11 {
                // Load offset first (to R11), then base (to RAX)
                emit_load_to(instrs, &offset_loc, R11, DtalType::Int);
                emit_load_to(instrs, &base_loc, RAX, DtalType::Int);
            } else {
                emit_load_to(instrs, &base_loc, RAX, DtalType::Int);
                emit_load_to(instrs, &offset_loc, R11, DtalType::Int);
            }
            instrs.push(DtalInstr::Load {
                dst: RAX,
                base: RAX,
                offset: R11,
                ty: ty.clone(),
            });
            emit_store_from(instrs, RAX, &dst_loc, ty.clone());
        }

        DtalInstr::Store { base, offset, src } => {
            let base_loc = resolve_reg(*base, alloc);
            let offset_loc = resolve_reg(*offset, alloc);
            let src_loc = resolve_reg(*src, alloc);
            // Need base in RAX, offset in R11, src in a third reg.
            // Must avoid clobbering: load src to RDX first (safest), then
            // base/offset with conflict detection.
            //
            // Step 1: Get src into a safe register (RDX unless already safe).
            let src_reg = match &src_loc {
                PhysLoc::Reg(r) if *r != RAX && *r != R11 => *r,
                _ => {
                    emit_load_to(instrs, &src_loc, RDX, DtalType::Int);
                    RDX
                }
            };
            // Step 2: Load base and offset without clobbering each other or src.
            let offset_in_rax = matches!(&offset_loc, PhysLoc::Reg(r) if *r == RAX);
            let base_in_r11 = matches!(&base_loc, PhysLoc::Reg(r) if *r == R11);
            if offset_in_rax || base_in_r11 {
                emit_load_to(instrs, &offset_loc, R11, DtalType::Int);
                emit_load_to(instrs, &base_loc, RAX, DtalType::Int);
            } else {
                emit_load_to(instrs, &base_loc, RAX, DtalType::Int);
                emit_load_to(instrs, &offset_loc, R11, DtalType::Int);
            }
            instrs.push(DtalInstr::Store {
                base: RAX,
                offset: R11,
                src: src_reg,
            });
        }

        DtalInstr::Call { target, return_ty } => {
            // Save caller-saved registers that are live (allocated to caller-saved x86 regs)
            let caller_saved_to_push: Vec<Reg> = alloc
                .allocation
                .values()
                .filter_map(|loc| {
                    if let Location::Reg(x86) = loc {
                        if X86Reg::CALLER_SAVED.contains(x86)
                            && X86Reg::ALLOCATABLE.contains(x86)
                        {
                            Some(x86_to_dtal_reg(*x86))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect();

            for reg in &caller_saved_to_push {
                instrs.push(DtalInstr::Push {
                    src: *reg,
                    ty: DtalType::Int,
                });
            }

            instrs.push(DtalInstr::Call {
                target: target.clone(),
                return_ty: return_ty.clone(),
            });

            // Restore caller-saved (reverse order)
            for reg in caller_saved_to_push.iter().rev() {
                instrs.push(DtalInstr::Pop {
                    dst: *reg,
                    ty: DtalType::Int,
                });
            }

            // Return value is in rax (LR), move to R0 (rdi) for DTAL convention
            instrs.push(DtalInstr::MovReg {
                dst: Reg::Physical(PhysicalReg::R0),
                src: RAX,
                ty: return_ty.clone(),
            });
        }

        DtalInstr::Ret => {
            // Move return value from R0 (rdi) to rax (x86 ABI)
            instrs.push(DtalInstr::MovReg {
                dst: RAX,
                src: Reg::Physical(PhysicalReg::R0),
                ty: func.return_type.clone(),
            });
            instrs.push(DtalInstr::Epilogue {
                callee_saved: alloc
                    .callee_saved_used
                    .iter()
                    .map(|x86| x86_to_dtal_reg(*x86))
                    .collect(),
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
            // Alloca stays as-is with physical dst
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

        // Annotations are dropped in physical DTAL — the verifier will
        // derive types from the physical instructions instead
        DtalInstr::TypeAnnotation { .. } | DtalInstr::ConstraintAssert { .. } => {}

        // Physical instructions should not appear in virtual DTAL input
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
    use crate::pipeline;
    use crate::backend::emit::emit_program;

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

    /// End-to-end test: compile → physalloc → direct_encode → ELF → execute
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

        // Write to temp file and execute
        let path = "/tmp/veritas_physalloc_test";
        std::fs::write(path, &elf).expect("write elf");
        std::fs::set_permissions(path, std::os::unix::fs::PermissionsExt::from_mode(0o755))
            .expect("chmod");
        let status = std::process::Command::new(path)
            .status()
            .expect("execute");
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
        let status = std::process::Command::new(path)
            .status()
            .expect("execute");
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
        let status = std::process::Command::new(path)
            .status()
            .expect("execute");
        assert_eq!(
            status.code(),
            Some(6),
            "Expected exit code 6 (4+2), got {:?}",
            status.code()
        );
    }
}
