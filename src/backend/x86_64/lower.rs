//! DTAL to x86-64 lowering.
//!
//! This module lowers virtual-register DTAL directly to x86-64 instructions.
//! It performs register allocation internally, maps DTAL physical registers to
//! the platform ABI, and emits prologue, epilogue, call, and spill code.
//!
//! # Lowering Sketch
//!
//! ```text
//! DtalInstr::BinOp(v2 = v0 + v1)
//!        |
//!        v
//! load v0/v1 locations -> scratch/target x86 regs
//!        |
//!        v
//! X86Instr::AddRR/AddRI sequence
//!        |
//!        v
//! store result location
//! ```
//!
//! # Design Notes
//!
//! This path is useful for tests and direct lowering experiments because it
//! performs allocation internally. The physical-DTAL path in
//! [`crate::backend::physalloc`] plus [`crate::backend::direct_encode`] keeps
//! allocation visible to the verifier and is the stronger trust-boundary story.
//!
//! # Related Modules
//!
//! [`crate::backend::x86_64::instr`] defines the target IR emitted here, and
//! [`crate::backend::regalloc`] provides the allocation algorithms.

use crate::backend::regalloc::{AllocationResult, GraphColoringAllocator, LinearScanAllocator};
use crate::backend::x86_64::instr::{Condition, MemOperand, X86Function, X86Instr, X86Program};
use crate::backend::x86_64::regs::{Location, X86Reg};
use crate::dtal::instr::{BinaryOp, CmpOp, DtalFunction, DtalInstr, DtalProgram};
use crate::dtal::regs::Reg;
#[cfg(test)]
use crate::dtal::regs::VirtualReg;

/// Lower a DTAL program to x86-64 machine-level IR.
pub fn lower_program(program: &DtalProgram) -> X86Program {
    let mut functions = Vec::new();

    for func in &program.functions {
        // Runtime stubs are provided by the runtime blob.
        if func.blocks.is_empty() {
            continue;
        }
        functions.push(lower_function(func));
    }

    X86Program { functions }
}

/// Lower one DTAL function after register allocation.
fn lower_function(func: &DtalFunction) -> X86Function {
    let uses_reserved_region_reg = function_uses_reserved_region_reg(func);
    let allocatable_regs = if uses_reserved_region_reg {
        X86Reg::ALLOCATABLE
            .iter()
            .copied()
            .filter(|reg| *reg != X86Reg::R15)
            .collect()
    } else {
        X86Reg::ALLOCATABLE.to_vec()
    };

    let mut allocation = if std::env::var("VERITAS_LS").is_ok() {
        let mut ls_allocator = LinearScanAllocator::with_available_regs(allocatable_regs.clone());
        ls_allocator.allocate(func)
    } else {
        let gc_allocator = GraphColoringAllocator::with_available_regs(allocatable_regs);
        gc_allocator.allocate(func)
    };

    if uses_reserved_region_reg && !allocation.callee_saved_used.contains(&X86Reg::R15) {
        allocation.callee_saved_used.push(X86Reg::R15);
        allocation.callee_saved_used.sort();
    }

    if std::env::var("VERITAS_DEBUG_ALLOC").is_ok() {
        use crate::backend::regalloc::liveness::{InterferenceGraph, LivenessAnalysis};
        let liveness = LivenessAnalysis::analyze(func);
        let graph = InterferenceGraph::build(func, &liveness);

        eprintln!("=== {} ===", func.name);

        for block in &func.blocks {
            eprintln!("  {}:", block.label);
            for (i, instr) in block.instructions.iter().enumerate() {
                eprintln!("    {}: {:?}", i, instr);
            }
        }

        let mut vregs: Vec<_> = allocation.allocation.keys().copied().collect();
        vregs.sort_by_key(|v| v.0);
        for vreg in &vregs {
            eprintln!("  v{}: {:?}", vreg.0, allocation.allocation.get(vreg));
        }

        for &node in &graph.nodes {
            if let Some(Location::Reg(r1)) = allocation.allocation.get(&node) {
                for neighbor in graph.neighbors(node) {
                    if let Some(Location::Reg(r2)) = allocation.allocation.get(&neighbor)
                        && r1 == r2
                        && node.0 < neighbor.0
                    {
                        eprintln!(
                            "  CONFLICT: v{} and v{} both in {:?}",
                            node.0, neighbor.0, r1
                        );
                    }
                }
            }
        }
    }

    let lowerer = FunctionLowerer::new(func, &allocation);
    lowerer.lower()
}

fn function_uses_reserved_region_reg(func: &DtalFunction) -> bool {
    use crate::dtal::regs::PhysicalReg;

    func.blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| match instr {
            DtalInstr::BinOp { dst, lhs, rhs, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(lhs, Reg::Physical(PhysicalReg::R12))
                    || matches!(rhs, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::AddImm { dst, src, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(src, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::MovReg { dst, src, .. }
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
            | DtalInstr::PortIn { dst, .. } => matches!(dst, Reg::Physical(PhysicalReg::R12)),
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
            DtalInstr::ShlImm { dst, src, .. } | DtalInstr::ShrImm { dst, src, .. } => {
                matches!(dst, Reg::Physical(PhysicalReg::R12))
                    || matches!(src, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::PortOut { port, value } => {
                matches!(port, Reg::Physical(PhysicalReg::R12))
                    || matches!(value, Reg::Physical(PhysicalReg::R12))
            }
            DtalInstr::TypeAnnotation { reg, .. } => matches!(reg, Reg::Physical(PhysicalReg::R12)),
            DtalInstr::ConstraintAssert { .. }
            | DtalInstr::DropOwned { .. }
            | DtalInstr::BorrowEnd { .. }
            | DtalInstr::Call { .. }
            | DtalInstr::Jmp { .. }
            | DtalInstr::Branch { .. }
            | DtalInstr::Ret => false,
            _ => false,
        })
    })
}

/// Per-function lowering state.
struct FunctionLowerer<'a> {
    func: &'a DtalFunction,
    allocation: AllocationResult,
    instructions: Vec<X86Instr>,
    /// Stack frame size for locals and spills.
    frame_size: i32,
    /// Whether the next `Physical(R0)` read should use RAX instead of RDI.
    return_value_in_rax: bool,
}

impl<'a> FunctionLowerer<'a> {
    fn new(func: &'a DtalFunction, allocation: &AllocationResult) -> Self {
        // Shift spill slots below the callee-saved save area.
        let callee_saved_size = (allocation.callee_saved_used.len() as i32) * 8;
        let mut adjusted = allocation.clone();
        for loc in adjusted.allocation.values_mut() {
            if let Location::Stack(offset) = loc {
                *offset -= callee_saved_size;
            }
        }

        // Keep the stack 16-byte aligned after `push rbp` and callee saves.
        let spill_size = (adjusted.spill_slots as i32) * 8;
        let total_before_frame = 8 + callee_saved_size;
        let frame_size = if (total_before_frame + spill_size) % 16 != 0 {
            spill_size + 8
        } else {
            spill_size
        };

        Self {
            func,
            allocation: adjusted,
            instructions: Vec::new(),
            frame_size,
            return_value_in_rax: false,
        }
    }

    fn lower(mut self) -> X86Function {
        self.emit_prologue();

        for block in &self.func.blocks {
            self.instructions.push(X86Instr::Label {
                name: block.label.clone(),
            });

            for instr in &block.instructions {
                self.lower_instruction(instr);
            }
        }

        X86Function {
            name: self.func.name.clone(),
            instructions: self.instructions,
        }
    }

    /// Emit the function prologue.
    fn emit_prologue(&mut self) {
        self.instructions.push(X86Instr::Push { src: X86Reg::Rbp });

        self.instructions.push(X86Instr::MovRR {
            dst: X86Reg::Rbp,
            src: X86Reg::Rsp,
        });

        // Save callee-saved registers above the spill frame.
        for &reg in &self.allocation.callee_saved_used {
            self.instructions.push(X86Instr::Push { src: reg });
        }

        if self.frame_size > 0 {
            self.instructions.push(X86Instr::SubRI {
                dst: X86Reg::Rsp,
                imm: self.frame_size,
            });
        }

        // Parameter moves are a parallel assignment.
        self.emit_param_moves();
    }

    /// Emit the function epilogue.
    fn emit_epilogue(&mut self) {
        let callee_saved_size = (self.allocation.callee_saved_used.len() as i32) * 8;

        // Position `rsp` at the last callee-saved push before popping.
        self.instructions.push(X86Instr::MovRR {
            dst: X86Reg::Rsp,
            src: X86Reg::Rbp,
        });

        if callee_saved_size > 0 {
            self.instructions.push(X86Instr::SubRI {
                dst: X86Reg::Rsp,
                imm: callee_saved_size,
            });
        }

        for &reg in self.allocation.callee_saved_used.iter().rev() {
            self.instructions.push(X86Instr::Pop { dst: reg });
        }

        self.instructions.push(X86Instr::Pop { dst: X86Reg::Rbp });

        self.instructions.push(X86Instr::Ret);
    }

    /// Emit parameter moves as a parallel assignment.
    fn emit_param_moves(&mut self) {
        let mut pending: Vec<(X86Reg, Location)> = Vec::new();

        for (i, (param_reg, _ty)) in self.func.params.iter().enumerate() {
            if let Reg::Virtual(vreg) = param_reg
                && let Some(&src_reg) = X86Reg::ARG_REGS.get(i)
                && let Some(&dst_loc) = self.allocation.allocation.get(vreg)
            {
                if dst_loc != Location::Reg(src_reg) {
                    pending.push((src_reg, dst_loc));
                }
            }
        }

        let max_iterations = pending.len() * pending.len() + 1;
        let mut iterations = 0;

        while !pending.is_empty() && iterations < max_iterations {
            iterations += 1;

            let ready_idx = pending.iter().enumerate().position(|(i, (_, dst))| {
                if let Location::Reg(dst_reg) = dst {
                    !pending
                        .iter()
                        .enumerate()
                        .any(|(j, (src, _))| j != i && *src == *dst_reg)
                } else {
                    true
                }
            });

            if let Some(idx) = ready_idx {
                let (src, dst) = pending.remove(idx);
                self.store_from_reg(src, dst);
            } else {
                // Break remaining cycles with R11.
                let (first_src, first_dst) = pending.remove(0);
                self.instructions.push(X86Instr::MovRR {
                    dst: X86Reg::R11,
                    src: first_src,
                });
                pending.push((X86Reg::R11, first_dst));
            }
        }
    }

    /// Lower one DTAL instruction.
    fn lower_instruction(&mut self, instr: &DtalInstr) {
        match instr {
            DtalInstr::MovImm { dst, imm, .. } => {
                self.lower_mov_imm(
                    *dst,
                    i64::try_from(*imm).expect("MovImm value exceeds i64 range"),
                );
            }

            DtalInstr::MovReg { dst, src, .. }
            | DtalInstr::AliasBorrow { dst, src, .. }
            | DtalInstr::BorrowMut { dst, src, .. }
            | DtalInstr::MoveOwned { dst, src, .. } => {
                self.lower_mov_reg(*dst, *src);
            }

            DtalInstr::BinOp {
                op, dst, lhs, rhs, ..
            } => {
                self.lower_binop(*op, *dst, *lhs, *rhs);
            }

            DtalInstr::AddImm { dst, src, imm, .. } => {
                self.lower_add_imm(
                    *dst,
                    *src,
                    i64::try_from(*imm).expect("AddImm value exceeds i64 range"),
                );
            }

            DtalInstr::Load {
                dst, base, offset, ..
            } => {
                self.lower_load(*dst, *base, *offset);
            }

            DtalInstr::LoadOp {
                op,
                dst,
                base,
                offset,
                other,
                ..
            } => {
                self.lower_load_op(*op, *dst, *base, *offset, *other);
            }

            DtalInstr::Store { base, offset, src } => {
                self.lower_store(*base, *offset, *src);
            }

            DtalInstr::Cmp { lhs, rhs } => {
                self.lower_cmp(*lhs, *rhs);
            }

            DtalInstr::CmpImm { lhs, imm } => {
                self.lower_cmp_imm(
                    *lhs,
                    i64::try_from(*imm).expect("CmpImm value exceeds i64 range"),
                );
            }

            DtalInstr::SetCC { dst, cond } => {
                let x86_cond = match cond {
                    CmpOp::Eq => Condition::E,
                    CmpOp::Ne => Condition::Ne,
                    CmpOp::Lt => Condition::L,
                    CmpOp::Le => Condition::Le,
                    CmpOp::Gt => Condition::G,
                    CmpOp::Ge => Condition::Ge,
                };

                // RAX avoids legacy high-byte register encodings for `setcc`.
                self.instructions.push(X86Instr::SetCC {
                    dst: X86Reg::Rax,
                    cond: x86_cond,
                });

                let dst_loc = self.get_reg_location(*dst);
                self.store_from_reg(X86Reg::Rax, dst_loc);
            }

            DtalInstr::Not { dst, src, .. } => {
                self.lower_not(*dst, *src);
            }

            DtalInstr::Neg { dst, src, .. } => {
                self.lower_neg(*dst, *src);
            }

            DtalInstr::ShlImm { dst, src, imm, .. } => {
                self.lower_shl_imm(*dst, *src, *imm);
            }
            DtalInstr::ShrImm { dst, src, imm, .. } => {
                self.lower_shr_imm(*dst, *src, *imm);
            }

            DtalInstr::Jmp { target } => {
                self.instructions.push(X86Instr::Jmp {
                    target: target.clone(),
                });
            }

            DtalInstr::Branch { cond, target } => {
                let x86_cond = match cond {
                    CmpOp::Eq => Condition::E,
                    CmpOp::Ne => Condition::Ne,
                    CmpOp::Lt => Condition::L,
                    CmpOp::Le => Condition::Le,
                    CmpOp::Gt => Condition::G,
                    CmpOp::Ge => Condition::Ge,
                };
                self.instructions.push(X86Instr::Jcc {
                    cond: x86_cond,
                    target: target.clone(),
                });
            }

            DtalInstr::Call { target, .. } => {
                let mut caller_saved_in_use: Vec<X86Reg> = self
                    .allocation
                    .allocation
                    .values()
                    .filter_map(|loc| match loc {
                        Location::Reg(r) if r.is_caller_saved() => Some(*r),
                        _ => None,
                    })
                    .collect::<std::collections::HashSet<_>>()
                    .into_iter()
                    .collect();
                caller_saved_in_use.sort();

                for &reg in &caller_saved_in_use {
                    self.instructions.push(X86Instr::Push { src: reg });
                }

                // The call instruction expects 16-byte alignment before pushing RIP.
                let push_bytes = (caller_saved_in_use.len() as i32) * 8;
                let needs_padding = push_bytes % 16 != 0;
                if needs_padding {
                    self.instructions.push(X86Instr::SubRI {
                        dst: X86Reg::Rsp,
                        imm: 8,
                    });
                }

                self.instructions.push(X86Instr::Call {
                    target: target.clone(),
                });

                if needs_padding {
                    self.instructions.push(X86Instr::AddRI {
                        dst: X86Reg::Rsp,
                        imm: 8,
                    });
                }

                for &reg in caller_saved_in_use.iter().rev() {
                    self.instructions.push(X86Instr::Pop { dst: reg });
                }

                // Defer RAX-to-R0 handling until the next `Physical(R0)` read.
                self.return_value_in_rax = true;
            }

            DtalInstr::Ret => {
                // DTAL returns via R0; x86-64 returns via RAX.
                self.instructions.push(X86Instr::MovRR {
                    dst: X86Reg::Rax,
                    src: X86Reg::Rdi,
                });
                self.emit_epilogue();
            }

            DtalInstr::Push { src, .. } => {
                let loc = self.get_reg_location(*src);
                match loc {
                    Location::Reg(r) => {
                        self.instructions.push(X86Instr::Push { src: r });
                    }
                    Location::Stack(offset) => {
                        let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                        self.instructions.push(X86Instr::MovRM {
                            dst: X86Reg::Rax,
                            src: mem,
                        });
                        self.instructions.push(X86Instr::Push { src: X86Reg::Rax });
                    }
                }
            }

            DtalInstr::Pop { dst, .. } => {
                let loc = self.get_vreg_location(*dst);
                match loc {
                    Location::Reg(r) => {
                        self.instructions.push(X86Instr::Pop { dst: r });
                    }
                    Location::Stack(offset) => {
                        self.instructions.push(X86Instr::Pop { dst: X86Reg::Rax });
                        let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                        self.instructions.push(X86Instr::MovMR {
                            dst: mem,
                            src: X86Reg::Rax,
                        });
                    }
                }
            }

            DtalInstr::Alloca { dst, size, .. } => {
                let alloc_size = (*size as i32 + 15) & !15;
                self.instructions.push(X86Instr::SubRI {
                    dst: X86Reg::Rsp,
                    imm: alloc_size,
                });

                let loc = self.get_vreg_location(*dst);
                match loc {
                    Location::Reg(r) => {
                        self.instructions.push(X86Instr::MovRR {
                            dst: r,
                            src: X86Reg::Rsp,
                        });
                    }
                    Location::Stack(offset) => {
                        let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                        self.instructions.push(X86Instr::MovMR {
                            dst: mem,
                            src: X86Reg::Rsp,
                        });
                    }
                }
            }

            DtalInstr::TypeAnnotation { .. }
            | DtalInstr::ConstraintAssert { .. }
            | DtalInstr::DropOwned { .. }
            | DtalInstr::BorrowEnd { .. } => {}

            DtalInstr::PortIn { .. }
            | DtalInstr::PortOut { .. }
            | DtalInstr::Cqo
            | DtalInstr::Idiv { .. }
            | DtalInstr::SpillStore { .. }
            | DtalInstr::SpillLoad { .. }
            | DtalInstr::Prologue { .. }
            | DtalInstr::Epilogue { .. } => {
                unreachable!(
                    "Physical DTAL instructions should not appear in virtual DTAL lowering"
                )
            }
        }
    }

    /// Lower `mov` from an immediate.
    fn lower_mov_imm(&mut self, dst: Reg, imm: i64) {
        let loc = self.get_vreg_location(dst);
        match loc {
            Location::Reg(r) => {
                self.instructions.push(X86Instr::MovRI { dst: r, imm });
            }
            Location::Stack(offset) => {
                if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
                    let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                    self.instructions.push(X86Instr::MovMI {
                        dst: mem,
                        imm: imm as i32,
                    });
                } else {
                    self.instructions.push(X86Instr::MovRI {
                        dst: X86Reg::R11,
                        imm,
                    });
                    let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                    self.instructions.push(X86Instr::MovMR {
                        dst: mem,
                        src: X86Reg::R11,
                    });
                }
            }
        }
    }

    /// Lower `mov` between registers or stack slots.
    fn lower_mov_reg(&mut self, dst: Reg, src: Reg) {
        // After a call, `Physical(R0)` reads the RAX return value once.
        let src_loc = if self.return_value_in_rax {
            if let Reg::Physical(preg) = src {
                use crate::dtal::regs::PhysicalReg;
                if preg == PhysicalReg::R0 {
                    self.return_value_in_rax = false;
                    Location::Reg(X86Reg::Rax)
                } else {
                    self.get_reg_location(src)
                }
            } else {
                self.get_reg_location(src)
            }
        } else {
            self.get_reg_location(src)
        };
        let dst_loc = self.get_vreg_location(dst);

        match (src_loc, dst_loc) {
            (Location::Reg(s), Location::Reg(d)) => {
                if s != d {
                    self.instructions.push(X86Instr::MovRR { dst: d, src: s });
                }
            }
            (Location::Reg(s), Location::Stack(offset)) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::MovMR { dst: mem, src: s });
            }
            (Location::Stack(offset), Location::Reg(d)) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::MovRM { dst: d, src: mem });
            }
            (Location::Stack(src_off), Location::Stack(dst_off)) => {
                let src_mem = MemOperand::base_disp(X86Reg::Rbp, src_off);
                let dst_mem = MemOperand::base_disp(X86Reg::Rbp, dst_off);
                self.instructions.push(X86Instr::MovRM {
                    dst: X86Reg::Rax,
                    src: src_mem,
                });
                self.instructions.push(X86Instr::MovMR {
                    dst: dst_mem,
                    src: X86Reg::Rax,
                });
            }
        }
    }

    /// Lower a binary operation.
    fn lower_binop(&mut self, op: BinaryOp, dst: Reg, lhs: Reg, rhs: Reg) {
        let lhs_loc = self.get_reg_location(lhs);
        let rhs_loc = self.get_reg_location(rhs);
        let dst_loc = self.get_vreg_location(dst);

        match op {
            BinaryOp::Div | BinaryOp::Mod => {
                // Load the divisor before putting the dividend in RAX.
                let rhs_reg = self.load_to_reg(rhs_loc, X86Reg::R11);
                self.load_to_fixed_reg(lhs_loc, X86Reg::Rax);
                let divisor = if rhs_reg == X86Reg::Rax {
                    X86Reg::R11
                } else {
                    rhs_reg
                };
                if divisor == X86Reg::Rax {
                    self.instructions.push(X86Instr::MovRR {
                        dst: X86Reg::R11,
                        src: X86Reg::Rax,
                    });
                }
                self.instructions.push(X86Instr::Cqo);
                self.instructions.push(X86Instr::IdivR { src: divisor });
                let result_reg = if op == BinaryOp::Mod {
                    X86Reg::Rdx
                } else {
                    X86Reg::Rax
                };
                self.store_from_reg(result_reg, dst_loc);
            }
            _ => {
                // Use RAX as the result scratch to avoid clobbering operands.
                self.load_to_fixed_reg(lhs_loc, X86Reg::Rax);
                let rhs_reg = self.load_to_reg(rhs_loc, X86Reg::R11);

                let instr = match op {
                    BinaryOp::Add => X86Instr::AddRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::Sub => X86Instr::SubRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::Mul => X86Instr::ImulRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::BitAnd | BinaryOp::And => X86Instr::AndRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::BitOr | BinaryOp::Or => X86Instr::OrRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::BitXor => X86Instr::XorRR {
                        dst: X86Reg::Rax,
                        src: rhs_reg,
                    },
                    BinaryOp::Shl | BinaryOp::Shr => {
                        // Variable shifts read the count from CL.
                        self.instructions.push(X86Instr::MovRR {
                            dst: X86Reg::Rcx,
                            src: rhs_reg,
                        });
                        if matches!(op, BinaryOp::Shl) {
                            X86Instr::ShlCl { dst: X86Reg::Rax }
                        } else {
                            X86Instr::ShrCl { dst: X86Reg::Rax }
                        }
                    }
                    BinaryOp::Div | BinaryOp::Mod => unreachable!(),
                };
                self.instructions.push(instr);
                self.store_from_reg(X86Reg::Rax, dst_loc);
            }
        }
    }

    /// Lower add immediate.
    fn lower_add_imm(&mut self, dst: Reg, src: Reg, imm: i64) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);

        self.load_to_fixed_reg(src_loc, X86Reg::Rax);

        if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
            self.instructions.push(X86Instr::AddRI {
                dst: X86Reg::Rax,
                imm: imm as i32,
            });
        } else {
            self.instructions.push(X86Instr::MovRI {
                dst: X86Reg::R11,
                imm,
            });
            self.instructions.push(X86Instr::AddRR {
                dst: X86Reg::Rax,
                src: X86Reg::R11,
            });
        }

        self.store_from_reg(X86Reg::Rax, dst_loc);
    }

    /// Pick a scratch register that is not in `avoid`.
    fn pick_scratch(avoid: &[X86Reg]) -> X86Reg {
        for &candidate in &[X86Reg::Rax, X86Reg::Rdx, X86Reg::R11] {
            if !avoid.contains(&candidate) {
                return candidate;
            }
        }
        unreachable!("ran out of scratch registers")
    }

    /// Lower a load.
    fn lower_load(&mut self, dst: Reg, base: Reg, offset: Reg) {
        let base_loc = self.get_reg_location(base);
        let offset_loc = self.get_reg_location(offset);
        let dst_loc = self.get_vreg_location(dst);

        let base_reg = self.load_to_reg(base_loc, X86Reg::Rax);
        let offset_scratch = Self::pick_scratch(&[base_reg]);
        let offset_reg = self.load_to_reg(offset_loc, offset_scratch);

        let mem = MemOperand::base_index_disp(base_reg, offset_reg, 8, 0);
        let result_reg = match dst_loc {
            Location::Reg(r) => r,
            Location::Stack(_) => Self::pick_scratch(&[base_reg, offset_reg]),
        };

        self.instructions.push(X86Instr::MovRM {
            dst: result_reg,
            src: mem,
        });

        if let Location::Stack(offset) = dst_loc {
            let dst_mem = MemOperand::base_disp(X86Reg::Rbp, offset);
            self.instructions.push(X86Instr::MovMR {
                dst: dst_mem,
                src: result_reg,
            });
        }
    }

    /// Lower `dst = *[base + offset*8] op other`.
    fn lower_load_op(
        &mut self,
        op: crate::dtal::instr::BinaryOp,
        dst: Reg,
        base: Reg,
        offset: Reg,
        other: Reg,
    ) {
        use crate::dtal::instr::BinaryOp;
        let base_loc = self.get_reg_location(base);
        let offset_loc = self.get_reg_location(offset);
        let other_loc = self.get_reg_location(other);
        let dst_loc = self.get_vreg_location(dst);

        let base_reg = self.load_to_reg(base_loc, X86Reg::Rax);
        let offset_scratch = Self::pick_scratch(&[base_reg]);
        let offset_reg = self.load_to_reg(offset_loc, offset_scratch);
        let other_scratch = Self::pick_scratch(&[base_reg, offset_reg]);
        let other_reg = self.load_to_reg(other_loc, other_scratch);

        let mem = MemOperand::base_index_disp(base_reg, offset_reg, 8, 0);
        let instr = match op {
            BinaryOp::Add => X86Instr::AddRM {
                dst: other_reg,
                src: mem,
            },
            BinaryOp::Sub => X86Instr::SubRM {
                dst: other_reg,
                src: mem,
            },
            _ => panic!("LoadOp only supports Add/Sub, got {:?}", op),
        };
        self.instructions.push(instr);

        if let Location::Stack(off) = dst_loc {
            let dst_mem = MemOperand::base_disp(X86Reg::Rbp, off);
            self.instructions.push(X86Instr::MovMR {
                dst: dst_mem,
                src: other_reg,
            });
        } else if let Location::Reg(r) = dst_loc
            && r != other_reg
        {
            self.instructions.push(X86Instr::MovRR {
                dst: r,
                src: other_reg,
            });
        }
    }

    /// Lower a store.
    fn lower_store(&mut self, base: Reg, offset: Reg, src: Reg) {
        let base_loc = self.get_reg_location(base);
        let offset_loc = self.get_reg_location(offset);
        let src_loc = self.get_reg_location(src);

        let base_reg = self.load_to_reg(base_loc, X86Reg::Rax);
        let offset_scratch = Self::pick_scratch(&[base_reg]);
        let offset_reg = self.load_to_reg(offset_loc, offset_scratch);
        let src_scratch = Self::pick_scratch(&[base_reg, offset_reg]);
        let src_reg = self.load_to_reg(src_loc, src_scratch);

        let mem = MemOperand::base_index_disp(base_reg, offset_reg, 8, 0);
        self.instructions.push(X86Instr::MovMR {
            dst: mem,
            src: src_reg,
        });
    }

    /// Lower `cmp`.
    fn lower_cmp(&mut self, lhs: Reg, rhs: Reg) {
        let lhs_loc = self.get_reg_location(lhs);
        let rhs_loc = self.get_reg_location(rhs);

        let lhs_reg = self.load_to_reg(lhs_loc, X86Reg::Rax);

        match rhs_loc {
            Location::Reg(r) => {
                self.instructions.push(X86Instr::CmpRR {
                    lhs: lhs_reg,
                    rhs: r,
                });
            }
            Location::Stack(offset) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::CmpRM {
                    lhs: lhs_reg,
                    rhs: mem,
                });
            }
        }
    }

    /// Lower `cmp` against an immediate.
    fn lower_cmp_imm(&mut self, lhs: Reg, imm: i64) {
        let lhs_loc = self.get_reg_location(lhs);
        let lhs_reg = self.load_to_reg(lhs_loc, X86Reg::Rax);

        if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
            self.instructions.push(X86Instr::CmpRI {
                lhs: lhs_reg,
                imm: imm as i32,
            });
        } else {
            let scratch = Self::pick_scratch(&[lhs_reg]);
            self.instructions
                .push(X86Instr::MovRI { dst: scratch, imm });
            self.instructions.push(X86Instr::CmpRR {
                lhs: lhs_reg,
                rhs: scratch,
            });
        }
    }

    /// Lower bitwise not.
    fn lower_not(&mut self, dst: Reg, src: Reg) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);

        self.load_to_fixed_reg(src_loc, X86Reg::Rax);
        self.instructions.push(X86Instr::Not { dst: X86Reg::Rax });
        self.store_from_reg(X86Reg::Rax, dst_loc);
    }

    fn lower_neg(&mut self, dst: Reg, src: Reg) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);

        self.load_to_fixed_reg(src_loc, X86Reg::Rax);
        self.instructions.push(X86Instr::Neg { dst: X86Reg::Rax });
        self.store_from_reg(X86Reg::Rax, dst_loc);
    }

    fn lower_shl_imm(&mut self, dst: Reg, src: Reg, imm: u8) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);
        self.load_to_fixed_reg(src_loc, X86Reg::Rax);
        self.instructions.push(X86Instr::ShlRI {
            dst: X86Reg::Rax,
            imm,
        });
        self.store_from_reg(X86Reg::Rax, dst_loc);
    }

    fn lower_shr_imm(&mut self, dst: Reg, src: Reg, imm: u8) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);
        self.load_to_fixed_reg(src_loc, X86Reg::Rax);
        self.instructions.push(X86Instr::ShrRI {
            dst: X86Reg::Rax,
            imm,
        });
        self.store_from_reg(X86Reg::Rax, dst_loc);
    }

    /// Get the location of a virtual or physical register.
    fn get_reg_location(&self, reg: Reg) -> Location {
        match reg {
            Reg::Virtual(vreg) => self.get_vreg_location(Reg::Virtual(vreg)),
            Reg::Physical(preg) => {
                use crate::dtal::regs::PhysicalReg;
                let x86_reg = match preg {
                    PhysicalReg::R0 => X86Reg::Rdi,
                    PhysicalReg::R1 => X86Reg::Rsi,
                    PhysicalReg::R2 => X86Reg::Rdx,
                    PhysicalReg::R3 => X86Reg::Rcx,
                    PhysicalReg::R4 => X86Reg::R8,
                    PhysicalReg::R5 => X86Reg::R9,
                    PhysicalReg::R6 => X86Reg::R10,
                    PhysicalReg::R7 => X86Reg::R11,
                    PhysicalReg::R8 => X86Reg::Rbx,
                    PhysicalReg::R9 => X86Reg::R12,
                    PhysicalReg::R10 => X86Reg::R13,
                    PhysicalReg::R11 => X86Reg::R14,
                    PhysicalReg::R12 => X86Reg::R15,
                    PhysicalReg::R13 | PhysicalReg::R14 | PhysicalReg::R15 => X86Reg::Rax,
                    PhysicalReg::SP => X86Reg::Rsp,
                    PhysicalReg::FP => X86Reg::Rbp,
                    PhysicalReg::LR => X86Reg::Rax,
                };
                Location::Reg(x86_reg)
            }
        }
    }

    /// Get an allocated virtual register location.
    fn get_vreg_location(&self, reg: Reg) -> Location {
        match reg {
            Reg::Virtual(vreg) => self
                .allocation
                .allocation
                .get(&vreg)
                .copied()
                .unwrap_or(Location::Reg(X86Reg::Rax)),
            Reg::Physical(preg) => self.get_reg_location(Reg::Physical(preg)),
        }
    }

    /// Load a location into a register, reusing existing registers when possible.
    fn load_to_reg(&mut self, loc: Location, scratch: X86Reg) -> X86Reg {
        match loc {
            Location::Reg(r) => r,
            Location::Stack(offset) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::MovRM {
                    dst: scratch,
                    src: mem,
                });
                scratch
            }
        }
    }

    /// Load a value into a required fixed register.
    fn load_to_fixed_reg(&mut self, loc: Location, dst: X86Reg) -> X86Reg {
        match loc {
            Location::Reg(r) => {
                if r != dst {
                    self.instructions.push(X86Instr::MovRR { dst, src: r });
                }
            }
            Location::Stack(offset) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::MovRM { dst, src: mem });
            }
        }
        dst
    }

    /// Store a register value to a location.
    fn store_from_reg(&mut self, reg: X86Reg, loc: Location) {
        match loc {
            Location::Reg(r) => {
                if r != reg {
                    self.instructions.push(X86Instr::MovRR { dst: r, src: reg });
                }
            }
            Location::Stack(offset) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions
                    .push(X86Instr::MovMR { dst: mem, src: reg });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dtal::instr::{DtalBlock, TypeState};
    use crate::dtal::types::DtalType;

    #[test]
    fn test_lower_simple_function() {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let func = DtalFunction {
            name: "add".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 10,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 20,
                        ty: DtalType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v2,
                        lhs: v0,
                        rhs: v1,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        };

        let program = DtalProgram {
            functions: vec![func],
        };

        let x86_program = lower_program(&program);

        assert_eq!(x86_program.functions.len(), 1);
        let x86_func = &x86_program.functions[0];
        assert_eq!(x86_func.name, "add");

        assert!(!x86_func.instructions.is_empty());

        for instr in &x86_func.instructions {
            println!("{}", instr);
        }
    }

    #[test]
    fn test_lower_with_branch() {
        use crate::dtal::instr::CmpOp;

        let v0 = Reg::Virtual(VirtualReg(0));

        let func = DtalFunction {
            name: "branch_test".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![
                DtalBlock {
                    label: "entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v0,
                            imm: 5,
                            ty: DtalType::Int,
                        },
                        DtalInstr::CmpImm { lhs: v0, imm: 10 },
                        DtalInstr::Branch {
                            cond: CmpOp::Lt,
                            target: "less".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "greater_equal".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v0,
                            imm: 1,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: "exit".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "less".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v0,
                            imm: 0,
                            ty: DtalType::Int,
                        },
                        DtalInstr::Jmp {
                            target: "exit".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "exit".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![DtalInstr::Ret],
                },
            ],
        };

        let program = DtalProgram {
            functions: vec![func],
        };

        let x86_program = lower_program(&program);
        let x86_func = &x86_program.functions[0];

        let has_jcc = x86_func
            .instructions
            .iter()
            .any(|i| matches!(i, X86Instr::Jcc { .. }));
        let has_jmp = x86_func
            .instructions
            .iter()
            .any(|i| matches!(i, X86Instr::Jmp { .. }));

        assert!(has_jcc, "Should have conditional jump");
        assert!(has_jmp, "Should have unconditional jump");
    }
}
