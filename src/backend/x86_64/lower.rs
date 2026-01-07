//! DTAL to x86-64 Lowering
//!
//! This module translates DTAL instructions with virtual registers into
//! x86-64 instructions with physical registers.
//!
//! # Pipeline
//!
//! ```text
//! DTAL (virtual regs) → Register Allocation → x86-64 Instructions
//! ```

use crate::backend::dtal::instr::{BinaryOp, CmpOp, DtalFunction, DtalInstr, DtalProgram};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use crate::backend::regalloc::{AllocationResult, LinearScanAllocator};
use crate::backend::x86_64::instr::{Condition, MemOperand, X86Function, X86Instr, X86Program};
use crate::backend::x86_64::regs::{Location, X86Reg};

/// Lower a DTAL program to x86-64
pub fn lower_program(program: &DtalProgram) -> X86Program {
    let mut functions = Vec::new();

    for func in &program.functions {
        functions.push(lower_function(func));
    }

    X86Program { functions }
}

/// Lower a DTAL function to x86-64
fn lower_function(func: &DtalFunction) -> X86Function {
    // Perform register allocation
    let mut allocator = LinearScanAllocator::new();
    let allocation = allocator.allocate(func);

    let lowerer = FunctionLowerer::new(func, &allocation);
    lowerer.lower()
}

/// Function lowering context
struct FunctionLowerer<'a, 'src> {
    func: &'a DtalFunction<'src>,
    allocation: &'a AllocationResult,
    instructions: Vec<X86Instr>,
    /// Stack frame size (for locals and spills)
    frame_size: i32,
}

impl<'a, 'src> FunctionLowerer<'a, 'src> {
    fn new(func: &'a DtalFunction<'src>, allocation: &'a AllocationResult) -> Self {
        // Calculate frame size: 8 bytes per spill slot + padding for alignment
        let spill_size = (allocation.spill_slots as i32) * 8;
        let frame_size = (spill_size + 15) & !15; // 16-byte align

        Self {
            func,
            allocation,
            instructions: Vec::new(),
            frame_size,
        }
    }

    fn lower(mut self) -> X86Function {
        // Function prologue
        self.emit_prologue();

        // Lower each block
        for block in &self.func.blocks {
            // Emit block label
            self.instructions.push(X86Instr::Label {
                name: block.label.clone(),
            });

            // Lower each instruction
            for instr in &block.instructions {
                self.lower_instruction(instr);
            }
        }

        X86Function {
            name: self.func.name.clone(),
            instructions: self.instructions,
        }
    }

    /// Emit function prologue
    fn emit_prologue(&mut self) {
        // push rbp
        self.instructions.push(X86Instr::Push { src: X86Reg::Rbp });

        // mov rbp, rsp
        self.instructions.push(X86Instr::MovRR {
            dst: X86Reg::Rbp,
            src: X86Reg::Rsp,
        });

        // Allocate stack frame if needed
        if self.frame_size > 0 {
            self.instructions.push(X86Instr::SubRI {
                dst: X86Reg::Rsp,
                imm: self.frame_size,
            });
        }

        // Save callee-saved registers
        for &reg in &self.allocation.callee_saved_used {
            self.instructions.push(X86Instr::Push { src: reg });
        }

        // Move arguments from ABI registers to allocated locations
        for (i, (param_reg, _ty)) in self.func.params.iter().enumerate() {
            if let Reg::Virtual(vreg) = param_reg {
                let arg_reg = X86Reg::ARG_REGS.get(i).copied();
                if let Some(src_reg) = arg_reg {
                    self.move_to_vreg(*vreg, src_reg);
                }
                // TODO: Handle stack-passed arguments (> 6 args)
            }
        }
    }

    /// Emit function epilogue
    fn emit_epilogue(&mut self) {
        // Restore callee-saved registers (in reverse order)
        for &reg in self.allocation.callee_saved_used.iter().rev() {
            self.instructions.push(X86Instr::Pop { dst: reg });
        }

        // mov rsp, rbp
        self.instructions.push(X86Instr::MovRR {
            dst: X86Reg::Rsp,
            src: X86Reg::Rbp,
        });

        // pop rbp
        self.instructions.push(X86Instr::Pop { dst: X86Reg::Rbp });

        // ret
        self.instructions.push(X86Instr::Ret);
    }

    /// Lower a single DTAL instruction
    fn lower_instruction(&mut self, instr: &DtalInstr<'src>) {
        match instr {
            DtalInstr::MovImm { dst, imm, .. } => {
                self.lower_mov_imm(*dst, *imm);
            }

            DtalInstr::MovReg { dst, src, .. } => {
                self.lower_mov_reg(*dst, *src);
            }

            DtalInstr::BinOp {
                op, dst, lhs, rhs, ..
            } => {
                self.lower_binop(*op, *dst, *lhs, *rhs);
            }

            DtalInstr::AddImm { dst, src, imm, .. } => {
                self.lower_add_imm(*dst, *src, *imm);
            }

            DtalInstr::Load {
                dst, base, offset, ..
            } => {
                self.lower_load(*dst, *base, *offset);
            }

            DtalInstr::Store { base, offset, src } => {
                self.lower_store(*base, *offset, *src);
            }

            DtalInstr::Cmp { lhs, rhs } => {
                self.lower_cmp(*lhs, *rhs);
            }

            DtalInstr::CmpImm { lhs, imm } => {
                self.lower_cmp_imm(*lhs, *imm);
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

                let dst_loc = self.get_location(*dst);
                match dst_loc {
                    Location::Reg(r) => {
                        self.instructions
                            .push(X86Instr::SetCC { dst: r, cond: x86_cond });
                    }
                    Location::Stack(offset) => {
                        // Set in scratch register, then store
                        self.instructions.push(X86Instr::SetCC {
                            dst: X86Reg::Rax,
                            cond: x86_cond,
                        });
                        self.instructions.push(X86Instr::MovMR {
                            dst: MemOperand::base_disp(X86Reg::Rbp, offset),
                            src: X86Reg::Rax,
                        });
                    }
                }
            }

            DtalInstr::Not { dst, src, .. } => {
                self.lower_not(*dst, *src);
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

            DtalInstr::Call { target } => {
                self.instructions.push(X86Instr::Call {
                    target: target.clone(),
                });
                // x86-64 ABI: return value is in RAX, but DTAL expects it in R0 (mapped to RDI)
                // Copy return value from RAX to RDI so subsequent reads from R0 work correctly
                self.instructions.push(X86Instr::MovRR {
                    dst: X86Reg::Rdi,
                    src: X86Reg::Rax,
                });
            }

            DtalInstr::Ret => {
                // x86-64 ABI: return value must be in RAX, but DTAL puts it in R0 (mapped to RDI)
                // Copy return value from RDI to RAX before returning
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
                        // Load to scratch register then push
                        let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                        self.instructions.push(X86Instr::MovRM {
                            dst: X86Reg::R11,
                            src: mem,
                        });
                        self.instructions.push(X86Instr::Push { src: X86Reg::R11 });
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
                        // Pop to scratch register then store
                        self.instructions.push(X86Instr::Pop { dst: X86Reg::R11 });
                        let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                        self.instructions.push(X86Instr::MovMR {
                            dst: mem,
                            src: X86Reg::R11,
                        });
                    }
                }
            }

            DtalInstr::Alloca { dst, size, .. } => {
                // Allocate stack space
                let alloc_size = (*size as i32 + 15) & !15; // 16-byte align
                self.instructions.push(X86Instr::SubRI {
                    dst: X86Reg::Rsp,
                    imm: alloc_size,
                });

                // Store pointer in destination
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

            // Annotations don't generate code
            DtalInstr::TypeAnnotation { .. }
            | DtalInstr::ConstraintAssume { .. }
            | DtalInstr::ConstraintAssert { .. } => {}
        }
    }

    /// Lower mov immediate
    fn lower_mov_imm(&mut self, dst: Reg, imm: i64) {
        let loc = self.get_vreg_location(dst);
        match loc {
            Location::Reg(r) => {
                self.instructions.push(X86Instr::MovRI { dst: r, imm });
            }
            Location::Stack(offset) => {
                // For large immediates or stack destinations, use scratch register
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

    /// Lower mov register
    fn lower_mov_reg(&mut self, dst: Reg, src: Reg) {
        let src_loc = self.get_reg_location(src);
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
                // Memory to memory: need scratch register
                let src_mem = MemOperand::base_disp(X86Reg::Rbp, src_off);
                let dst_mem = MemOperand::base_disp(X86Reg::Rbp, dst_off);
                self.instructions.push(X86Instr::MovRM {
                    dst: X86Reg::R11,
                    src: src_mem,
                });
                self.instructions.push(X86Instr::MovMR {
                    dst: dst_mem,
                    src: X86Reg::R11,
                });
            }
        }
    }

    /// Lower binary operation
    fn lower_binop(&mut self, op: BinaryOp, dst: Reg, lhs: Reg, rhs: Reg) {
        let lhs_loc = self.get_reg_location(lhs);
        let rhs_loc = self.get_reg_location(rhs);
        let dst_loc = self.get_vreg_location(dst);

        // Load operands into registers
        let lhs_reg = self.load_to_reg(lhs_loc, X86Reg::Rax);
        let rhs_reg = self.load_to_reg(rhs_loc, X86Reg::R11);

        // Perform operation
        match op {
            BinaryOp::Add => {
                self.instructions.push(X86Instr::AddRR {
                    dst: lhs_reg,
                    src: rhs_reg,
                });
            }
            BinaryOp::Sub => {
                self.instructions.push(X86Instr::SubRR {
                    dst: lhs_reg,
                    src: rhs_reg,
                });
            }
            BinaryOp::Mul => {
                self.instructions.push(X86Instr::ImulRR {
                    dst: lhs_reg,
                    src: rhs_reg,
                });
            }
            BinaryOp::And => {
                self.instructions.push(X86Instr::AndRR {
                    dst: lhs_reg,
                    src: rhs_reg,
                });
            }
            BinaryOp::Or => {
                self.instructions.push(X86Instr::OrRR {
                    dst: lhs_reg,
                    src: rhs_reg,
                });
            }
        }

        // Store result
        self.store_from_reg(lhs_reg, dst_loc);
    }

    /// Lower add immediate
    fn lower_add_imm(&mut self, dst: Reg, src: Reg, imm: i64) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);

        let src_reg = self.load_to_reg(src_loc, X86Reg::Rax);

        if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
            self.instructions.push(X86Instr::AddRI {
                dst: src_reg,
                imm: imm as i32,
            });
        } else {
            // Large immediate: load to scratch and add
            self.instructions.push(X86Instr::MovRI {
                dst: X86Reg::R11,
                imm,
            });
            self.instructions.push(X86Instr::AddRR {
                dst: src_reg,
                src: X86Reg::R11,
            });
        }

        self.store_from_reg(src_reg, dst_loc);
    }

    /// Lower load instruction
    fn lower_load(&mut self, dst: Reg, base: Reg, offset: Reg) {
        let base_loc = self.get_reg_location(base);
        let offset_loc = self.get_reg_location(offset);
        let dst_loc = self.get_vreg_location(dst);

        let base_reg = self.load_to_reg(base_loc, X86Reg::Rax);
        let offset_reg = self.load_to_reg(offset_loc, X86Reg::R10);

        // Load from [base + offset * 8]
        let mem = MemOperand::base_index_disp(base_reg, offset_reg, 8, 0);
        let result_reg = match dst_loc {
            Location::Reg(r) => r,
            Location::Stack(_) => X86Reg::R11,
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

    /// Lower store instruction
    fn lower_store(&mut self, base: Reg, offset: Reg, src: Reg) {
        let base_loc = self.get_reg_location(base);
        let offset_loc = self.get_reg_location(offset);
        let src_loc = self.get_reg_location(src);

        let base_reg = self.load_to_reg(base_loc, X86Reg::Rax);
        let offset_reg = self.load_to_reg(offset_loc, X86Reg::R10);
        let src_reg = self.load_to_reg(src_loc, X86Reg::R11);

        // Store to [base + offset * 8]
        let mem = MemOperand::base_index_disp(base_reg, offset_reg, 8, 0);
        self.instructions.push(X86Instr::MovMR {
            dst: mem,
            src: src_reg,
        });
    }

    /// Lower cmp instruction
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

    /// Lower cmp immediate instruction
    fn lower_cmp_imm(&mut self, lhs: Reg, imm: i64) {
        let lhs_loc = self.get_reg_location(lhs);
        let lhs_reg = self.load_to_reg(lhs_loc, X86Reg::Rax);

        if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
            self.instructions.push(X86Instr::CmpRI {
                lhs: lhs_reg,
                imm: imm as i32,
            });
        } else {
            // Large immediate
            self.instructions.push(X86Instr::MovRI {
                dst: X86Reg::R11,
                imm,
            });
            self.instructions.push(X86Instr::CmpRR {
                lhs: lhs_reg,
                rhs: X86Reg::R11,
            });
        }
    }

    /// Lower not instruction
    fn lower_not(&mut self, dst: Reg, src: Reg) {
        let src_loc = self.get_reg_location(src);
        let dst_loc = self.get_vreg_location(dst);

        let src_reg = self.load_to_reg(src_loc, X86Reg::Rax);
        self.instructions.push(X86Instr::Not { dst: src_reg });
        self.store_from_reg(src_reg, dst_loc);
    }

    /// Get location of a register (handles both virtual and physical)
    fn get_reg_location(&self, reg: Reg) -> Location {
        match reg {
            Reg::Virtual(vreg) => self.get_vreg_location(Reg::Virtual(vreg)),
            Reg::Physical(preg) => {
                use crate::backend::dtal::regs::PhysicalReg;
                // Map DTAL physical registers to x86-64 ABI registers
                let x86_reg = match preg {
                    PhysicalReg::R0 => X86Reg::Rdi, // 1st arg
                    PhysicalReg::R1 => X86Reg::Rsi, // 2nd arg
                    PhysicalReg::R2 => X86Reg::Rdx, // 3rd arg
                    PhysicalReg::R3 => X86Reg::Rcx, // 4th arg
                    PhysicalReg::R4 => X86Reg::R8,  // 5th arg
                    PhysicalReg::R5 => X86Reg::R9,  // 6th arg
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

    /// Get location of a virtual register from allocation
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

    /// Load a value from a location into a register
    fn load_to_reg(&mut self, loc: Location, preferred: X86Reg) -> X86Reg {
        match loc {
            Location::Reg(r) => r,
            Location::Stack(offset) => {
                let mem = MemOperand::base_disp(X86Reg::Rbp, offset);
                self.instructions.push(X86Instr::MovRM {
                    dst: preferred,
                    src: mem,
                });
                preferred
            }
        }
    }

    /// Store a value from a register to a location
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

    /// Move from a physical register to a virtual register location
    fn move_to_vreg(&mut self, vreg: VirtualReg, src: X86Reg) {
        let dst_loc = self.allocation.allocation.get(&vreg).copied();
        if let Some(loc) = dst_loc {
            self.store_from_reg(src, loc);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{DtalBlock, TypeState};
    use crate::common::types::IType;

    #[test]
    fn test_lower_simple_function() {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        let func = DtalFunction {
            name: "add".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 10,
                        ty: IType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 20,
                        ty: IType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v2,
                        lhs: v0,
                        rhs: v1,
                        ty: IType::Int,
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

        // Should have prologue, body, and epilogue
        assert!(!x86_func.instructions.is_empty());

        // Print instructions for debugging
        for instr in &x86_func.instructions {
            println!("{}", instr);
        }
    }

    #[test]
    fn test_lower_with_branch() {
        use crate::backend::dtal::instr::CmpOp;

        let v0 = Reg::Virtual(VirtualReg(0));

        let func = DtalFunction {
            name: "branch_test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            blocks: vec![
                DtalBlock {
                    label: "entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v0,
                            imm: 5,
                            ty: IType::Int,
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
                            ty: IType::Int,
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
                            ty: IType::Int,
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

        // Should have branch instructions
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
