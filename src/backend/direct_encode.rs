//! Trivial Physical DTAL → x86-64 Encoder
//!
//! Maps physically-allocated DTAL instructions 1:1 to x86-64 instructions,
//! then uses the existing x86 instruction encoder to produce machine code.
//!
//! This module is part of the Trusted Computing Base. Its correctness is
//! critical but easy to audit — each DTAL instruction maps to exactly one
//! x86 instruction (or a small fixed sequence for Prologue/Epilogue).

use crate::backend::dtal::instr::{BinaryOp, CmpOp, DtalFunction, DtalInstr, DtalProgram};
use crate::backend::dtal::regs::{PhysicalReg, Reg};
use crate::backend::x86_64::encode::{EncodedProgram, Encoder};
use crate::backend::x86_64::instr::{Condition, MemOperand, X86Function, X86Instr, X86Program};
use crate::backend::x86_64::regs::X86Reg;

/// Map a DTAL PhysicalReg to an x86-64 register.
fn phys_to_x86(preg: PhysicalReg) -> X86Reg {
    match preg {
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
        PhysicalReg::LR => X86Reg::Rax,
        PhysicalReg::SP => X86Reg::Rsp,
        PhysicalReg::FP => X86Reg::Rbp,
        PhysicalReg::R13 | PhysicalReg::R14 | PhysicalReg::R15 => X86Reg::Rax,
    }
}

fn reg_to_x86(reg: &Reg) -> X86Reg {
    match reg {
        Reg::Physical(preg) => phys_to_x86(*preg),
        Reg::Virtual(v) => panic!(
            "Virtual register v{} in physical DTAL — physalloc pass missed it",
            v.0
        ),
    }
}

fn cmpop_to_condition(op: CmpOp) -> Condition {
    match op {
        CmpOp::Eq => Condition::E,
        CmpOp::Ne => Condition::Ne,
        CmpOp::Lt => Condition::L,
        CmpOp::Le => Condition::Le,
        CmpOp::Gt => Condition::G,
        CmpOp::Ge => Condition::Ge,
    }
}

/// Encode a physically-allocated DTAL program to machine code.
pub fn encode_physical_dtal(program: &DtalProgram) -> EncodedProgram {
    let x86_program = lower_to_x86(program);
    let mut encoder = Encoder::new();
    encoder.encode_program(&x86_program)
}

fn lower_to_x86(program: &DtalProgram) -> X86Program {
    let mut functions = Vec::new();
    for func in &program.functions {
        if func.blocks.is_empty() {
            continue;
        }
        functions.push(lower_function(func));
    }
    X86Program { functions }
}

fn lower_function(func: &DtalFunction) -> X86Function {
    let mut instructions = Vec::new();

    for block in &func.blocks {
        instructions.push(X86Instr::Label {
            name: block.label.clone(),
        });
        for instr in &block.instructions {
            lower_instruction(&mut instructions, instr);
        }
    }

    X86Function {
        name: func.name.clone(),
        instructions,
    }
}

fn lower_instruction(out: &mut Vec<X86Instr>, instr: &DtalInstr) {
    match instr {
        DtalInstr::MovImm { dst, imm, .. } => {
            out.push(X86Instr::MovRI {
                dst: reg_to_x86(dst),
                imm: i64::try_from(*imm).expect("immediate value exceeds i64 range"),
            });
        }

        DtalInstr::MovReg { dst, src, .. }
        | DtalInstr::AliasBorrow { dst, src, .. }
        | DtalInstr::MoveOwned { dst, src, .. } => {
            let d = reg_to_x86(dst);
            let s = reg_to_x86(src);
            if d != s {
                out.push(X86Instr::MovRR { dst: d, src: s });
            }
        }

        DtalInstr::BinOp { op, dst, rhs, .. } => {
            let d = reg_to_x86(dst);
            let r = reg_to_x86(rhs);
            match op {
                BinaryOp::Add => out.push(X86Instr::AddRR { dst: d, src: r }),
                BinaryOp::Sub => out.push(X86Instr::SubRR { dst: d, src: r }),
                BinaryOp::Mul => out.push(X86Instr::ImulRR { dst: d, src: r }),
                BinaryOp::BitAnd | BinaryOp::And => out.push(X86Instr::AndRR { dst: d, src: r }),
                BinaryOp::BitOr | BinaryOp::Or => out.push(X86Instr::OrRR { dst: d, src: r }),
                BinaryOp::BitXor => out.push(X86Instr::XorRR { dst: d, src: r }),
                BinaryOp::Shl => out.push(X86Instr::ShlCl { dst: d }),
                BinaryOp::Shr => out.push(X86Instr::ShrCl { dst: d }),
                BinaryOp::Div | BinaryOp::Mod => {
                    unreachable!("Div/Mod expanded to Cqo+Idiv in physical DTAL")
                }
            }
        }

        DtalInstr::AddImm { dst, imm, .. } => {
            out.push(X86Instr::AddRI {
                dst: reg_to_x86(dst),
                imm: *imm as i32,
            });
        }

        DtalInstr::Cmp { lhs, rhs } => {
            out.push(X86Instr::CmpRR {
                lhs: reg_to_x86(lhs),
                rhs: reg_to_x86(rhs),
            });
        }

        DtalInstr::CmpImm { lhs, imm } => {
            out.push(X86Instr::CmpRI {
                lhs: reg_to_x86(lhs),
                imm: *imm as i32,
            });
        }

        DtalInstr::SetCC { dst, cond } => {
            out.push(X86Instr::SetCC {
                dst: reg_to_x86(dst),
                cond: cmpop_to_condition(*cond),
            });
        }

        DtalInstr::Not { dst, .. } => {
            out.push(X86Instr::Not {
                dst: reg_to_x86(dst),
            });
        }

        DtalInstr::Neg { dst, .. } => {
            out.push(X86Instr::Neg {
                dst: reg_to_x86(dst),
            });
        }

        DtalInstr::ShlImm { dst, imm, .. } => {
            out.push(X86Instr::ShlRI {
                dst: reg_to_x86(dst),
                imm: *imm,
            });
        }
        DtalInstr::ShrImm { dst, imm, .. } => {
            out.push(X86Instr::ShrRI {
                dst: reg_to_x86(dst),
                imm: *imm,
            });
        }

        DtalInstr::Load {
            dst, base, offset, ..
        } => {
            out.push(X86Instr::MovRM {
                dst: reg_to_x86(dst),
                src: MemOperand::base_index_disp(reg_to_x86(base), reg_to_x86(offset), 8, 0),
            });
        }

        DtalInstr::LoadOp {
            op,
            dst,
            base,
            offset,
            other,
            ..
        } => {
            use crate::backend::dtal::instr::BinaryOp;
            let other_x86 = reg_to_x86(other);
            let mem = MemOperand::base_index_disp(reg_to_x86(base), reg_to_x86(offset), 8, 0);
            let instr = match op {
                BinaryOp::Add => X86Instr::AddRM {
                    dst: other_x86,
                    src: mem,
                },
                BinaryOp::Sub => X86Instr::SubRM {
                    dst: other_x86,
                    src: mem,
                },
                _ => panic!("LoadOp only supports Add/Sub"),
            };
            out.push(instr);
            let dst_x86 = reg_to_x86(dst);
            if dst_x86 != other_x86 {
                out.push(X86Instr::MovRR {
                    dst: dst_x86,
                    src: other_x86,
                });
            }
        }

        DtalInstr::Store { base, offset, src } => {
            out.push(X86Instr::MovMR {
                dst: MemOperand::base_index_disp(reg_to_x86(base), reg_to_x86(offset), 8, 0),
                src: reg_to_x86(src),
            });
        }

        DtalInstr::Jmp { target } => {
            out.push(X86Instr::Jmp {
                target: target.clone(),
            });
        }

        DtalInstr::Branch { cond, target } => {
            out.push(X86Instr::Jcc {
                cond: cmpop_to_condition(*cond),
                target: target.clone(),
            });
        }

        DtalInstr::Call { target, .. } => {
            out.push(X86Instr::Call {
                target: target.clone(),
            });
        }

        DtalInstr::Ret => {
            out.push(X86Instr::Ret);
        }

        DtalInstr::Push { src, .. } => {
            out.push(X86Instr::Push {
                src: reg_to_x86(src),
            });
        }

        DtalInstr::Pop { dst, .. } => {
            out.push(X86Instr::Pop {
                dst: reg_to_x86(dst),
            });
        }

        DtalInstr::Alloca { dst, size, .. } => {
            let aligned = ((*size as i32) + 15) & !15;
            out.push(X86Instr::SubRI {
                dst: X86Reg::Rsp,
                imm: aligned,
            });
            out.push(X86Instr::MovRR {
                dst: reg_to_x86(dst),
                src: X86Reg::Rsp,
            });
        }

        DtalInstr::PortIn { .. } => {
            // in al, dx (port in DX, result in AL)
            out.push(X86Instr::InAlDx);
            // movzx rax, al (zero-extend byte to 64-bit)
            // Note: the runtime blob handles this; in direct encode we trust
            // the physalloc has set up the registers correctly
        }

        DtalInstr::PortOut { .. } => {
            // out dx, al (value in AL, port in DX)
            out.push(X86Instr::OutDxAl);
        }

        DtalInstr::Cqo => {
            out.push(X86Instr::Cqo);
        }

        DtalInstr::Idiv { src } => {
            out.push(X86Instr::IdivR {
                src: reg_to_x86(src),
            });
        }

        DtalInstr::SpillStore { src, offset, .. } => {
            out.push(X86Instr::MovMR {
                dst: MemOperand::base_disp(X86Reg::Rbp, *offset),
                src: reg_to_x86(src),
            });
        }

        DtalInstr::SpillLoad { dst, offset, .. } => {
            out.push(X86Instr::MovRM {
                dst: reg_to_x86(dst),
                src: MemOperand::base_disp(X86Reg::Rbp, *offset),
            });
        }

        DtalInstr::Prologue {
            frame_size,
            callee_saved,
        } => {
            out.push(X86Instr::Push { src: X86Reg::Rbp });
            out.push(X86Instr::MovRR {
                dst: X86Reg::Rbp,
                src: X86Reg::Rsp,
            });
            for reg in callee_saved {
                out.push(X86Instr::Push {
                    src: reg_to_x86(reg),
                });
            }
            if *frame_size > 0 {
                out.push(X86Instr::SubRI {
                    dst: X86Reg::Rsp,
                    imm: *frame_size as i32,
                });
            }
        }

        DtalInstr::Epilogue { callee_saved } => {
            out.push(X86Instr::MovRR {
                dst: X86Reg::Rsp,
                src: X86Reg::Rbp,
            });
            let callee_size = (callee_saved.len() * 8) as i32;
            if callee_size > 0 {
                out.push(X86Instr::SubRI {
                    dst: X86Reg::Rsp,
                    imm: callee_size,
                });
            }
            for reg in callee_saved.iter().rev() {
                out.push(X86Instr::Pop {
                    dst: reg_to_x86(reg),
                });
            }
            out.push(X86Instr::Pop { dst: X86Reg::Rbp });
        }

        DtalInstr::TypeAnnotation { .. }
        | DtalInstr::ConstraintAssert { .. }
        | DtalInstr::DropOwned { .. } => {}
    }
}
