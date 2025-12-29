//! x86-64 Instruction Definitions
//!
//! This module defines the x86-64 instruction set used as the target for
//! DTAL lowering.

use super::regs::X86Reg;
use std::fmt;

/// Condition codes for conditional jumps
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Condition {
    /// Equal (ZF=1)
    E,
    /// Not Equal (ZF=0)
    Ne,
    /// Less Than (SF!=OF)
    L,
    /// Less or Equal (ZF=1 or SF!=OF)
    Le,
    /// Greater Than (ZF=0 and SF=OF)
    G,
    /// Greater or Equal (SF=OF)
    Ge,
    /// Below (unsigned less than, CF=1)
    B,
    /// Below or Equal (unsigned, CF=1 or ZF=1)
    Be,
    /// Above (unsigned greater than, CF=0 and ZF=0)
    A,
    /// Above or Equal (unsigned, CF=0)
    Ae,
}

impl Condition {
    /// Get the condition code byte for Jcc instructions
    pub fn cc_byte(self) -> u8 {
        match self {
            Condition::E => 0x84,   // JE/JZ
            Condition::Ne => 0x85,  // JNE/JNZ
            Condition::L => 0x8C,   // JL/JNGE
            Condition::Le => 0x8E,  // JLE/JNG
            Condition::G => 0x8F,   // JG/JNLE
            Condition::Ge => 0x8D,  // JGE/JNL
            Condition::B => 0x82,   // JB/JNAE/JC
            Condition::Be => 0x86,  // JBE/JNA
            Condition::A => 0x87,   // JA/JNBE
            Condition::Ae => 0x83,  // JAE/JNB/JNC
        }
    }

    /// Negate the condition
    pub fn negate(self) -> Condition {
        match self {
            Condition::E => Condition::Ne,
            Condition::Ne => Condition::E,
            Condition::L => Condition::Ge,
            Condition::Le => Condition::G,
            Condition::G => Condition::Le,
            Condition::Ge => Condition::L,
            Condition::B => Condition::Ae,
            Condition::Be => Condition::A,
            Condition::A => Condition::Be,
            Condition::Ae => Condition::B,
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Condition::E => "e",
            Condition::Ne => "ne",
            Condition::L => "l",
            Condition::Le => "le",
            Condition::G => "g",
            Condition::Ge => "ge",
            Condition::B => "b",
            Condition::Be => "be",
            Condition::A => "a",
            Condition::Ae => "ae",
        };
        write!(f, "{}", name)
    }
}

/// Memory operand
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemOperand {
    /// Base register
    pub base: X86Reg,
    /// Optional index register with scale
    pub index: Option<(X86Reg, u8)>, // (reg, scale: 1, 2, 4, or 8)
    /// Displacement
    pub disp: i32,
}

impl MemOperand {
    /// Create a simple base + displacement operand
    pub fn base_disp(base: X86Reg, disp: i32) -> Self {
        Self {
            base,
            index: None,
            disp,
        }
    }

    /// Create a base + index * scale + displacement operand
    pub fn base_index_disp(base: X86Reg, index: X86Reg, scale: u8, disp: i32) -> Self {
        Self {
            base,
            index: Some((index, scale)),
            disp,
        }
    }
}

impl fmt::Display for MemOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        write!(f, "{}", self.base)?;

        if let Some((idx, scale)) = &self.index {
            if *scale == 1 {
                write!(f, "+{}", idx)?;
            } else {
                write!(f, "+{}*{}", idx, scale)?;
            }
        }

        if self.disp > 0 {
            write!(f, "+{}", self.disp)?;
        } else if self.disp < 0 {
            write!(f, "{}", self.disp)?;
        }

        write!(f, "]")
    }
}

/// x86-64 Instructions
#[derive(Clone, Debug)]
pub enum X86Instr {
    // === Data Movement ===
    /// mov reg, reg
    MovRR { dst: X86Reg, src: X86Reg },
    /// mov reg, imm64
    MovRI { dst: X86Reg, imm: i64 },
    /// mov reg, [mem]
    MovRM { dst: X86Reg, src: MemOperand },
    /// mov [mem], reg
    MovMR { dst: MemOperand, src: X86Reg },
    /// mov [mem], imm32 (sign-extended)
    MovMI { dst: MemOperand, imm: i32 },

    /// lea reg, [mem]
    Lea { dst: X86Reg, src: MemOperand },

    // === Arithmetic ===
    /// add reg, reg
    AddRR { dst: X86Reg, src: X86Reg },
    /// add reg, imm32
    AddRI { dst: X86Reg, imm: i32 },
    /// add reg, [mem]
    AddRM { dst: X86Reg, src: MemOperand },

    /// sub reg, reg
    SubRR { dst: X86Reg, src: X86Reg },
    /// sub reg, imm32
    SubRI { dst: X86Reg, imm: i32 },
    /// sub reg, [mem]
    SubRM { dst: X86Reg, src: MemOperand },

    /// imul reg, reg (signed multiply)
    ImulRR { dst: X86Reg, src: X86Reg },
    /// imul reg, reg, imm32
    ImulRRI { dst: X86Reg, src: X86Reg, imm: i32 },

    /// neg reg (two's complement negate)
    Neg { dst: X86Reg },

    // === Comparison ===
    /// cmp reg, reg
    CmpRR { lhs: X86Reg, rhs: X86Reg },
    /// cmp reg, imm32
    CmpRI { lhs: X86Reg, imm: i32 },
    /// cmp reg, [mem]
    CmpRM { lhs: X86Reg, rhs: MemOperand },

    /// test reg, reg (AND without storing result)
    TestRR { lhs: X86Reg, rhs: X86Reg },
    /// test reg, imm32
    TestRI { lhs: X86Reg, imm: i32 },

    // === Logical ===
    /// and reg, reg
    AndRR { dst: X86Reg, src: X86Reg },
    /// and reg, imm32
    AndRI { dst: X86Reg, imm: i32 },

    /// or reg, reg
    OrRR { dst: X86Reg, src: X86Reg },
    /// or reg, imm32
    OrRI { dst: X86Reg, imm: i32 },

    /// xor reg, reg
    XorRR { dst: X86Reg, src: X86Reg },
    /// xor reg, imm32
    XorRI { dst: X86Reg, imm: i32 },

    /// not reg
    Not { dst: X86Reg },

    // === Control Flow ===
    /// jmp label (unconditional)
    Jmp { target: String },
    /// jmp rel32 (offset known)
    JmpRel { offset: i32 },

    /// jcc label (conditional)
    Jcc { cond: Condition, target: String },
    /// jcc rel32 (offset known)
    JccRel { cond: Condition, offset: i32 },

    /// call label
    Call { target: String },
    /// call rel32 (offset known)
    CallRel { offset: i32 },

    /// ret
    Ret,

    // === Stack ===
    /// push reg
    Push { src: X86Reg },
    /// push imm32
    PushI { imm: i32 },

    /// pop reg
    Pop { dst: X86Reg },

    // === System ===
    /// syscall
    Syscall,

    // === Pseudo-instructions (resolved before encoding) ===
    /// Label definition
    Label { name: String },

    /// Comment (for debugging output)
    Comment { text: String },
}

impl fmt::Display for X86Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Instr::MovRR { dst, src } => write!(f, "mov {}, {}", dst, src),
            X86Instr::MovRI { dst, imm } => write!(f, "mov {}, {}", dst, imm),
            X86Instr::MovRM { dst, src } => write!(f, "mov {}, {}", dst, src),
            X86Instr::MovMR { dst, src } => write!(f, "mov {}, {}", dst, src),
            X86Instr::MovMI { dst, imm } => write!(f, "mov {}, {}", dst, imm),
            X86Instr::Lea { dst, src } => write!(f, "lea {}, {}", dst, src),

            X86Instr::AddRR { dst, src } => write!(f, "add {}, {}", dst, src),
            X86Instr::AddRI { dst, imm } => write!(f, "add {}, {}", dst, imm),
            X86Instr::AddRM { dst, src } => write!(f, "add {}, {}", dst, src),

            X86Instr::SubRR { dst, src } => write!(f, "sub {}, {}", dst, src),
            X86Instr::SubRI { dst, imm } => write!(f, "sub {}, {}", dst, imm),
            X86Instr::SubRM { dst, src } => write!(f, "sub {}, {}", dst, src),

            X86Instr::ImulRR { dst, src } => write!(f, "imul {}, {}", dst, src),
            X86Instr::ImulRRI { dst, src, imm } => write!(f, "imul {}, {}, {}", dst, src, imm),

            X86Instr::Neg { dst } => write!(f, "neg {}", dst),

            X86Instr::CmpRR { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),
            X86Instr::CmpRI { lhs, imm } => write!(f, "cmp {}, {}", lhs, imm),
            X86Instr::CmpRM { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),

            X86Instr::TestRR { lhs, rhs } => write!(f, "test {}, {}", lhs, rhs),
            X86Instr::TestRI { lhs, imm } => write!(f, "test {}, {}", lhs, imm),

            X86Instr::AndRR { dst, src } => write!(f, "and {}, {}", dst, src),
            X86Instr::AndRI { dst, imm } => write!(f, "and {}, {}", dst, imm),

            X86Instr::OrRR { dst, src } => write!(f, "or {}, {}", dst, src),
            X86Instr::OrRI { dst, imm } => write!(f, "or {}, {}", dst, imm),

            X86Instr::XorRR { dst, src } => write!(f, "xor {}, {}", dst, src),
            X86Instr::XorRI { dst, imm } => write!(f, "xor {}, {}", dst, imm),

            X86Instr::Not { dst } => write!(f, "not {}", dst),

            X86Instr::Jmp { target } => write!(f, "jmp {}", target),
            X86Instr::JmpRel { offset } => write!(f, "jmp .+{}", offset),

            X86Instr::Jcc { cond, target } => write!(f, "j{} {}", cond, target),
            X86Instr::JccRel { cond, offset } => write!(f, "j{} .+{}", cond, offset),

            X86Instr::Call { target } => write!(f, "call {}", target),
            X86Instr::CallRel { offset } => write!(f, "call .+{}", offset),

            X86Instr::Ret => write!(f, "ret"),

            X86Instr::Push { src } => write!(f, "push {}", src),
            X86Instr::PushI { imm } => write!(f, "push {}", imm),

            X86Instr::Pop { dst } => write!(f, "pop {}", dst),

            X86Instr::Syscall => write!(f, "syscall"),

            X86Instr::Label { name } => write!(f, "{}:", name),
            X86Instr::Comment { text } => write!(f, "; {}", text),
        }
    }
}

/// A sequence of x86-64 instructions forming a function
#[derive(Clone, Debug)]
pub struct X86Function {
    pub name: String,
    pub instructions: Vec<X86Instr>,
}

/// A complete x86-64 program
#[derive(Clone, Debug)]
pub struct X86Program {
    pub functions: Vec<X86Function>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_display() {
        let instr = X86Instr::MovRR {
            dst: X86Reg::Rax,
            src: X86Reg::Rbx,
        };
        assert_eq!(format!("{}", instr), "mov rax, rbx");

        let instr = X86Instr::AddRI {
            dst: X86Reg::Rsp,
            imm: -8,
        };
        assert_eq!(format!("{}", instr), "add rsp, -8");
    }

    #[test]
    fn test_mem_operand_display() {
        let mem = MemOperand::base_disp(X86Reg::Rbp, -8);
        assert_eq!(format!("{}", mem), "[rbp-8]");

        let mem = MemOperand::base_index_disp(X86Reg::Rax, X86Reg::Rcx, 4, 0);
        assert_eq!(format!("{}", mem), "[rax+rcx*4]");
    }

    #[test]
    fn test_condition_negate() {
        assert_eq!(Condition::E.negate(), Condition::Ne);
        assert_eq!(Condition::L.negate(), Condition::Ge);
        assert_eq!(Condition::G.negate(), Condition::Le);
    }
}
