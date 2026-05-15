//! x86-64 instruction definitions.
//!
//! The backend lowers DTAL into this small instruction IR before machine-code
//! encoding.
//!
//! # Example
//!
//! ```text
//! Label("main")
//! Push rbp
//! MovRR rbp, rsp
//! ...
//! Ret
//! ```
//!
//! # Design Notes
//!
//! This IR models only the instruction subset the backend emits. Pseudo
//! instructions such as labels are resolved by
//! [`crate::backend::x86_64::encode`] before bytes are written.
//!
//! # Related Modules
//!
//! [`crate::backend::x86_64::regs`] defines register metadata, and
//! [`crate::backend::x86_64::lower`] emits this IR from DTAL.

use super::regs::X86Reg;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Condition {
    E,
    Ne,
    L,
    Le,
    G,
    Ge,
    B,
    Be,
    A,
    Ae,
}

impl Condition {
    pub fn cc_byte(self) -> u8 {
        match self {
            Condition::E => 0x84,
            Condition::Ne => 0x85,
            Condition::L => 0x8C,
            Condition::Le => 0x8E,
            Condition::G => 0x8F,
            Condition::Ge => 0x8D,
            Condition::B => 0x82,
            Condition::Be => 0x86,
            Condition::A => 0x87,
            Condition::Ae => 0x83,
        }
    }

    pub fn setcc_byte(self) -> u8 {
        match self {
            Condition::E => 0x94,
            Condition::Ne => 0x95,
            Condition::L => 0x9C,
            Condition::Le => 0x9E,
            Condition::G => 0x9F,
            Condition::Ge => 0x9D,
            Condition::B => 0x92,
            Condition::Be => 0x96,
            Condition::A => 0x97,
            Condition::Ae => 0x93,
        }
    }

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemOperand {
    pub base: X86Reg,
    pub index: Option<(X86Reg, u8)>,
    pub disp: i32,
}

impl MemOperand {
    pub fn base_disp(base: X86Reg, disp: i32) -> Self {
        Self {
            base,
            index: None,
            disp,
        }
    }

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

#[derive(Clone, Debug)]
pub enum X86Instr {
    MovRR { dst: X86Reg, src: X86Reg },
    MovRI { dst: X86Reg, imm: i64 },
    MovRM { dst: X86Reg, src: MemOperand },
    MovMR { dst: MemOperand, src: X86Reg },
    MovMI { dst: MemOperand, imm: i32 },

    Lea { dst: X86Reg, src: MemOperand },

    AddRR { dst: X86Reg, src: X86Reg },
    AddRI { dst: X86Reg, imm: i32 },
    AddRM { dst: X86Reg, src: MemOperand },

    SubRR { dst: X86Reg, src: X86Reg },
    SubRI { dst: X86Reg, imm: i32 },
    SubRM { dst: X86Reg, src: MemOperand },

    ImulRR { dst: X86Reg, src: X86Reg },
    ImulRRI { dst: X86Reg, src: X86Reg, imm: i32 },

    Cqo,

    IdivR { src: X86Reg },

    Neg { dst: X86Reg },

    CmpRR { lhs: X86Reg, rhs: X86Reg },
    CmpRI { lhs: X86Reg, imm: i32 },
    CmpRM { lhs: X86Reg, rhs: MemOperand },

    TestRR { lhs: X86Reg, rhs: X86Reg },
    TestRI { lhs: X86Reg, imm: i32 },

    SetCC { dst: X86Reg, cond: Condition },

    AndRR { dst: X86Reg, src: X86Reg },
    AndRI { dst: X86Reg, imm: i32 },

    OrRR { dst: X86Reg, src: X86Reg },
    OrRI { dst: X86Reg, imm: i32 },

    XorRR { dst: X86Reg, src: X86Reg },
    XorRI { dst: X86Reg, imm: i32 },

    Not { dst: X86Reg },

    ShlCl { dst: X86Reg },
    ShrCl { dst: X86Reg },
    ShlRI { dst: X86Reg, imm: u8 },
    ShrRI { dst: X86Reg, imm: u8 },

    Jmp { target: String },
    JmpRel { offset: i32 },

    Jcc { cond: Condition, target: String },
    JccRel { cond: Condition, offset: i32 },

    Call { target: String },
    CallRel { offset: i32 },

    Ret,

    Push { src: X86Reg },
    PushI { imm: i32 },

    Pop { dst: X86Reg },

    Syscall,

    InAlDx,
    OutDxAl,

    Label { name: String },

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

            X86Instr::Cqo => write!(f, "cqo"),
            X86Instr::IdivR { src } => write!(f, "idiv {}", src),

            X86Instr::Neg { dst } => write!(f, "neg {}", dst),

            X86Instr::CmpRR { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),
            X86Instr::CmpRI { lhs, imm } => write!(f, "cmp {}, {}", lhs, imm),
            X86Instr::CmpRM { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),

            X86Instr::TestRR { lhs, rhs } => write!(f, "test {}, {}", lhs, rhs),
            X86Instr::TestRI { lhs, imm } => write!(f, "test {}, {}", lhs, imm),

            X86Instr::SetCC { dst, cond } => write!(f, "set{} {}", cond, dst),

            X86Instr::AndRR { dst, src } => write!(f, "and {}, {}", dst, src),
            X86Instr::AndRI { dst, imm } => write!(f, "and {}, {}", dst, imm),

            X86Instr::OrRR { dst, src } => write!(f, "or {}, {}", dst, src),
            X86Instr::OrRI { dst, imm } => write!(f, "or {}, {}", dst, imm),

            X86Instr::XorRR { dst, src } => write!(f, "xor {}, {}", dst, src),
            X86Instr::XorRI { dst, imm } => write!(f, "xor {}, {}", dst, imm),

            X86Instr::Not { dst } => write!(f, "not {}", dst),
            X86Instr::ShlCl { dst } => write!(f, "shl {}, cl", dst),
            X86Instr::ShrCl { dst } => write!(f, "shr {}, cl", dst),
            X86Instr::ShlRI { dst, imm } => write!(f, "shl {}, {}", dst, imm),
            X86Instr::ShrRI { dst, imm } => write!(f, "shr {}, {}", dst, imm),

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
            X86Instr::InAlDx => write!(f, "in al, dx"),
            X86Instr::OutDxAl => write!(f, "out dx, al"),

            X86Instr::Label { name } => write!(f, "{}:", name),
            X86Instr::Comment { text } => write!(f, "; {}", text),
        }
    }
}

#[derive(Clone, Debug)]
pub struct X86Function {
    pub name: String,
    pub instructions: Vec<X86Instr>,
}

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
