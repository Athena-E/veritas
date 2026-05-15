//! x86-64 Register Definitions
//!
//! This module defines the x86-64 register set following the System V AMD64 ABI.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum X86Reg {
    Rax,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,

    Rbx,
    Rbp,
    R12,
    R13,
    R14,
    R15,

    Rsp,
}

impl X86Reg {
    pub const ALLOCATABLE: &'static [X86Reg] = &[
        X86Reg::Rcx,
        X86Reg::Rsi,
        X86Reg::Rdi,
        X86Reg::R8,
        X86Reg::R9,
        X86Reg::R10,
        X86Reg::Rbx,
        X86Reg::R12,
        X86Reg::R13,
        X86Reg::R14,
        X86Reg::R15,
    ];

    pub const CALLER_SAVED: &'static [X86Reg] = &[
        X86Reg::Rax,
        X86Reg::Rcx,
        X86Reg::Rdx,
        X86Reg::Rsi,
        X86Reg::Rdi,
        X86Reg::R8,
        X86Reg::R9,
        X86Reg::R10,
        X86Reg::R11,
    ];

    pub const CALLEE_SAVED: &'static [X86Reg] = &[
        X86Reg::Rbx,
        X86Reg::Rbp,
        X86Reg::R12,
        X86Reg::R13,
        X86Reg::R14,
        X86Reg::R15,
    ];

    pub const ARG_REGS: &'static [X86Reg] = &[
        X86Reg::Rdi,
        X86Reg::Rsi,
        X86Reg::Rdx,
        X86Reg::Rcx,
        X86Reg::R8,
        X86Reg::R9,
    ];

    pub const RETURN_REG: X86Reg = X86Reg::Rax;

    pub const STACK_PTR: X86Reg = X86Reg::Rsp;

    pub const BASE_PTR: X86Reg = X86Reg::Rbp;

    pub fn encoding(self) -> u8 {
        match self {
            X86Reg::Rax => 0,
            X86Reg::Rcx => 1,
            X86Reg::Rdx => 2,
            X86Reg::Rbx => 3,
            X86Reg::Rsp => 4,
            X86Reg::Rbp => 5,
            X86Reg::Rsi => 6,
            X86Reg::Rdi => 7,
            X86Reg::R8 => 8,
            X86Reg::R9 => 9,
            X86Reg::R10 => 10,
            X86Reg::R11 => 11,
            X86Reg::R12 => 12,
            X86Reg::R13 => 13,
            X86Reg::R14 => 14,
            X86Reg::R15 => 15,
        }
    }

    pub fn needs_rex_b(self) -> bool {
        self.encoding() >= 8
    }

    pub fn needs_rex_r(self) -> bool {
        self.encoding() >= 8
    }

    pub fn needs_rex_for_byte(self) -> bool {
        self.encoding() >= 4
    }

    pub fn reg3(self) -> u8 {
        self.encoding() & 0x7
    }

    pub fn is_caller_saved(self) -> bool {
        Self::CALLER_SAVED.contains(&self)
    }

    pub fn is_callee_saved(self) -> bool {
        Self::CALLEE_SAVED.contains(&self)
    }
}

impl fmt::Display for X86Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            X86Reg::Rax => "rax",
            X86Reg::Rbx => "rbx",
            X86Reg::Rcx => "rcx",
            X86Reg::Rdx => "rdx",
            X86Reg::Rsi => "rsi",
            X86Reg::Rdi => "rdi",
            X86Reg::Rsp => "rsp",
            X86Reg::Rbp => "rbp",
            X86Reg::R8 => "r8",
            X86Reg::R9 => "r9",
            X86Reg::R10 => "r10",
            X86Reg::R11 => "r11",
            X86Reg::R12 => "r12",
            X86Reg::R13 => "r13",
            X86Reg::R14 => "r14",
            X86Reg::R15 => "r15",
        };
        write!(f, "{}", name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    Reg(X86Reg),
    Stack(i32),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Reg(r) => write!(f, "{}", r),
            Location::Stack(offset) => {
                if *offset >= 0 {
                    write!(f, "[rbp+{}]", offset)
                } else {
                    write!(f, "[rbp{}]", offset)
                }
            }
        }
    }
}
#[cfg(test)]

mod tests {
    use super::*;
    #[test]

    fn test_register_encoding() {
        assert_eq!(X86Reg::Rax.encoding(), 0);
        assert_eq!(X86Reg::Rcx.encoding(), 1);
        assert_eq!(X86Reg::R8.encoding(), 8);
        assert_eq!(X86Reg::R15.encoding(), 15);
    }
    #[test]

    fn test_rex_prefix_needed() {
        assert!(!X86Reg::Rax.needs_rex_b());
        assert!(!X86Reg::Rdi.needs_rex_b());
        assert!(X86Reg::R8.needs_rex_b());
        assert!(X86Reg::R15.needs_rex_b());
    }
    #[test]

    fn test_arg_registers() {
        assert_eq!(X86Reg::ARG_REGS.len(), 6);
        assert_eq!(X86Reg::ARG_REGS[0], X86Reg::Rdi);
        assert_eq!(X86Reg::ARG_REGS[5], X86Reg::R9);
    }
}
