//! x86-64 Register Definitions
//!
//! This module defines the x86-64 register set following the System V AMD64 ABI.

use std::fmt;

/// x86-64 General Purpose Registers (64-bit)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum X86Reg {
    // Caller-saved registers (volatile)
    Rax, // Return value, accumulator
    Rcx, // 4th argument
    Rdx, // 3rd argument
    Rsi, // 2nd argument
    Rdi, // 1st argument
    R8,  // 5th argument
    R9,  // 6th argument
    R10, // Caller-saved
    R11, // Caller-saved

    // Callee-saved registers (non-volatile)
    Rbx, // Callee-saved
    Rbp, // Base pointer (callee-saved)
    R12, // Callee-saved
    R13, // Callee-saved
    R14, // Callee-saved
    R15, // Callee-saved

    // Special registers
    Rsp, // Stack pointer
}

impl X86Reg {
    /// Registers available for allocation (excludes RSP, RBP, R11)
    /// Note: R11 is reserved as a scratch register for x86 lowering
    pub const ALLOCATABLE: &'static [X86Reg] = &[
        X86Reg::Rax,
        X86Reg::Rcx,
        X86Reg::Rdx,
        X86Reg::Rsi,
        X86Reg::Rdi,
        X86Reg::R8,
        X86Reg::R9,
        X86Reg::R10,
        // R11 reserved for scratch
        X86Reg::Rbx,
        X86Reg::R12,
        X86Reg::R13,
        X86Reg::R14,
        X86Reg::R15,
    ];

    /// Caller-saved registers (must be saved by caller if live across call)
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

    /// Callee-saved registers (must be preserved by callee)
    pub const CALLEE_SAVED: &'static [X86Reg] = &[
        X86Reg::Rbx,
        X86Reg::Rbp,
        X86Reg::R12,
        X86Reg::R13,
        X86Reg::R14,
        X86Reg::R15,
    ];

    /// Argument registers (System V AMD64 ABI order)
    pub const ARG_REGS: &'static [X86Reg] = &[
        X86Reg::Rdi, // 1st argument
        X86Reg::Rsi, // 2nd argument
        X86Reg::Rdx, // 3rd argument
        X86Reg::Rcx, // 4th argument
        X86Reg::R8,  // 5th argument
        X86Reg::R9,  // 6th argument
    ];

    /// Return value register
    pub const RETURN_REG: X86Reg = X86Reg::Rax;

    /// Stack pointer
    pub const STACK_PTR: X86Reg = X86Reg::Rsp;

    /// Base pointer
    pub const BASE_PTR: X86Reg = X86Reg::Rbp;

    /// Get the register encoding for ModR/M byte
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

    /// Check if register requires REX.B prefix (R8-R15)
    pub fn needs_rex_b(self) -> bool {
        self.encoding() >= 8
    }

    /// Check if register requires REX.R prefix when used as reg field (R8-R15)
    pub fn needs_rex_r(self) -> bool {
        self.encoding() >= 8
    }

    /// Get the 3-bit encoding (lower 3 bits of register number)
    pub fn reg3(self) -> u8 {
        self.encoding() & 0x7
    }

    /// Check if this is a caller-saved register
    pub fn is_caller_saved(self) -> bool {
        Self::CALLER_SAVED.contains(&self)
    }

    /// Check if this is a callee-saved register
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

/// Location of a value - either a register or stack slot
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    /// Value is in a register
    Reg(X86Reg),
    /// Value is on the stack at offset from RBP
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
