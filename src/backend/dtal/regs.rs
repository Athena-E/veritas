//! Register definitions for DTAL
//!
//! This module defines virtual and physical registers used in DTAL code.

use std::fmt;

/// A virtual register (before physical allocation)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VirtualReg(pub u32);

impl fmt::Display for VirtualReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

/// Allocator for virtual registers
#[derive(Debug, Default)]
pub struct VirtualRegAllocator {
    next_id: u32,
}

impl VirtualRegAllocator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    /// Allocate a fresh virtual register
    pub fn fresh(&mut self) -> VirtualReg {
        let id = self.next_id;
        self.next_id += 1;
        VirtualReg(id)
    }

    /// Get the number of registers allocated so far
    pub fn count(&self) -> u32 {
        self.next_id
    }
}

/// Physical registers (for extension phase)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum PhysicalReg {
    // General purpose registers
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // Special registers
    SP, // Stack pointer
    FP, // Frame pointer
    LR, // Link register (return address)
}

impl PhysicalReg {
    /// Registers available for allocation
    #[allow(dead_code)]
    pub fn allocatable() -> &'static [PhysicalReg] {
        use PhysicalReg::*;
        &[R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11]
    }

    /// Registers used for parameter passing
    #[allow(dead_code)]
    pub fn param_regs() -> &'static [PhysicalReg] {
        use PhysicalReg::*;
        &[R0, R1, R2, R3, R4, R5, R6, R7]
    }

    /// Return value register
    #[allow(dead_code)]
    pub fn return_reg() -> PhysicalReg {
        PhysicalReg::R0
    }
}

impl fmt::Display for PhysicalReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PhysicalReg::*;
        match self {
            R0 => write!(f, "r0"),
            R1 => write!(f, "r1"),
            R2 => write!(f, "r2"),
            R3 => write!(f, "r3"),
            R4 => write!(f, "r4"),
            R5 => write!(f, "r5"),
            R6 => write!(f, "r6"),
            R7 => write!(f, "r7"),
            R8 => write!(f, "r8"),
            R9 => write!(f, "r9"),
            R10 => write!(f, "r10"),
            R11 => write!(f, "r11"),
            R12 => write!(f, "r12"),
            R13 => write!(f, "r13"),
            R14 => write!(f, "r14"),
            R15 => write!(f, "r15"),
            SP => write!(f, "sp"),
            FP => write!(f, "fp"),
            LR => write!(f, "lr"),
        }
    }
}

/// A register that can be either virtual or physical
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Reg {
    Virtual(VirtualReg),
    Physical(PhysicalReg),
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::Virtual(v) => write!(f, "{}", v),
            Reg::Physical(p) => write!(f, "{}", p),
        }
    }
}

impl From<VirtualReg> for Reg {
    fn from(v: VirtualReg) -> Self {
        Reg::Virtual(v)
    }
}

impl From<PhysicalReg> for Reg {
    fn from(p: PhysicalReg) -> Self {
        Reg::Physical(p)
    }
}
