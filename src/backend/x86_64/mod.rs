//! x86-64 Backend
//!
//! This module provides the x86-64 code generation backend including:
//! - Register definitions
//! - Instruction types
//! - DTAL to x86-64 lowering
//! - Binary encoding
//!
//! # Pipeline
//!
//! ```text
//! DTAL (virtual registers)
//!     │
//!     ▼ Register Allocation
//! DTAL (physical registers)
//!     │
//!     ▼ Lowering
//! x86-64 Instructions
//!     │
//!     ▼ Encoding
//! Machine Code
//! ```

pub mod instr;
pub mod regs;

pub use instr::{Condition, MemOperand, X86Function, X86Instr, X86Program};
pub use regs::{Location, X86Reg};
