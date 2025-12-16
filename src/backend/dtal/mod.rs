//! DTAL (Dependently Typed Assembly Language) AST definitions
//!
//! This module defines the target language for the Veritas compiler.
//! DTAL is inspired by Xi and Harper's dependently typed assembly language,
//! with type annotations that allow independent verification.
//!
//! # Modules
//!
//! - `regs`: Virtual and physical register definitions
//! - `constraints`: Constraint domain (index expressions and propositions)
//! - `instr`: Instructions and program structure

pub mod constraints;
pub mod instr;
pub mod regs;

// Re-export commonly used types
pub use constraints::{Constraint, IndexExpr};
pub use instr::{BinaryOp, CmpOp, DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
pub use regs::{PhysicalReg, Reg, VirtualReg, VirtualRegAllocator};
