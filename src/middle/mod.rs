//! Middle-end representations and lowering.
//!
//! The middle end owns the typed SSA representation used between the frontend
//! and verifier-visible DTAL generation.

pub mod lower;
pub mod tir;

pub use lower::lower_program;
pub use tir::{
    BasicBlock, BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr, TirProgram,
};
