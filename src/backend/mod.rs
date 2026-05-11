//! Veritas compiler backend.
//!
//! The backend owns every representation after frontend type checking. It
//! lowers typed AST into [`tir`], emits DTAL through [`codegen`], and can
//! continue through physical allocation and x86-64 encoding for native
//! execution.
//!
//! # Pipeline
//!
//! ```text
//! typed frontend program
//!        |
//!        v
//! TIR: typed SSA control-flow graph
//!        |
//!        v
//! DTAL: verifier-visible assembly with types and constraints
//!        |
//!        v
//! physical DTAL -> x86-64 IR -> encoded program
//! ```
//!
//! # Design Notes
//!
//! DTAL remains the trust boundary: optimisation, register allocation, and
//! lowering may transform programs, but the verifier can re-check the emitted
//! types, ownership facts, contracts, and constraints before machine-code
//! emission. This keeps backend passes useful for performance without making
//! them part of the proof story.
//!
//! # Related Modules
//!
//! - [`tir`] defines the typed SSA IR used by frontend lowering.
//! - [`physalloc`] maps virtual-register DTAL to physical-register DTAL.
//! - [`x86_64`] contains the target instruction IR and encoder.
//! - [`crate::verifier`] validates DTAL before native execution.

pub mod codegen;
pub mod direct_encode;
pub mod dtal;
pub mod elf;
pub mod emit;
pub mod lower;
pub mod optimise;
pub mod physalloc;
pub mod regalloc;
pub mod runtime;
pub mod tir;
pub mod x86_64;

pub use dtal::{Constraint, IndexExpr, VirtualReg, VirtualRegAllocator};

pub use tir::{
    BasicBlock, BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr, TirProgram,
};

pub use lower::lower_program;

pub use codegen::codegen_program;

pub use emit::emit_program;
