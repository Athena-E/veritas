//! Veritas Compiler Backend
//!
//! This module implements the backend of the Veritas compiler, which translates
//! the typed AST from the frontend into DTAL (Dependently Typed Assembly Language)
//! and optionally to native x86-64 machine code.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
//! │   Typed AST     │────▶│  TIR Lowering   │────▶│  DTAL Emitter   │
//! │   (TProgram)    │      │   (SSA form)    │      │                 │
//! └─────────────────┘      └─────────────────┘      └─────────────────┘
//!                                                         │
//!                                                         ▼
//!                                                 ┌─────────────────┐
//!                                                 │   x86-64        │
//!                                                 │   Assembler     │
//!                                                 └─────────────────┘
//! ```
//!
//! # Modules
//!
//! - `dtal`: DTAL AST definitions (target language)
//! - `tir`: Typed Intermediate Representation with SSA
//! - `lower`: TAST to TIR lowering
//! - `x86_64`: x86-64 code generation backend

pub mod codegen;
pub mod dtal;
pub mod elf;
pub mod emit;
pub mod lower;
pub mod regalloc;
pub mod tir;
pub mod x86_64;

// Re-export commonly used types from dtal
pub use dtal::{Constraint, IndexExpr, VirtualReg, VirtualRegAllocator};

// Re-export commonly used types from tir
pub use tir::{
    BasicBlock, BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr, TirProgram,
};

// Re-export lowering function
pub use lower::lower_program;

// Re-export codegen function
pub use codegen::codegen_program;

// Re-export emit function
pub use emit::emit_program;
