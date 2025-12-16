//! Veritas Compiler Backend
//!
//! This module implements the backend of the Veritas compiler, which translates
//! the typed AST from the frontend into DTAL (Dependently Typed Assembly Language).
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
//! │   Typed AST     │────▶│  TIR Lowering   │────▶│  DTAL Emitter   │
//! │   (TProgram)    │     │   (SSA form)    │     │                 │
//! └─────────────────┘     └─────────────────┘     └─────────────────┘
//! ```
//!
//! # Modules
//!
//! - `dtal`: DTAL AST definitions (target language)
//! - `tir`: Typed Intermediate Representation with SSA

pub mod dtal;
pub mod tir;

// Re-export commonly used types from dtal
pub use dtal::{Constraint, IndexExpr, VirtualReg, VirtualRegAllocator};

// Re-export commonly used types from tir
pub use tir::{BasicBlock, BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr, TirProgram};
