//! Typed Intermediate Representation (TIR) with SSA
//!
//! TIR is a CFG-based intermediate representation in SSA form.
//! Each virtual register (SSA variable) is defined exactly once
//! and carries a type annotation.
//!
//! # Modules
//!
//! - `types`: Core types (BlockId, RegisterState, BinaryOp, etc.)
//! - `phi`: SSA phi nodes for merge points
//! - `instr`: TIR instructions and terminators
//! - `program`: Program, function, and basic block structures
//! - `builder`: Builder utilities for constructing TIR

pub mod builder;
pub mod instr;
pub mod phi;
pub mod program;
pub mod types;

// Re-export commonly used types
pub use builder::{and_constraints, constraint_from_binop, negate_constraint, or_constraints, TirBuilder};
pub use instr::{Terminator, TirInstr};
pub use phi::PhiNode;
pub use program::{BasicBlock, TirFunction, TirProgram};
pub use types::{BinaryOp, BlockId, BlockIdAllocator, BoundsProof, ProofJustification, RegisterState, UnaryOp};
