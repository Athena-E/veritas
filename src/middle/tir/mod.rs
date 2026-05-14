//! Typed Intermediate Representation (TIR).
//!
//! TIR is the compiler's SSA-based control-flow representation between the
//! typed frontend and DTAL generation. Values are virtual registers with
//! explicit types, blocks end in terminators, and phi nodes model joins.
//!
//! # Shape
//!
//! ```text
//! TirProgram
//!   `- TirFunction
//!        `- BasicBlock
//!             |- PhiNode*
//!             |- TirInstr*
//!             `- Terminator
//! ```
//!
//! # Design Notes
//!
//! TIR keeps frontend types, ownership operations, and proven constraints
//! explicit so [`crate::backend::codegen`] can emit verifier-visible DTAL
//! annotations without re-deriving source-level intent. It uses virtual
//! registers only; physical allocation happens later in
//! [`crate::backend::physalloc`].
//!
//! # Related Modules
//!
//! - [`builder`] contains [`TirBuilder`] and constraint helpers.
//! - [`instr`] defines [`TirInstr`] and [`Terminator`].
//! - [`program`] defines [`TirProgram`], [`TirFunction`], and [`BasicBlock`].
//! - [`phi`] defines [`PhiNode`] for control-flow joins.

pub mod builder;
pub mod instr;
pub mod phi;
pub mod program;
pub mod types;

pub use builder::{
    TirBuilder, and_constraints, constraint_from_binop, negate_constraint, or_constraints,
};
pub use instr::{Terminator, TirInstr};
pub use phi::PhiNode;
pub use program::{BasicBlock, TirFunction, TirProgram};
pub use types::{BinaryOp, BlockId, BlockIdAllocator, RegisterState, UnaryOp};
