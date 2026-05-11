//! Shared compiler data structures.
//!
//! The `common` module contains syntax trees, typed syntax trees, semantic
//! types, source spans, and ownership metadata used by both the frontend and
//! backend.
//!
//! # Data Flow
//!
//! ```text
//! ast: surface syntax tree
//!        |
//!        v
//! typechecker
//!        |
//!        v
//! tast::TProgram with types, ownership, and contract metadata
//!        |
//!        v
//! backend lowering
//! ```
//!
//! # Design Notes
//!
//! These types are intentionally representation-focused. They do not perform
//! parsing, type checking, lowering, or verification themselves; instead they
//! define the shared contracts between [`crate::frontend`], [`crate::backend`],
//! and [`crate::verifier`]. Keeping them in one module avoids duplicating type
//! and ownership concepts across compiler stages.
//!
//! # Related Modules
//!
//! - [`ast`] defines source-level tokens, expressions, statements, and programs.
//! - [`tast`] defines typed AST nodes produced by semantic analysis.
//! - [`types`] defines semantic types, values, propositions, and signatures.
//! - [`ownership`] defines parameter and ownership passing modes.
//! - [`span`] carries source locations for diagnostics.

pub mod ast;
pub mod ownership;
pub mod span;
pub mod tast;
pub mod types;
