//! TAST to TIR lowering
//!
//! This module implements the lowering pass that converts the typed AST
//! from the frontend into TIR (Typed Intermediate Representation) in SSA form.
//!
//! # Modules
//!
//! - `context`: Lowering context for tracking state
//! - `expr`: Expression lowering
//! - `stmt`: Statement lowering
//! - `function`: Function lowering
//!
//! # Usage
//!
//! ```ignore
//! use veritas::backend::lower::lower_program;
//!
//! let tir_program = lower_program(&tast_program);
//! ```

pub mod context;
pub mod expr;
pub mod function;
pub mod stmt;

// Re-exports
pub use context::LoweringContext;
pub use function::lower_function;

use crate::backend::tir::TirProgram;
use crate::common::tast::TProgram;

/// Lower a typed program to TIR
pub fn lower_program<'src>(program: &TProgram<'src>) -> TirProgram<'src> {
    TirProgram {
        functions: program
            .functions
            .iter()
            .map(|f| lower_function(f))
            .collect(),
    }
}
