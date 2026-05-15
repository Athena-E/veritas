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
//! ```no_run
//! use veritas::middle::lower_program;
//! use veritas::frontend::typechecker::check_program;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Given a typed program from the frontend
//! # let tast_program: veritas::common::tast::TProgram = todo!();
//! let tir_program = lower_program(&tast_program);
//! # Ok(())
//! # }
//! ```

pub mod context;
pub mod expr;
pub mod function;
pub mod stmt;
#[cfg(test)]
mod tests;

pub use context::LoweringContext;
pub use function::lower_function;

use crate::common::tast::TProgram;
use crate::common::types::IType;
use crate::middle::tir::TirProgram;

pub fn lower_program<'src>(program: &TProgram<'src>) -> TirProgram<'src> {
    TirProgram {
        functions: program
            .functions
            .iter()
            .map(|f| lower_function(f))
            .collect(),
    }
}

pub fn widen_itype(ty: IType<'_>) -> IType<'_> {
    match ty {
        IType::SingletonInt(_) => IType::Int,
        IType::RefinedInt { base, .. } => base.as_ref().clone(),
        IType::Unit => IType::Int,
        other => other,
    }
}
