// src/frontend/typechecker/mod.rs

pub mod check;
pub mod context;
pub mod error;
pub mod helpers;
pub mod report;
pub mod smt;
pub mod subtyping;
pub mod synthesize;

#[cfg(test)]
mod tests;

// Re-export commonly used types
pub use check::{check_function, check_program, check_stmt, check_stmts};
pub use context::{MutableBinding, TypingContext, VarBinding};
pub use error::TypeError;
pub use helpers::{check_array_bounds, check_array_bounds_expr, extract_proposition, join_op, negate_proposition};
pub use report::report_type_error;
pub use smt::check_provable;
pub use subtyping::is_subtype;
pub use synthesize::synth_expr;
