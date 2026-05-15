mod array_props;
pub mod check;
pub mod context;
pub mod error;
pub mod helpers;
mod ownership;
mod postconditions;
pub mod report;
pub mod smt;
mod substitution;
pub mod subtyping;
pub mod synthesize;
#[cfg(test)]
mod tests;
mod type_utils;

pub use check::{check_program, check_program_bare_metal, check_stmts};
pub use context::{TypingContext, VarBinding};
pub use error::TypeError;
pub use helpers::{
    build_equality_refinement, check_array_bounds_expr, check_divisor_nonzero, extract_proposition,
    join_op, negate_proposition,
};
pub use report::report_type_error;
pub use smt::check_provable;
pub use subtyping::is_subtype;
pub use synthesize::synth_expr;
