//! Source-language frontend.
//!
//! The frontend turns source text into a typed AST while checking refinement,
//! ownership, borrowing, contract, and target-mode rules.
//!
//! # Pipeline
//!
//! ```text
//! source text
//!   -> lexer::lexer()
//!   -> parser::program_parser()
//!   -> typechecker::check_program()
//!   -> common::tast::TProgram
//! ```
//!
//! # Design Notes
//!
//! Parsing produces the surface AST from [`crate::common::ast`]. Type checking
//! enriches that AST into [`crate::common::tast`] while preserving spans for
//! diagnostics and recording semantic types from [`crate::common::types`].
//! Hosted and bare-metal targets share parsing but use different type-checking
//! entry points so runtime intrinsics and ownership restrictions stay explicit.
//!
//! # Errors
//!
//! Lexer and parser errors are collected by the pipeline as textual diagnostics.
//! Type checking returns structured [`typechecker::TypeError`] values that can
//! be rendered with source context by [`typechecker::report_type_error`].
//!
//! # Related Modules
//!
//! - [`lexer`] tokenizes source text.
//! - [`parser`] builds the AST.
//! - [`typechecker`] performs semantic analysis and refinement proof checks.
//! - [`crate::pipeline`] wires the frontend to backend lowering.

pub mod lexer;
pub mod parser;
pub mod typechecker;

#[cfg(test)]
mod tests;
