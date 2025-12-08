pub mod expr;
pub mod types;
pub mod stmt;
pub mod program;

// Re-exports for tests
#[cfg(test)]
pub use expr::{expr_parser, expr_parser_for_types};
#[cfg(test)]
pub use types::type_parser;
#[cfg(test)]
pub use program::function_parser;

// Main entry point for parsing
pub use program::program_parser;
