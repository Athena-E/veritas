// src/frontend/typechecker/mod.rs

pub mod context;
pub mod error;
pub mod smt;

// Re-export commonly used types
pub use context::{MutableBinding, TypingContext, VarBinding};
pub use error::TypeError;
pub use smt::check_provable;
