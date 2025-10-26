// src/frontend/typechecker/mod.rs

pub mod context;
pub mod error;

// Re-export commonly used types
pub use context::{MutableBinding, TypingContext, VarBinding};
pub use error::TypeError;
