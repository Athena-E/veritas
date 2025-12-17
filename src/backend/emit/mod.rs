//! DTAL Text Emitter
//!
//! This module emits DTAL programs as text files with type annotations.
//! The format is human-readable and includes type state information
//! for verification purposes.

mod emitter;

pub use emitter::emit_program;
