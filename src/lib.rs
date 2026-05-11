//! Veritas compiler library.
//!
//! Veritas compiles a small, typed source language with refinement types,
//! ownership-aware arrays and references, contracts, and verifier-checked DTAL
//! output.
//!
//! # Architecture
//!
//! ```text
//! source text
//!   -> frontend: tokens, AST, type checking
//!   -> common: shared AST, TAST, types, spans, ownership metadata
//!   -> backend: TIR, DTAL, optimisation, allocation, x86-64 encoding
//!   -> verifier: independent DTAL checking
//! ```
//!
//! # Design Notes
//!
//! The compiler keeps trust boundaries explicit. Frontend type checking records
//! source-level facts, the backend lowers those facts into verifier-visible
//! DTAL annotations, and [`verifier`] independently checks the DTAL before
//! native code generation.
//!
//! # Public Entry Points
//!
//! - [`pipeline`] provides the end-to-end compile APIs.
//! - [`frontend`] exposes lexing, parsing, and type checking components.
//! - [`backend`] exposes IR and code-generation stages.
//! - [`verifier`] validates emitted DTAL programs.

#![allow(clippy::result_large_err)]

pub mod backend;
pub mod common;
pub mod frontend;
pub mod pipeline;
pub mod verifier;
