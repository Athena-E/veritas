//! Register Allocation
//!
//! This module provides register allocation for mapping DTAL virtual registers
//! to x86-64 physical registers.
//!
//! # Pipeline
//!
//! ```text
//! DTAL (virtual regs) → Liveness Analysis → Register Allocation → DTAL (physical regs)
//! ```

pub mod allocator;
pub mod liveness;

pub use allocator::{AllocationResult, GraphColoringAllocator, LinearScanAllocator};
pub use liveness::{InterferenceGraph, LivenessAnalysis, LivenessInfo};
