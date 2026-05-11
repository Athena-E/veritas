//! Verification errors reported by the DTAL verifier.
//!
//! # Error Categories
//!
//! ```text
//! VerifyError
//!   |- typing and singleton mismatches
//!   |- ownership and consumed-register violations
//!   |- CFG join failures
//!   |- bounds, precondition, and postcondition failures
//!   `- internal invariants
//! ```
//!
//! # Design Notes
//!
//! The enum carries structured context instead of only formatted strings so
//! callers can choose between human-readable reports and tooling-friendly
//! diagnostics. Display formatting is intentionally concise and includes the
//! current constraint context for proof failures.
//!
//! # Related Modules
//!
//! The verifier checker and dataflow modules produce these errors.
//! [`crate::verifier::smt`] is the source of most proof-failure context.

use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
use std::fmt;

/// Error emitted while verifying a DTAL program.
///
/// Variants are grouped by the invariant that failed: type derivation,
/// ownership, control-flow joins, contract obligations, constraint proofs, or
/// verifier-internal assumptions.
#[derive(Debug)]
pub enum VerifyError {
    TypeMismatch {
        block: String,
        instr_desc: String,
        expected: DtalType,
        actual: DtalType,
    },

    UndefinedRegister {
        reg: Reg,
        block: String,
    },

    ConsumedRegister {
        reg: Reg,
        block: String,
    },

    UnprovableConstraint {
        constraint: Constraint,
        context: Vec<Constraint>,
        block: String,
    },

    JoinMismatch {
        block: String,
        reg: Reg,
        expected: DtalType,
        actual: DtalType,
        from_block: String,
    },

    SingletonMismatch {
        block: String,
        expected_value: i128,
        actual_value: i128,
    },

    BinOpTypeMismatch {
        block: String,
        op: String,
        lhs_type: DtalType,
        rhs_type: DtalType,
    },

    ReturnTypeMismatch {
        function: String,
        expected: DtalType,
        actual: DtalType,
    },

    UnknownFunction {
        name: String,
    },

    UnknownBlock {
        label: String,
    },

    BoundsCheckFailed {
        block: String,
        instr_desc: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    PostconditionFailed {
        function: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    PreconditionFailed {
        block: String,
        callee: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    ArithmeticOverflow {
        block: String,
        op: String,
        context: Vec<Constraint>,
    },

    // Indicates an invariant violation inside the verifier.
    InternalError {
        msg: String,
    },

    OwnershipViolation {
        block: String,
        instr_desc: String,
        msg: String,
    },
}

impl fmt::Display for VerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VerifyError::TypeMismatch {
                block,
                instr_desc,
                expected,
                actual,
            } => {
                write!(
                    f,
                    "Type mismatch in block '{}' at '{}': expected {}, found {}",
                    block, instr_desc, expected, actual
                )
            }
            VerifyError::UndefinedRegister { reg, block } => {
                write!(
                    f,
                    "Register {:?} used before definition in block '{}'",
                    reg, block
                )
            }
            VerifyError::ConsumedRegister { reg, block } => {
                write!(
                    f,
                    "Register {:?} used after ownership was consumed in block '{}'",
                    reg, block
                )
            }
            VerifyError::UnprovableConstraint {
                constraint,
                context,
                block,
            } => {
                write!(
                    f,
                    "Cannot prove constraint '{}' in block '{}'\nContext: {:?}",
                    constraint, block, context
                )
            }
            VerifyError::JoinMismatch {
                block,
                reg,
                expected,
                actual,
                from_block,
            } => {
                write!(
                    f,
                    "Join mismatch at block '{}': register {:?} has type {} from '{}', but expected {}",
                    block, reg, actual, from_block, expected
                )
            }
            VerifyError::SingletonMismatch {
                block,
                expected_value,
                actual_value,
            } => {
                write!(
                    f,
                    "Singleton type mismatch in block '{}': expected int({}), found int({})",
                    block, expected_value, actual_value
                )
            }
            VerifyError::BinOpTypeMismatch {
                block,
                op,
                lhs_type,
                rhs_type,
            } => {
                write!(
                    f,
                    "Binary operation '{}' type mismatch in block '{}': lhs is {}, rhs is {}",
                    op, block, lhs_type, rhs_type
                )
            }
            VerifyError::ReturnTypeMismatch {
                function,
                expected,
                actual,
            } => {
                write!(
                    f,
                    "Return type mismatch in function '{}': expected {}, found {}",
                    function, expected, actual
                )
            }
            VerifyError::UnknownFunction { name } => {
                write!(f, "Unknown function '{}'", name)
            }
            VerifyError::UnknownBlock { label } => {
                write!(f, "Unknown block '{}'", label)
            }
            VerifyError::BoundsCheckFailed {
                block,
                instr_desc,
                constraint,
                context,
            } => {
                write!(
                    f,
                    "Bounds check failed in block '{}' at '{}': cannot prove '{}'\nContext: {:?}",
                    block, instr_desc, constraint, context
                )
            }
            VerifyError::PostconditionFailed {
                function,
                constraint,
                context,
            } => {
                write!(
                    f,
                    "Postcondition not provable in function '{}': cannot prove '{}'\nContext: {:?}",
                    function, constraint, context
                )
            }
            VerifyError::PreconditionFailed {
                block,
                callee,
                constraint,
                context,
            } => {
                write!(
                    f,
                    "Precondition not provable at call to '{}' in block '{}': cannot prove '{}'\nContext: {:?}",
                    callee, block, constraint, context
                )
            }
            VerifyError::ArithmeticOverflow { block, op, context } => {
                write!(
                    f,
                    "Arithmetic overflow: i64 operation '{}' in block '{}' may exceed 64-bit signed range\nContext: {:?}",
                    op, block, context
                )
            }
            VerifyError::InternalError { msg } => {
                write!(f, "Internal verifier error: {}", msg)
            }
            VerifyError::OwnershipViolation {
                block,
                instr_desc,
                msg,
            } => {
                write!(
                    f,
                    "Ownership violation in block '{}' at '{}': {}",
                    block, instr_desc, msg
                )
            }
        }
    }
}

impl std::error::Error for VerifyError {}
