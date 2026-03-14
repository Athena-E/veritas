//! Verification error types
//!
//! This module defines error types for the DTAL verifier with
//! detailed diagnostics for debugging verification failures.

use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
use std::fmt;

/// Verification error
#[derive(Debug)]
pub enum VerifyError {
    /// Type annotation doesn't match actual value
    TypeMismatch {
        block: String,
        instr_desc: String,
        expected: DtalType,
        actual: DtalType,
    },

    /// Register used before definition
    UndefinedRegister { reg: Reg, block: String },

    /// Constraint cannot be proven from context
    UnprovableConstraint {
        constraint: Constraint,
        context: Vec<Constraint>,
        block: String,
    },

    /// Type states incompatible at join point
    JoinMismatch {
        block: String,
        reg: Reg,
        expected: DtalType,
        actual: DtalType,
        from_block: String,
    },

    /// Singleton type value mismatch
    SingletonMismatch {
        block: String,
        expected_value: i64,
        actual_value: i64,
    },

    /// Binary operation type error
    BinOpTypeMismatch {
        block: String,
        op: String,
        lhs_type: DtalType,
        rhs_type: DtalType,
    },

    /// Return type doesn't match function signature
    ReturnTypeMismatch {
        function: String,
        expected: DtalType,
        actual: DtalType,
    },

    /// Function not found
    UnknownFunction { name: String },

    /// Block not found
    UnknownBlock { label: String },

    /// Bounds check failed for memory access
    BoundsCheckFailed {
        block: String,
        instr_desc: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    /// Postcondition not provable at return
    PostconditionFailed {
        function: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    /// Precondition not provable at call site
    PreconditionFailed {
        block: String,
        callee: String,
        constraint: Constraint,
        context: Vec<Constraint>,
    },

    /// Internal error (should not happen)
    InternalError { msg: String },
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
            VerifyError::InternalError { msg } => {
                write!(f, "Internal verifier error: {}", msg)
            }
        }
    }
}

impl std::error::Error for VerifyError {}
