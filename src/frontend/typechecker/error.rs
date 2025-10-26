use crate::common::span::Span;
use crate::common::types::{IProposition, IType};
use std::fmt;

#[derive(Clone, Debug)]
pub enum TypeError<'src> {
    TypeMismatch {
        expected: IType<'src>,
        found: IType<'src>,
        span: Span,
    },

    // A refinement predicate could not be proven
    Unprovable {
        proposition: IProposition<'src>,
        context: String,
        span: Span,
    },

    UndefinedVariable { name: String, span: Span },

    UndefinedFunction { name: String, span: Span },

    InvalidArrayAccess {
        array_type: IType<'src>,
        index_expr: String, 
        reason: String,     
        span: Span,
    },

    InvalidAssignment {
        variable: String,
        reason: String, 
        span: Span,
    },

    // Assignment to mutable variable violates master type constraint
    MasterTypeMismatch {
        variable: String,
        master_type: IType<'src>,
        assigned_type: IType<'src>,
        span: Span,
    },

    PreconditionViolation {
        function: String,
        precondition: IProposition<'src>,
        span: Span,
    },

    ArityMismatch {
        function: String,
        expected: usize,
        found: usize,
        span: Span,
    },

    InvalidArraySize { size: String, span: Span },

    AssignToImmutable { variable: String, span: Span },

    ReturnTypeMismatch {
        expected: IType<'src>,
        found: IType<'src>,
        span: Span,
    },

    InvalidOperation {
        operation: String,
        operand_types: Vec<IType<'src>>,
        span: Span,
    },
}

impl<'src> fmt::Display for TypeError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::TypeMismatch {
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "Type mismatch at {:?}: expected `{}`, found `{}`",
                    span, expected, found
                )
            }

            TypeError::Unprovable {
                proposition,
                context,
                span,
            } => {
                write!(
                    f,
                    "Could not prove refinement at {:?}: {}\n  Required: {}",
                    span, context, proposition
                )
            }

            TypeError::UndefinedVariable { name, span } => {
                write!(f, "Undefined variable `{}` at {:?}", name, span)
            }

            TypeError::UndefinedFunction { name, span } => {
                write!(f, "Undefined function `{}` at {:?}", name, span)
            }

            TypeError::InvalidArrayAccess {
                array_type,
                index_expr,
                reason,
                span,
            } => {
                write!(
                    f,
                    "Invalid array access at {:?}: {}\n  Array type: {}\n  Index: {}",
                    span, reason, array_type, index_expr
                )
            }

            TypeError::InvalidAssignment {
                variable,
                reason,
                span,
            } => {
                write!(
                    f,
                    "Invalid assignment to `{}` at {:?}: {}",
                    variable, span, reason
                )
            }

            TypeError::MasterTypeMismatch {
                variable,
                master_type,
                assigned_type,
                span,
            } => {
                write!(
                    f,
                    "Assignment to `{}` at {:?} violates master type constraint\n  Master type: {}\n  Assigned type: {}",
                    variable, span, master_type, assigned_type
                )
            }

            TypeError::PreconditionViolation {
                function,
                precondition,
                span,
            } => {
                write!(
                    f,
                    "Precondition not satisfied for function `{}` at {:?}\n  Required: {}",
                    function, span, precondition
                )
            }

            TypeError::ArityMismatch {
                function,
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "Function `{}` at {:?} expects {} argument(s), but {} were provided",
                    function, span, expected, found
                )
            }

            TypeError::InvalidArraySize { size, span } => {
                write!(
                    f,
                    "Invalid array size at {:?}: `{}` must be a positive integer",
                    span, size
                )
            }

            TypeError::AssignToImmutable { variable, span } => {
                write!(
                    f,
                    "Cannot assign to immutable variable `{}` at {:?}",
                    variable, span
                )
            }

            TypeError::ReturnTypeMismatch {
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "Return type mismatch at {:?}: expected `{}`, found `{}`",
                    span, expected, found
                )
            }

            TypeError::InvalidOperation {
                operation,
                operand_types,
                span,
            } => {
                let types_str = operand_types
                    .iter()
                    .map(|t| format!("`{}`", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "Invalid operation `{}` at {:?} for types: {}",
                    operation, span, types_str
                )
            }
        }
    }
}

impl<'src> std::error::Error for TypeError<'src> {}
