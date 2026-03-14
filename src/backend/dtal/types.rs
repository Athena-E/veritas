//! First-order DTAL types (lifetime-free)
//!
//! These types mirror `IType<'src>` but use `Constraint` instead of
//! `IProposition<'src>`, making DTAL programs self-contained and
//! independent of the source AST.

use crate::backend::dtal::constraints::Constraint;
use std::fmt;
use std::sync::Arc;

/// A compile-time value in the DTAL domain (lifetime-free mirror of `IValue`)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DtalValue {
    Int(i64),
    Bool(bool),
    Symbolic(String),
}

impl fmt::Display for DtalValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DtalValue::Int(n) => write!(f, "{}", n),
            DtalValue::Bool(b) => write!(f, "{}", b),
            DtalValue::Symbolic(s) => write!(f, "{}", s),
        }
    }
}

/// A first-order DTAL type (no `'src` lifetime)
///
/// The key difference from `IType<'src>`: `RefinedInt` uses `Constraint`
/// (first-order, index domain) instead of `IProposition<'src>` (full source AST).
#[derive(Clone, Debug)]
pub enum DtalType {
    Unit,
    Int,
    Bool,
    Array {
        element_type: Arc<Self>,
        size: DtalValue,
    },
    Ref(Arc<Self>),
    RefMut(Arc<Self>),
    SingletonInt(DtalValue),
    RefinedInt {
        base: Arc<Self>,
        var: String,
        constraint: Constraint,
    },
    Master(Arc<Self>),
}

impl PartialEq for DtalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DtalType::Unit, DtalType::Unit) => true,
            (DtalType::Int, DtalType::Int) => true,
            (DtalType::Bool, DtalType::Bool) => true,
            (
                DtalType::Array {
                    element_type: e1,
                    size: s1,
                },
                DtalType::Array {
                    element_type: e2,
                    size: s2,
                },
            ) => e1 == e2 && s1 == s2,
            (DtalType::Ref(a), DtalType::Ref(b)) => a == b,
            (DtalType::RefMut(a), DtalType::RefMut(b)) => a == b,
            (DtalType::SingletonInt(a), DtalType::SingletonInt(b)) => a == b,
            (
                DtalType::RefinedInt {
                    base: b1,
                    var: v1,
                    constraint: c1,
                },
                DtalType::RefinedInt {
                    base: b2,
                    var: v2,
                    constraint: c2,
                },
            ) => b1 == b2 && v1 == v2 && c1 == c2,
            (DtalType::Master(a), DtalType::Master(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for DtalType {}

impl fmt::Display for DtalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DtalType::Unit => write!(f, "unit"),
            DtalType::Int => write!(f, "int"),
            DtalType::Bool => write!(f, "bool"),
            DtalType::Array { element_type, size } => {
                write!(f, "[{}; {}]", element_type, size)
            }
            DtalType::Ref(inner) => write!(f, "&{}", inner),
            DtalType::RefMut(inner) => write!(f, "&mut {}", inner),
            DtalType::SingletonInt(val) => write!(f, "int({})", val),
            DtalType::RefinedInt {
                base: _,
                var,
                constraint,
            } => {
                write!(f, "{{{}: int | {} }}", var, constraint)
            }
            DtalType::Master(inner) => write!(f, "master({})", inner),
        }
    }
}

impl DtalType {
    /// Convert an `IType<'src>` to a `DtalType`.
    ///
    /// For `RefinedInt`, attempts to convert the embedded `IProposition`
    /// to a `Constraint` via `expr_to_constraint`. If conversion fails,
    /// the type is widened to its base type (safe, lossy).
    pub fn from_itype(ty: &crate::common::types::IType<'_>) -> Self {
        use crate::common::types::IType;

        match ty {
            IType::Unit => DtalType::Unit,
            IType::Int => DtalType::Int,
            IType::Bool => DtalType::Bool,
            IType::Array { element_type, size } => DtalType::Array {
                element_type: Arc::new(DtalType::from_itype(element_type)),
                size: DtalValue::from_ivalue(size),
            },
            IType::Ref(inner) => DtalType::Ref(Arc::new(DtalType::from_itype(inner))),
            IType::RefMut(inner) => DtalType::RefMut(Arc::new(DtalType::from_itype(inner))),
            IType::SingletonInt(val) => DtalType::SingletonInt(DtalValue::from_ivalue(val)),
            IType::RefinedInt { base, prop } => {
                use crate::backend::dtal::convert::expr_to_constraint;
                match expr_to_constraint(&prop.predicate.0) {
                    Some(constraint) => DtalType::RefinedInt {
                        base: Arc::new(DtalType::from_itype(base)),
                        var: prop.var.clone(),
                        constraint,
                    },
                    None => {
                        // Lossy widening: drop the refinement, keep the base type
                        DtalType::from_itype(base)
                    }
                }
            }
            IType::Master(inner) => DtalType::Master(Arc::new(DtalType::from_itype(inner))),
        }
    }
}

impl DtalValue {
    /// Convert an `IValue` to a `DtalValue`.
    pub fn from_ivalue(val: &crate::common::types::IValue) -> Self {
        use crate::common::types::IValue;
        match val {
            IValue::Int(n) => DtalValue::Int(*n),
            IValue::Bool(b) => DtalValue::Bool(*b),
            IValue::Symbolic(s) => DtalValue::Symbolic(s.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::{IType, IValue};

    #[test]
    fn test_display_basic_types() {
        assert_eq!(DtalType::Unit.to_string(), "unit");
        assert_eq!(DtalType::Int.to_string(), "int");
        assert_eq!(DtalType::Bool.to_string(), "bool");
    }

    #[test]
    fn test_display_singleton() {
        let ty = DtalType::SingletonInt(DtalValue::Int(42));
        assert_eq!(ty.to_string(), "int(42)");
    }

    #[test]
    fn test_display_array() {
        let ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: DtalValue::Int(10),
        };
        assert_eq!(ty.to_string(), "[int; 10]");
    }

    #[test]
    fn test_display_ref() {
        let ty = DtalType::Ref(Arc::new(DtalType::Int));
        assert_eq!(ty.to_string(), "&int");
    }

    #[test]
    fn test_display_ref_mut() {
        let ty = DtalType::RefMut(Arc::new(DtalType::Int));
        assert_eq!(ty.to_string(), "&mut int");
    }

    #[test]
    fn test_display_master() {
        let ty = DtalType::Master(Arc::new(DtalType::Int));
        assert_eq!(ty.to_string(), "master(int)");
    }

    #[test]
    fn test_display_refined() {
        use crate::backend::dtal::constraints::{Constraint, IndexExpr};
        let ty = DtalType::RefinedInt {
            base: Arc::new(DtalType::Int),
            var: "x".to_string(),
            constraint: Constraint::Gt(IndexExpr::Var("x".to_string()), IndexExpr::Const(0)),
        };
        assert_eq!(ty.to_string(), "{x: int | x > 0 }");
    }

    #[test]
    fn test_from_itype_basic() {
        assert_eq!(DtalType::from_itype(&IType::Unit), DtalType::Unit);
        assert_eq!(DtalType::from_itype(&IType::Int), DtalType::Int);
        assert_eq!(DtalType::from_itype(&IType::Bool), DtalType::Bool);
    }

    #[test]
    fn test_from_itype_singleton() {
        let itype = IType::SingletonInt(IValue::Int(5));
        let dtal_type = DtalType::from_itype(&itype);
        assert_eq!(dtal_type, DtalType::SingletonInt(DtalValue::Int(5)));
    }

    #[test]
    fn test_from_itype_array() {
        let itype = IType::Array {
            element_type: Arc::new(IType::Int),
            size: IValue::Int(10),
        };
        let dtal_type = DtalType::from_itype(&itype);
        assert_eq!(
            dtal_type,
            DtalType::Array {
                element_type: Arc::new(DtalType::Int),
                size: DtalValue::Int(10),
            }
        );
    }

    #[test]
    fn test_equality() {
        assert_eq!(DtalType::Int, DtalType::Int);
        assert_ne!(DtalType::Int, DtalType::Bool);
        assert_eq!(
            DtalType::SingletonInt(DtalValue::Int(5)),
            DtalType::SingletonInt(DtalValue::Int(5))
        );
        assert_ne!(
            DtalType::SingletonInt(DtalValue::Int(5)),
            DtalType::SingletonInt(DtalValue::Int(6))
        );
    }
}
