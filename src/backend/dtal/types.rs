//! First-order DTAL types (lifetime-free)
//!
//! These types mirror `IType<'src>` but use `Constraint` instead of
//! `IProposition<'src>`, making DTAL programs self-contained and
//! independent of the source AST.
//!
//! Singleton types use `IndexExpr` for symbolic index tracking:
//! `SingletonInt(IndexExpr::Const(5))` for concrete values,
//! `SingletonInt(IndexExpr::Var("n"))` for symbolic values,
//! `SingletonInt(IndexExpr::Add(..))` for derived values.

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use std::fmt;
use std::sync::Arc;

/// A first-order DTAL type (no `'src` lifetime)
///
/// The key difference from `IType<'src>`: `RefinedInt` uses `Constraint`
/// (first-order, index domain) instead of `IProposition<'src>` (full source AST).
///
/// `SingletonInt` and `Array` use `IndexExpr` for symbolic value tracking,
/// enabling Xi & Harper's derivation-based typing rules.
#[derive(Clone, Debug)]
pub enum DtalType {
    Unit,
    Int,
    I64,
    Bool,
    Array {
        element_type: Arc<Self>,
        size: IndexExpr,
    },
    Ref(Arc<Self>),
    RefMut(Arc<Self>),
    SingletonInt(IndexExpr),
    RefinedInt {
        base: Arc<Self>,
        var: String,
        constraint: Constraint,
    },
    Master(Arc<Self>),
    /// ∃witness_var. int(witness_var) where constraint
    /// The constraint may reference witness_var.
    ExistentialInt {
        witness_var: String,
        constraint: Constraint,
    },
}

impl PartialEq for DtalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DtalType::Unit, DtalType::Unit) => true,
            (DtalType::Int, DtalType::Int) => true,
            (DtalType::I64, DtalType::I64) => true,
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
            (
                DtalType::ExistentialInt {
                    witness_var: w1,
                    constraint: c1,
                },
                DtalType::ExistentialInt {
                    witness_var: w2,
                    constraint: c2,
                },
            ) => w1 == w2 && c1 == c2,
            _ => false,
        }
    }
}

impl std::hash::Hash for DtalType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            DtalType::Unit | DtalType::Int | DtalType::I64 | DtalType::Bool => {}
            DtalType::Array { element_type, size } => {
                element_type.hash(state);
                format!("{}", size).hash(state);
            }
            DtalType::Ref(inner) | DtalType::RefMut(inner) | DtalType::Master(inner) => {
                inner.hash(state);
            }
            DtalType::SingletonInt(idx) => {
                format!("{}", idx).hash(state);
            }
            DtalType::RefinedInt {
                base,
                var,
                constraint,
            } => {
                base.hash(state);
                var.hash(state);
                format!("{}", constraint).hash(state);
            }
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            } => {
                witness_var.hash(state);
                format!("{}", constraint).hash(state);
            }
        }
    }
}

impl Eq for DtalType {}

impl fmt::Display for DtalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DtalType::Unit => write!(f, "unit"),
            DtalType::Int => write!(f, "int"),
            DtalType::I64 => write!(f, "i64"),
            DtalType::Bool => write!(f, "bool"),
            DtalType::Array { element_type, size } => {
                write!(f, "[{}; {}]", element_type, size)
            }
            DtalType::Ref(inner) => write!(f, "&{}", inner),
            DtalType::RefMut(inner) => write!(f, "&mut {}", inner),
            DtalType::SingletonInt(idx) => write!(f, "int({})", idx),
            DtalType::RefinedInt {
                base,
                var,
                constraint,
            } => {
                let base_name = if matches!(base.as_ref(), DtalType::I64) {
                    "i64"
                } else {
                    "int"
                };
                write!(f, "{{{}: {} | {} }}", var, base_name, constraint)
            }
            DtalType::Master(inner) => write!(f, "master({})", inner),
            DtalType::ExistentialInt {
                witness_var,
                constraint,
            } => {
                write!(
                    f,
                    "exists {}. int({}) where {}",
                    witness_var, witness_var, constraint
                )
            }
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
            IType::I64 => DtalType::I64,
            IType::Bool => DtalType::Bool,
            IType::Array { element_type, size } => DtalType::Array {
                element_type: Arc::new(DtalType::from_itype(element_type)),
                size: Self::ivalue_to_index_expr(size),
            },
            IType::Ref(inner) => DtalType::Ref(Arc::new(DtalType::from_itype(inner))),
            IType::RefMut(inner) => DtalType::RefMut(Arc::new(DtalType::from_itype(inner))),
            IType::SingletonInt(val) => DtalType::SingletonInt(Self::ivalue_to_index_expr(val)),
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

    /// Convert an `IValue` to an `IndexExpr`.
    fn ivalue_to_index_expr(val: &crate::common::types::IValue) -> IndexExpr {
        use crate::common::types::IValue;
        match val {
            IValue::Int(n) => IndexExpr::Const(*n),
            IValue::Bool(b) => IndexExpr::Const(if *b { 1 } else { 0 }),
            IValue::Symbolic(s) => IndexExpr::Var(s.clone()),
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
        let ty = DtalType::SingletonInt(IndexExpr::Const(42));
        assert_eq!(ty.to_string(), "int(42)");
    }

    #[test]
    fn test_display_singleton_symbolic() {
        let ty = DtalType::SingletonInt(IndexExpr::Var("n".to_string()));
        assert_eq!(ty.to_string(), "int(n)");
    }

    #[test]
    fn test_display_singleton_compound() {
        let ty = DtalType::SingletonInt(IndexExpr::Add(
            Box::new(IndexExpr::Var("n".to_string())),
            Box::new(IndexExpr::Const(1)),
        ));
        assert_eq!(ty.to_string(), "int((n + 1))");
    }

    #[test]
    fn test_display_array() {
        let ty = DtalType::Array {
            element_type: Arc::new(DtalType::Int),
            size: IndexExpr::Const(10),
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
        assert_eq!(dtal_type, DtalType::SingletonInt(IndexExpr::Const(5)));
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
                size: IndexExpr::Const(10),
            }
        );
    }

    #[test]
    fn test_equality() {
        assert_eq!(DtalType::Int, DtalType::Int);
        assert_ne!(DtalType::Int, DtalType::Bool);
        assert_eq!(
            DtalType::SingletonInt(IndexExpr::Const(5)),
            DtalType::SingletonInt(IndexExpr::Const(5))
        );
        assert_ne!(
            DtalType::SingletonInt(IndexExpr::Const(5)),
            DtalType::SingletonInt(IndexExpr::Const(6))
        );
    }

    #[test]
    fn test_parse_roundtrip_symbolic() {
        let ty = DtalType::SingletonInt(IndexExpr::Var("n".to_string()));
        let s = ty.to_string();
        assert_eq!(s, "int(n)");
    }
}
