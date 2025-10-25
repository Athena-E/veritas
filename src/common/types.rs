use crate::common::ast::Expr;
use crate::common::span::{Span, Spanned};
use std::fmt;
use std::sync::Arc; 

// Semantic types of compile-time values
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IValue {
    Int(i64),
    Bool(bool),
    Symbolic(String),
}

impl fmt::Display for IValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IValue::Int(n) => write!(f, "{}", n),
            IValue::Bool(b) => write!(f, "{}", b),
            IValue::Symbolic(s) => write!(f, "{}", s),
        }
    }
}

// Semantic types of propositions
#[derive(Clone, Debug)]
pub struct IProposition<'src> {
    pub var: String, 
    pub predicate: Arc<Spanned<Expr<'src>>>, 
}

// Internal, semantic representation of a type
#[derive(Clone, Debug)]
pub enum IType<'src> {
    Unit,
    Int,
    Bool,
    Array {
        element_type: Arc<Self>,
        size: IValue, 
    },
    Ref(Arc<Self>),
    RefMut(Arc<Self>),
    SingletonInt(IValue),
    RefinedInt {
        base: Arc<Self>,
        prop: IProposition<'src>,
    },
    // Master/most general type for mutable variables
    Master(Arc<Self>),
}

/// Internal representation of a function's signature
/// To be stored in the global context (Σ_F)
#[derive(Clone, Debug)]
pub struct FunctionSignature<'src> {
    pub name: String,
    pub parameters: Vec<(String, IType<'src>)>,
    pub return_type: IType<'src>,
    pub precondition: Option<IProposition<'src>>,
    pub span: Span,
}

// Helper function to display expressions (for propositions and refinements)
fn fmt_expr(expr: &Expr) -> String {
    use crate::common::ast::{BinOp, Literal, UnaryOp};

    match expr {
        Expr::Error => "<error>".to_string(),
        Expr::Literal(Literal::Int(n)) => n.to_string(),
        Expr::Literal(Literal::Bool(b)) => b.to_string(),
        Expr::Variable(v) => v.to_string(),
        Expr::BinOp { op, lhs, rhs } => {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Eq => "==",
                BinOp::NotEq => "!=",
                BinOp::Lt => "<",
                BinOp::Lte => "<=",
                BinOp::Gt => ">",
                BinOp::Gte => ">=",
                BinOp::And => "&&",
                BinOp::Or => "||",
            };
            format!("{} {} {}", fmt_expr(&lhs.0), op_str, fmt_expr(&rhs.0))
        }
        Expr::UnaryOp { op, cond } => {
            let op_str = match op {
                UnaryOp::Not => "!",
            };
            format!("{}{}", op_str, fmt_expr(&cond.0))
        }
        Expr::Call { func_name, args } => {
            let args_str = args
                .0
                .iter()
                .map(|arg| fmt_expr(&arg.0))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", func_name, args_str)
        }
        Expr::Index { base, index } => {
            format!("{}[{}]", fmt_expr(&base.0), fmt_expr(&index.0))
        }
        Expr::ArrayInit { value, length } => {
            format!("[{}; {}]", fmt_expr(&value.0), fmt_expr(&length.0))
        }
        Expr::If { .. } => "<if-expr>".to_string(), // Simplified for type display
    }
}

impl<'src> fmt::Display for IProposition<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", fmt_expr(&self.predicate.0))
    }
}

impl<'src> fmt::Display for IType<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IType::Unit => write!(f, "()"),
            IType::Int => write!(f, "int"),
            IType::Bool => write!(f, "bool"),
            IType::Array { element_type, size } => {
                write!(f, "[{}; {}]", element_type, size)
            }
            IType::Ref(inner) => write!(f, "&{}", inner),
            IType::RefMut(inner) => write!(f, "&mut {}", inner),
            IType::SingletonInt(value) => write!(f, "int({})", value),
            IType::RefinedInt { base, prop } => {
                write!(f, "{{{}: {} | {}}}", prop.var, base, prop)
            }
            IType::Master(inner) => write!(f, "master({})", inner),
        }
    }
}

impl<'src> fmt::Display for FunctionSignature<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params_str = self
            .parameters
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "fn {}({}) -> {}", self.name, params_str, self.return_type)?;

        if let Some(ref precond) = self.precondition {
            write!(f, " requires {}", precond)?;
        }

        Ok(())
    }
}