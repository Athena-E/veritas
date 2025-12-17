//! Constraint domain for DTAL
//!
//! This module defines constraints and index expressions used for
//! refinement types and bounds proofs.

use std::fmt;

/// A constraint in the index domain
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constraint {
    True,
    False,
    Eq(IndexExpr, IndexExpr),
    Lt(IndexExpr, IndexExpr),
    Le(IndexExpr, IndexExpr),
    Gt(IndexExpr, IndexExpr),
    Ge(IndexExpr, IndexExpr),
    Ne(IndexExpr, IndexExpr),
    And(Box<Constraint>, Box<Constraint>),
    Or(Box<Constraint>, Box<Constraint>),
    Not(Box<Constraint>),
}

/// An expression in the index domain
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IndexExpr {
    Const(i64),
    Var(String),
    Add(Box<IndexExpr>, Box<IndexExpr>),
    Sub(Box<IndexExpr>, Box<IndexExpr>),
    Mul(Box<IndexExpr>, Box<IndexExpr>),
}

impl fmt::Display for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IndexExpr::Const(n) => write!(f, "{}", n),
            IndexExpr::Var(s) => write!(f, "{}", s),
            IndexExpr::Add(l, r) => write!(f, "({} + {})", l, r),
            IndexExpr::Sub(l, r) => write!(f, "({} - {})", l, r),
            IndexExpr::Mul(l, r) => write!(f, "({} * {})", l, r),
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constraint::True => write!(f, "true"),
            Constraint::False => write!(f, "false"),
            Constraint::Eq(l, r) => write!(f, "{} == {}", l, r),
            Constraint::Lt(l, r) => write!(f, "{} < {}", l, r),
            Constraint::Le(l, r) => write!(f, "{} <= {}", l, r),
            Constraint::Gt(l, r) => write!(f, "{} > {}", l, r),
            Constraint::Ge(l, r) => write!(f, "{} >= {}", l, r),
            Constraint::Ne(l, r) => write!(f, "{} != {}", l, r),
            Constraint::And(l, r) => write!(f, "({} && {})", l, r),
            Constraint::Or(l, r) => write!(f, "({} || {})", l, r),
            Constraint::Not(c) => write!(f, "!{}", c),
        }
    }
}
