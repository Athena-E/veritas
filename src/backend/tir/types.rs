//! Core TIR support types.
//!
//! This module contains block identifiers, block-local type state, and the
//! operation enums shared by TIR instructions and lowering helpers.
//!
//! # Example
//!
//! ```text
//! bb0:
//!   state = { v0: int, constraints: [v0 >= 0] }
//! ```
//!
//! # Design Notes
//!
//! [`RegisterState`] is a lightweight snapshot used while constructing and
//! lowering TIR. It records facts that should become DTAL annotations; it is
//! not a substitute for verifier state in [`crate::verifier`].

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::common::types::IType;
use std::collections::HashMap;
use std::fmt;

/// Unique basic block identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// Allocator for block IDs.
#[derive(Debug, Default)]
pub struct BlockIdAllocator {
    next_id: u32,
}

impl BlockIdAllocator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn fresh(&mut self) -> BlockId {
        let id = self.next_id;
        self.next_id += 1;
        BlockId(id)
    }
}

/// Type and constraint state at a point in the TIR CFG.
#[derive(Clone, Debug, Default)]
pub struct RegisterState<'src> {
    pub registers: HashMap<VirtualReg, IType<'src>>,
    pub constraints: Vec<Constraint>,
}

impl<'src> RegisterState<'src> {
    pub fn new() -> Self {
        Self {
            registers: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    pub fn insert(&mut self, reg: VirtualReg, ty: IType<'src>) {
        self.registers.insert(reg, ty);
    }

    pub fn get(&self, reg: &VirtualReg) -> Option<&IType<'src>> {
        self.registers.get(reg)
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
}

/// Binary operations in TIR.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Ne => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}

/// Unary operations in TIR.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Neg => write!(f, "-"),
        }
    }
}
