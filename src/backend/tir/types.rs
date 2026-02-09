//! Core types for TIR
//!
//! This module defines basic types used throughout the TIR representation.

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::common::types::IType;
use std::collections::HashMap;
use std::fmt;

/// Unique identifier for a basic block
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// Allocator for block IDs
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

/// Register state: mapping from virtual registers to types
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

/// Binary operations in TIR
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
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

/// Unary operations in TIR
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

/// Justification for a bounds proof
#[derive(Clone, Debug)]
pub enum ProofJustification {
    /// Proven by frontend type checker
    FromFrontend,
    /// From loop invariant
    LoopInvariant(String),
    /// From branch condition
    BranchCondition,
    /// From function precondition
    Precondition,
}

/// Proof that an array access is in bounds
#[derive(Clone, Debug)]
pub struct BoundsProof {
    /// The constraint that was proven: 0 <= index < size
    pub constraint: Constraint,
    /// How it was proven
    pub justification: ProofJustification,
}
