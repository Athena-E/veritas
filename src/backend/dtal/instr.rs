//! DTAL instructions and program structures
//!
//! This module defines the DTAL instruction set and program representation.

use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::regs::Reg;
use crate::common::types::IType;
use std::collections::HashMap;
use std::fmt;

/// Type state at a program point
#[derive(Clone, Debug)]
pub struct TypeState<'src> {
    /// Type of each register
    pub register_types: HashMap<Reg, IType<'src>>,
    /// Active constraints
    pub constraints: Vec<Constraint>,
}

impl<'src> TypeState<'src> {
    pub fn new() -> Self {
        Self {
            register_types: HashMap::new(),
            constraints: Vec::new(),
        }
    }
}

impl<'src> Default for TypeState<'src> {
    fn default() -> Self {
        Self::new()
    }
}

/// A DTAL program
#[derive(Clone, Debug)]
pub struct DtalProgram<'src> {
    pub functions: Vec<DtalFunction<'src>>,
}

/// A DTAL function
#[derive(Clone, Debug)]
pub struct DtalFunction<'src> {
    pub name: String,
    /// Parameter registers with their types
    pub params: Vec<(Reg, IType<'src>)>,
    /// Return type
    pub return_type: IType<'src>,
    /// Precondition (if any)
    pub precondition: Option<Constraint>,
    /// Basic blocks
    pub blocks: Vec<DtalBlock<'src>>,
}

/// A basic block in DTAL
#[derive(Clone, Debug)]
pub struct DtalBlock<'src> {
    /// Block label
    pub label: String,
    /// Type state at block entry (for verification)
    pub entry_state: TypeState<'src>,
    /// Instructions in this block
    pub instructions: Vec<DtalInstr<'src>>,
}

/// Binary operations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
        }
    }
}

/// Comparison operations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CmpOp::Eq => write!(f, "eq"),
            CmpOp::Ne => write!(f, "ne"),
            CmpOp::Lt => write!(f, "lt"),
            CmpOp::Le => write!(f, "le"),
            CmpOp::Gt => write!(f, "gt"),
            CmpOp::Ge => write!(f, "ge"),
        }
    }
}

/// A DTAL instruction
#[derive(Clone, Debug)]
pub enum DtalInstr<'src> {
    // Data movement
    /// mov rd, imm
    MovImm { dst: Reg, imm: i64, ty: IType<'src> },
    /// mov rd, rs
    MovReg { dst: Reg, src: Reg, ty: IType<'src> },
    /// load rd, [base + offset]
    Load {
        dst: Reg,
        base: Reg,
        offset: Reg,
        ty: IType<'src>,
    },
    /// store [base + offset], src
    Store { base: Reg, offset: Reg, src: Reg },

    // Arithmetic
    /// rd = lhs op rhs
    BinOp {
        op: BinaryOp,
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        ty: IType<'src>,
    },
    /// rd = rs + imm
    AddImm {
        dst: Reg,
        src: Reg,
        imm: i64,
        ty: IType<'src>,
    },

    // Comparison
    /// cmp lhs, rhs
    Cmp { lhs: Reg, rhs: Reg },
    /// cmp lhs, imm
    CmpImm { lhs: Reg, imm: i64 },
    /// Set dst to 1 if condition is true (based on flags), else 0
    SetCC { dst: Reg, cond: CmpOp },

    // Logical
    /// not rd, rs
    Not { dst: Reg, src: Reg, ty: IType<'src> },

    // Control flow
    /// jmp label
    Jmp { target: String },
    /// branch if condition
    Branch { cond: CmpOp, target: String },
    /// call function
    Call { target: String, return_ty: IType<'src> },
    /// ret
    Ret,

    // Stack operations
    /// push rs
    Push { src: Reg, ty: IType<'src> },
    /// pop rd
    Pop { dst: Reg, ty: IType<'src> },
    /// alloca rd, size
    Alloca {
        dst: Reg,
        size: u32,
        ty: IType<'src>,
    },

    // Annotations (for verification)
    /// Type annotation for a register
    TypeAnnotation { reg: Reg, ty: IType<'src> },
    /// Assume a constraint
    ConstraintAssume { constraint: Constraint },
    /// Assert a constraint (for bounds checks)
    ConstraintAssert { constraint: Constraint, msg: String },
}
