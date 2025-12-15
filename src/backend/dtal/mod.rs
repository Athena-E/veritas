//! DTAL (Dependently Typed Assembly Language) AST definitions
//!
//! This module defines the target language for the Veritas compiler.
//! DTAL is inspired by Xi and Harper's dependently typed assembly language,
//! with type annotations that allow independent verification.

use crate::common::types::IType;
use std::collections::HashMap;
use std::fmt;

/// A virtual register (before physical allocation)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VirtualReg(pub u32);

impl fmt::Display for VirtualReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

/// Allocator for virtual registers
#[derive(Debug, Default)]
pub struct VirtualRegAllocator {
    next_id: u32,
}

impl VirtualRegAllocator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    /// Allocate a fresh virtual register
    pub fn fresh(&mut self) -> VirtualReg {
        let id = self.next_id;
        self.next_id += 1;
        VirtualReg(id)
    }

    /// Get the number of registers allocated so far
    pub fn count(&self) -> u32 {
        self.next_id
    }
}

/// Physical registers (for extension phase)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum PhysicalReg {
    // General purpose registers
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // Special registers
    SP, // Stack pointer
    FP, // Frame pointer
    LR, // Link register (return address)
}

impl PhysicalReg {
    /// Registers available for allocation
    #[allow(dead_code)]
    pub fn allocatable() -> &'static [PhysicalReg] {
        use PhysicalReg::*;
        &[R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11]
    }

    /// Registers used for parameter passing
    #[allow(dead_code)]
    pub fn param_regs() -> &'static [PhysicalReg] {
        use PhysicalReg::*;
        &[R0, R1, R2, R3, R4, R5, R6, R7]
    }

    /// Return value register
    #[allow(dead_code)]
    pub fn return_reg() -> PhysicalReg {
        PhysicalReg::R0
    }
}

impl fmt::Display for PhysicalReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PhysicalReg::*;
        match self {
            R0 => write!(f, "r0"),
            R1 => write!(f, "r1"),
            R2 => write!(f, "r2"),
            R3 => write!(f, "r3"),
            R4 => write!(f, "r4"),
            R5 => write!(f, "r5"),
            R6 => write!(f, "r6"),
            R7 => write!(f, "r7"),
            R8 => write!(f, "r8"),
            R9 => write!(f, "r9"),
            R10 => write!(f, "r10"),
            R11 => write!(f, "r11"),
            R12 => write!(f, "r12"),
            R13 => write!(f, "r13"),
            R14 => write!(f, "r14"),
            R15 => write!(f, "r15"),
            SP => write!(f, "sp"),
            FP => write!(f, "fp"),
            LR => write!(f, "lr"),
        }
    }
}

/// A register that can be either virtual or physical
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Reg {
    Virtual(VirtualReg),
    Physical(PhysicalReg),
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::Virtual(v) => write!(f, "{}", v),
            Reg::Physical(p) => write!(f, "{}", p),
        }
    }
}

impl From<VirtualReg> for Reg {
    fn from(v: VirtualReg) -> Self {
        Reg::Virtual(v)
    }
}

impl From<PhysicalReg> for Reg {
    fn from(p: PhysicalReg) -> Self {
        Reg::Physical(p)
    }
}

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
    MovImm {
        dst: Reg,
        imm: i64,
        ty: IType<'src>,
    },
    /// mov rd, rs
    MovReg {
        dst: Reg,
        src: Reg,
        ty: IType<'src>,
    },
    /// load rd, [base + offset]
    Load {
        dst: Reg,
        base: Reg,
        offset: Reg,
        ty: IType<'src>,
    },
    /// store [base + offset], src
    Store {
        base: Reg,
        offset: Reg,
        src: Reg,
    },

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
    Cmp {
        lhs: Reg,
        rhs: Reg,
    },
    /// cmp lhs, imm
    CmpImm {
        lhs: Reg,
        imm: i64,
    },

    // Logical
    /// not rd, rs
    Not {
        dst: Reg,
        src: Reg,
        ty: IType<'src>,
    },

    // Control flow
    /// jmp label
    Jmp {
        target: String,
    },
    /// branch if condition
    Branch {
        cond: CmpOp,
        target: String,
    },
    /// call function
    Call {
        target: String,
    },
    /// ret
    Ret,

    // Stack operations
    /// push rs
    Push {
        src: Reg,
        ty: IType<'src>,
    },
    /// pop rd
    Pop {
        dst: Reg,
        ty: IType<'src>,
    },
    /// alloca rd, size
    Alloca {
        dst: Reg,
        size: u32,
        ty: IType<'src>,
    },

    // Annotations (for verification)
    /// Type annotation for a register
    TypeAnnotation {
        reg: Reg,
        ty: IType<'src>,
    },
    /// Assume a constraint
    ConstraintAssume {
        constraint: Constraint,
    },
    /// Assert a constraint (for bounds checks)
    ConstraintAssert {
        constraint: Constraint,
        msg: String,
    },
}
