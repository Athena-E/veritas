//! DTAL instructions and program structures
//!
//! This module defines the DTAL instruction set and program representation.

use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::regs::Reg;
use crate::backend::dtal::types::DtalType;
use std::collections::HashMap;
use std::fmt;

/// Operands of the most recent comparison instruction
#[derive(Clone, Debug)]
pub enum CmpOperands {
    /// cmp reg, reg
    RegReg(Reg, Reg),
    /// cmp reg, imm
    RegImm(Reg, i128),
}

/// Type state at a program point
#[derive(Clone, Debug)]
pub struct TypeState {
    /// Type of each register
    pub register_types: HashMap<Reg, DtalType>,
    /// Active constraints
    pub constraints: Vec<Constraint>,
    /// Operands of the most recent comparison (for deriving branch constraints)
    pub last_cmp: Option<CmpOperands>,
    /// Typed stack (Xi & Harper's `S = τ :: S`).
    /// Top of stack is the last element.
    pub stack: Vec<DtalType>,
    /// Array version tracking for store/select axioms.
    /// Each store to an array register increments its version,
    /// creating a new uninterpreted function name in the constraint domain.
    pub array_versions: HashMap<Reg, u32>,
    /// Frontend-proven assertions (e.g., loop invariants) that survive joins.
    /// Taken as union across predecessors since they've been verified by the
    /// frontend typechecker and are safe to assume at join points.
    pub proven_assertions: Vec<Constraint>,
    /// Types stored at stack spill slot offsets (post-regalloc).
    /// Maps frame offset (negative from rbp) to the type stored there.
    pub spill_types: HashMap<i32, DtalType>,
}

impl TypeState {
    pub fn new() -> Self {
        Self {
            register_types: HashMap::new(),
            constraints: Vec::new(),
            last_cmp: None,
            stack: Vec::new(),
            array_versions: HashMap::new(),
            proven_assertions: Vec::new(),
            spill_types: HashMap::new(),
        }
    }
}

impl Default for TypeState {
    fn default() -> Self {
        Self::new()
    }
}

/// A DTAL program
#[derive(Clone, Debug)]
pub struct DtalProgram {
    pub functions: Vec<DtalFunction>,
}

/// A DTAL function
#[derive(Clone, Debug)]
pub struct DtalFunction {
    pub name: String,
    /// Parameter registers with their types
    pub params: Vec<(Reg, DtalType)>,
    /// Return type
    pub return_type: DtalType,
    /// Precondition (if any)
    pub precondition: Option<Constraint>,
    /// Postcondition (if any)
    pub postcondition: Option<Constraint>,
    /// Basic blocks
    pub blocks: Vec<DtalBlock>,
}

/// A basic block in DTAL
#[derive(Clone, Debug)]
pub struct DtalBlock {
    /// Block label
    pub label: String,
    /// Type state at block entry (for verification)
    pub entry_state: TypeState,
    /// Instructions in this block
    pub instructions: Vec<DtalInstr>,
}

/// Binary operations
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
    And,
    Or,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::Div => write!(f, "div"),
            BinaryOp::Mod => write!(f, "mod"),
            BinaryOp::BitAnd => write!(f, "bitand"),
            BinaryOp::BitOr => write!(f, "bitor"),
            BinaryOp::BitXor => write!(f, "bitxor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::Shr => write!(f, "shr"),
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
pub enum DtalInstr {
    // Data movement
    /// mov rd, imm
    MovImm { dst: Reg, imm: i128, ty: DtalType },
    /// mov rd, rs
    MovReg { dst: Reg, src: Reg, ty: DtalType },
    /// load rd, [base + offset]
    Load {
        dst: Reg,
        base: Reg,
        offset: Reg,
        ty: DtalType,
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
        ty: DtalType,
    },
    /// rd = rs + imm
    AddImm {
        dst: Reg,
        src: Reg,
        imm: i128,
        ty: DtalType,
    },
    /// rd = rs << imm (left shift by immediate)
    ShlImm {
        dst: Reg,
        src: Reg,
        imm: u8,
        ty: DtalType,
    },
    /// rd = rs >> imm (logical right shift by immediate)
    ShrImm {
        dst: Reg,
        src: Reg,
        imm: u8,
        ty: DtalType,
    },

    // Comparison
    /// cmp lhs, rhs
    Cmp { lhs: Reg, rhs: Reg },
    /// cmp lhs, imm
    CmpImm { lhs: Reg, imm: i128 },
    /// Set dst to 1 if condition is true (based on flags), else 0
    SetCC { dst: Reg, cond: CmpOp },

    // Logical / Unary
    /// not rd, rs
    Not { dst: Reg, src: Reg, ty: DtalType },
    /// neg rd, rs (rd = 0 - rs)
    Neg { dst: Reg, src: Reg, ty: DtalType },

    // Control flow
    /// jmp label
    Jmp { target: String },
    /// branch if condition
    Branch { cond: CmpOp, target: String },
    /// call function
    Call { target: String, return_ty: DtalType },
    /// ret
    Ret,

    // Stack operations
    /// push rs
    Push { src: Reg, ty: DtalType },
    /// pop rd
    Pop { dst: Reg, ty: DtalType },
    /// alloca rd, size
    Alloca { dst: Reg, size: u32, ty: DtalType },

    // Port I/O (bare-metal hardware access)
    /// in al, dx: read byte from I/O port in src to dst
    PortIn { dst: Reg, port: Reg },
    /// out dx, al: write byte from src to I/O port in dst
    PortOut { port: Reg, value: Reg },

    // Physical allocation instructions (post-regalloc)
    /// cqo: sign-extend rax into rdx:rax (required before idiv)
    Cqo,
    /// idiv src: signed divide rdx:rax by src; quotient → rax, remainder → rdx
    Idiv { src: Reg },
    /// Store register to stack spill slot at [rbp + offset]
    SpillStore { src: Reg, offset: i32, ty: DtalType },
    /// Load register from stack spill slot at [rbp + offset]
    SpillLoad { dst: Reg, offset: i32, ty: DtalType },
    /// Function prologue: push rbp, set frame, save callee-saved, allocate frame
    Prologue {
        frame_size: u32,
        callee_saved: Vec<Reg>,
    },
    /// Function epilogue: restore callee-saved, pop rbp, ret
    Epilogue { callee_saved: Vec<Reg> },

    // Annotations (for verification)
    /// Type annotation for a register
    TypeAnnotation { reg: Reg, ty: DtalType },
    /// Assert a constraint (for bounds checks and loop invariants)
    ConstraintAssert { constraint: Constraint },
}
