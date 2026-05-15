//! DTAL instructions and program structures
//!
//! This module defines the DTAL instruction set and program representation.

use crate::common::ownership::{LifetimeId, OwnershipMode, ParameterKind};
use crate::dtal::constraints::Constraint;
use crate::dtal::regs::Reg;
use crate::dtal::types::DtalType;
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug)]
pub enum CmpOperands {
    RegReg(Reg, Reg),
    RegImm(Reg, i128),
}

#[derive(Clone, Debug)]
pub struct TypeState {
    pub register_types: HashMap<Reg, DtalType>,
    pub constraints: Vec<Constraint>,
    pub last_cmp: Option<CmpOperands>,
    pub stack: Vec<DtalType>,
    pub array_versions: HashMap<Reg, u32>,
    pub proven_assertions: Vec<Constraint>,
    pub spill_types: HashMap<i32, DtalType>,
    pub owned_registers: HashSet<Reg>,
    pub owned_object_ids: HashMap<Reg, u32>,
    pub shared_borrow_object_ids: HashMap<Reg, u32>,
    pub shared_borrow_lifetimes: HashMap<Reg, Option<LifetimeId>>,
    pub mutable_borrow_object_ids: HashMap<Reg, u32>,
    pub mutable_borrow_lifetimes: HashMap<Reg, Option<LifetimeId>>,
    pub owned_stack: Vec<bool>,
    pub owned_stack_object_ids: Vec<Option<u32>>,
    pub shared_borrow_stack_object_ids: Vec<Option<u32>>,
    pub shared_borrow_stack_lifetimes: Vec<Option<Option<LifetimeId>>>,
    pub mutable_borrow_stack_object_ids: Vec<Option<u32>>,
    pub mutable_borrow_stack_lifetimes: Vec<Option<Option<LifetimeId>>>,
    pub owned_spills: HashSet<i32>,
    pub owned_spill_object_ids: HashMap<i32, u32>,
    pub shared_borrow_spill_object_ids: HashMap<i32, u32>,
    pub shared_borrow_spill_lifetimes: HashMap<i32, Option<LifetimeId>>,
    pub mutable_borrow_spill_object_ids: HashMap<i32, u32>,
    pub mutable_borrow_spill_lifetimes: HashMap<i32, Option<LifetimeId>>,
    pub consumed_registers: HashSet<Reg>,
    pub next_object_id: u32,
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
            owned_registers: HashSet::new(),
            owned_object_ids: HashMap::new(),
            shared_borrow_object_ids: HashMap::new(),
            shared_borrow_lifetimes: HashMap::new(),
            mutable_borrow_object_ids: HashMap::new(),
            mutable_borrow_lifetimes: HashMap::new(),
            owned_stack: Vec::new(),
            owned_stack_object_ids: Vec::new(),
            shared_borrow_stack_object_ids: Vec::new(),
            shared_borrow_stack_lifetimes: Vec::new(),
            mutable_borrow_stack_object_ids: Vec::new(),
            mutable_borrow_stack_lifetimes: Vec::new(),
            owned_spills: HashSet::new(),
            owned_spill_object_ids: HashMap::new(),
            shared_borrow_spill_object_ids: HashMap::new(),
            shared_borrow_spill_lifetimes: HashMap::new(),
            mutable_borrow_spill_object_ids: HashMap::new(),
            mutable_borrow_spill_lifetimes: HashMap::new(),
            consumed_registers: HashSet::new(),
            next_object_id: 0,
        }
    }
}

impl Default for TypeState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct DtalProgram {
    pub functions: Vec<DtalFunction>,
}

#[derive(Clone, Debug)]
pub struct DtalFunction {
    pub name: String,
    pub params: Vec<(Reg, DtalType)>,
    pub parameter_kinds: Vec<ParameterKind>,
    pub return_type: DtalType,
    pub precondition: Option<Constraint>,
    pub postcondition: Option<Constraint>,
    pub blocks: Vec<DtalBlock>,
}

#[derive(Clone, Debug)]
pub struct DtalBlock {
    pub label: String,
    pub entry_state: TypeState,
    pub instructions: Vec<DtalInstr>,
}

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

#[derive(Clone, Debug)]
pub enum DtalInstr {
    MovImm {
        dst: Reg,
        imm: i128,
        ty: DtalType,
    },
    MovReg {
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },
    AliasBorrow {
        lifetime: Option<LifetimeId>,
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },
    BorrowMut {
        lifetime: Option<LifetimeId>,
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },
    BorrowEnd {
        lifetime: Option<LifetimeId>,
        src: Reg,
        ty: DtalType,
    },
    MoveOwned {
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },
    Load {
        dst: Reg,
        base: Reg,
        offset: Reg,
        ty: DtalType,
    },
    Store {
        base: Reg,
        offset: Reg,
        src: Reg,
    },
    LoadOp {
        op: BinaryOp,
        dst: Reg,
        base: Reg,
        offset: Reg,
        other: Reg,
        ty: DtalType,
    },

    BinOp {
        op: BinaryOp,
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        ty: DtalType,
    },
    AddImm {
        dst: Reg,
        src: Reg,
        imm: i128,
        ty: DtalType,
    },
    ShlImm {
        dst: Reg,
        src: Reg,
        imm: u8,
        ty: DtalType,
    },
    ShrImm {
        dst: Reg,
        src: Reg,
        imm: u8,
        ty: DtalType,
    },

    Cmp {
        lhs: Reg,
        rhs: Reg,
    },
    CmpImm {
        lhs: Reg,
        imm: i128,
    },
    SetCC {
        dst: Reg,
        cond: CmpOp,
    },

    Not {
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },
    Neg {
        dst: Reg,
        src: Reg,
        ty: DtalType,
    },

    Jmp {
        target: String,
    },
    Branch {
        cond: CmpOp,
        target: String,
    },
    Call {
        target: String,
        arg_kinds: Vec<ParameterKind>,
        return_ty: DtalType,
        ownership: OwnershipMode,
    },
    Ret,

    Push {
        src: Reg,
        ty: DtalType,
    },
    Pop {
        dst: Reg,
        ty: DtalType,
    },
    Alloca {
        dst: Reg,
        size: u32,
        ty: DtalType,
    },
    DropOwned {
        src: Reg,
        ty: DtalType,
    },

    PortIn {
        dst: Reg,
        port: Reg,
    },
    PortOut {
        port: Reg,
        value: Reg,
    },

    Cqo,
    Idiv {
        src: Reg,
    },
    SpillStore {
        src: Reg,
        offset: i32,
        ty: DtalType,
    },
    SpillLoad {
        dst: Reg,
        offset: i32,
        ty: DtalType,
    },
    Prologue {
        frame_size: u32,
        callee_saved: Vec<Reg>,
    },
    Epilogue {
        callee_saved: Vec<Reg>,
    },

    TypeAnnotation {
        reg: Reg,
        ty: DtalType,
    },
    ConstraintAssert {
        constraint: Constraint,
    },
}
