//! Typed Intermediate Representation (TIR) with SSA
//!
//! TIR is a CFG-based intermediate representation in SSA form.
//! Each virtual register (SSA variable) is defined exactly once
//! and carries a type annotation.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg, VirtualRegAllocator};
use crate::common::types::IType;
use std::collections::HashMap;
use std::fmt;

/// Unique identifier for a basic block
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

/// A TIR program consists of functions
#[derive(Clone, Debug)]
pub struct TirProgram<'src> {
    pub functions: Vec<TirFunction<'src>>,
}

/// A function in TIR is a CFG in SSA form
#[derive(Clone, Debug)]
pub struct TirFunction<'src> {
    pub name: String,
    /// Parameters with their SSA registers and types
    pub params: Vec<(VirtualReg, IType<'src>)>,
    /// Return type
    pub return_type: IType<'src>,
    /// Precondition (optional)
    pub precondition: Option<Constraint>,
    /// Entry block ID
    pub entry_block: BlockId,
    /// All basic blocks
    pub blocks: HashMap<BlockId, BasicBlock<'src>>,
}

/// A basic block in SSA form
#[derive(Clone, Debug)]
pub struct BasicBlock<'src> {
    pub id: BlockId,
    /// Phi nodes at block entry (SSA merge points)
    pub phi_nodes: Vec<PhiNode<'src>>,
    /// Instructions in this block
    pub instructions: Vec<TirInstr<'src>>,
    /// Block terminator
    pub terminator: Terminator,
    /// Predecessor blocks
    pub predecessors: Vec<BlockId>,
    /// Type state at block entry (after phi nodes)
    pub entry_state: RegisterState<'src>,
}

/// SSA Phi node: merges values from predecessor blocks
#[derive(Clone, Debug)]
pub struct PhiNode<'src> {
    /// The SSA variable being defined
    pub dst: VirtualReg,
    /// Type of the result (join of incoming types)
    pub ty: IType<'src>,
    /// Incoming values: (predecessor block, SSA variable from that block)
    pub incoming: Vec<(BlockId, VirtualReg)>,
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

/// TIR instructions (three-address code in SSA form)
#[derive(Clone, Debug)]
pub enum TirInstr<'src> {
    /// dst = immediate
    LoadImm {
        dst: VirtualReg,
        value: i64,
        ty: IType<'src>,
    },

    /// dst = src (copy)
    Copy {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = lhs op rhs
    BinOp {
        dst: VirtualReg,
        op: BinaryOp,
        lhs: VirtualReg,
        rhs: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = op operand
    UnaryOp {
        dst: VirtualReg,
        op: UnaryOp,
        operand: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = base[index]
    ArrayLoad {
        dst: VirtualReg,
        base: VirtualReg,
        index: VirtualReg,
        element_ty: IType<'src>,
        bounds_proof: BoundsProof,
    },

    /// base[index] = value
    ArrayStore {
        base: VirtualReg,
        index: VirtualReg,
        value: VirtualReg,
        bounds_proof: BoundsProof,
    },

    /// dst = call func(args...)
    Call {
        dst: Option<VirtualReg>,
        func: String,
        args: Vec<VirtualReg>,
        result_ty: IType<'src>,
    },

    /// Allocate array on stack
    AllocArray {
        dst: VirtualReg,
        element_ty: IType<'src>,
        size: i64,
    },

    /// Assume a constraint (from branch or precondition)
    AssumeConstraint {
        constraint: Constraint,
    },
}

impl<'src> TirInstr<'src> {
    /// Get the destination register of this instruction (if any)
    pub fn dst(&self) -> Option<VirtualReg> {
        match self {
            TirInstr::LoadImm { dst, .. } => Some(*dst),
            TirInstr::Copy { dst, .. } => Some(*dst),
            TirInstr::BinOp { dst, .. } => Some(*dst),
            TirInstr::UnaryOp { dst, .. } => Some(*dst),
            TirInstr::ArrayLoad { dst, .. } => Some(*dst),
            TirInstr::ArrayStore { .. } => None,
            TirInstr::Call { dst, .. } => *dst,
            TirInstr::AllocArray { dst, .. } => Some(*dst),
            TirInstr::AssumeConstraint { .. } => None,
        }
    }

    /// Get the type of the result (if any)
    pub fn result_type(&self) -> Option<&IType<'src>> {
        match self {
            TirInstr::LoadImm { ty, .. } => Some(ty),
            TirInstr::Copy { ty, .. } => Some(ty),
            TirInstr::BinOp { ty, .. } => Some(ty),
            TirInstr::UnaryOp { ty, .. } => Some(ty),
            TirInstr::ArrayLoad { element_ty, .. } => Some(element_ty),
            TirInstr::ArrayStore { .. } => None,
            TirInstr::Call { result_ty, .. } => Some(result_ty),
            TirInstr::AllocArray { element_ty, .. } => Some(element_ty),
            TirInstr::AssumeConstraint { .. } => None,
        }
    }
}

/// Block terminator
#[derive(Clone, Debug)]
pub enum Terminator {
    /// Unconditional jump
    Jump {
        target: BlockId,
    },

    /// Conditional branch
    Branch {
        cond: VirtualReg,
        true_target: BlockId,
        false_target: BlockId,
        /// Constraint added to true branch
        true_constraint: Constraint,
        /// Constraint added to false branch
        false_constraint: Constraint,
    },

    /// Return from function
    Return {
        value: Option<VirtualReg>,
    },

    /// Unreachable (after error or infinite loop)
    Unreachable,
}

/// Builder for constructing TIR functions
pub struct TirBuilder<'src> {
    /// Virtual register allocator
    pub reg_alloc: VirtualRegAllocator,
    /// Block ID allocator
    pub block_alloc: BlockIdAllocator,
    /// All blocks built so far
    pub blocks: HashMap<BlockId, BasicBlock<'src>>,
    /// Current block being built
    current_block: Option<BlockId>,
    /// Instructions for current block
    current_instructions: Vec<TirInstr<'src>>,
    /// Phi nodes for current block
    current_phi_nodes: Vec<PhiNode<'src>>,
}

impl<'src> TirBuilder<'src> {
    pub fn new() -> Self {
        Self {
            reg_alloc: VirtualRegAllocator::new(),
            block_alloc: BlockIdAllocator::new(),
            blocks: HashMap::new(),
            current_block: None,
            current_instructions: Vec::new(),
            current_phi_nodes: Vec::new(),
        }
    }

    /// Allocate a fresh virtual register
    pub fn fresh_reg(&mut self) -> VirtualReg {
        self.reg_alloc.fresh()
    }

    /// Create a new block and return its ID
    pub fn new_block(&mut self) -> BlockId {
        self.block_alloc.fresh()
    }

    /// Start building a block
    pub fn start_block(&mut self, id: BlockId) {
        assert!(
            self.current_block.is_none(),
            "Must finish current block before starting a new one"
        );
        self.current_block = Some(id);
        self.current_instructions.clear();
        self.current_phi_nodes.clear();
    }

    /// Add a phi node to the current block
    pub fn add_phi(&mut self, phi: PhiNode<'src>) {
        self.current_phi_nodes.push(phi);
    }

    /// Add an instruction to the current block
    pub fn add_instr(&mut self, instr: TirInstr<'src>) {
        self.current_instructions.push(instr);
    }

    /// Finish the current block with a terminator
    pub fn finish_block(&mut self, terminator: Terminator, predecessors: Vec<BlockId>) {
        let id = self
            .current_block
            .take()
            .expect("No block to finish");

        let block = BasicBlock {
            id,
            phi_nodes: std::mem::take(&mut self.current_phi_nodes),
            instructions: std::mem::take(&mut self.current_instructions),
            terminator,
            predecessors,
            entry_state: RegisterState::new(),
        };

        self.blocks.insert(id, block);
    }

    /// Build the function
    pub fn build(
        self,
        name: String,
        params: Vec<(VirtualReg, IType<'src>)>,
        return_type: IType<'src>,
        precondition: Option<Constraint>,
        entry_block: BlockId,
    ) -> TirFunction<'src> {
        TirFunction {
            name,
            params,
            return_type,
            precondition,
            entry_block,
            blocks: self.blocks,
        }
    }
}

impl<'src> Default for TirBuilder<'src> {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to create constraints from comparisons
pub fn constraint_from_binop(op: BinaryOp, lhs: &str, rhs: &str) -> Constraint {
    let lhs_expr = IndexExpr::Var(lhs.to_string());
    let rhs_expr = IndexExpr::Var(rhs.to_string());
    match op {
        BinaryOp::Eq => Constraint::Eq(lhs_expr, rhs_expr),
        BinaryOp::Ne => Constraint::Ne(lhs_expr, rhs_expr),
        BinaryOp::Lt => Constraint::Lt(lhs_expr, rhs_expr),
        BinaryOp::Le => Constraint::Le(lhs_expr, rhs_expr),
        BinaryOp::Gt => Constraint::Gt(lhs_expr, rhs_expr),
        BinaryOp::Ge => Constraint::Ge(lhs_expr, rhs_expr),
        _ => Constraint::True, // Non-comparison ops
    }
}

/// Negate a constraint
pub fn negate_constraint(c: Constraint) -> Constraint {
    match c {
        Constraint::True => Constraint::False,
        Constraint::False => Constraint::True,
        Constraint::Eq(l, r) => Constraint::Ne(l, r),
        Constraint::Ne(l, r) => Constraint::Eq(l, r),
        Constraint::Lt(l, r) => Constraint::Ge(l, r),
        Constraint::Le(l, r) => Constraint::Gt(l, r),
        Constraint::Gt(l, r) => Constraint::Le(l, r),
        Constraint::Ge(l, r) => Constraint::Lt(l, r),
        Constraint::And(l, r) => {
            Constraint::Or(Box::new(negate_constraint(*l)), Box::new(negate_constraint(*r)))
        }
        Constraint::Or(l, r) => {
            Constraint::And(Box::new(negate_constraint(*l)), Box::new(negate_constraint(*r)))
        }
        Constraint::Not(c) => *c,
    }
}
