//! Utilities for constructing TIR functions and constraints.
//!
//! [`TirBuilder`] tracks the current block under construction, allocates
//! virtual registers and block IDs, and packages completed blocks into a
//! [`TirFunction`].
//!
//! # Example
//!
//! ```text
//! let entry = builder.new_block();
//! builder.start_block(entry);
//! let value = builder.fresh_reg();
//! builder.add_instr(...);
//! builder.finish_block(return value, vec![]);
//! ```
//!
//! # Design Notes
//!
//! The builder is intentionally small: it preserves construction order and
//! enforces "one open block at a time", but it does not perform whole-function
//! validation. That validation belongs in later passes that have complete CFG
//! context.
//!
//! # Panics
//!
//! [`TirBuilder::start_block`] panics if another block is already open, and
//! [`TirBuilder::finish_block`] panics if no block is open.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg, VirtualRegAllocator};
use crate::backend::tir::instr::{Terminator, TirInstr};
use crate::backend::tir::phi::PhiNode;
use crate::backend::tir::program::{BasicBlock, TirFunction};
use crate::backend::tir::types::{BinaryOp, BlockId, BlockIdAllocator, RegisterState};
use crate::common::ownership::ParameterKind;
use crate::common::types::IType;
use std::collections::HashMap;

/// Stateful builder for TIR functions.
pub struct TirBuilder<'src> {
    /// Virtual register allocator.
    pub reg_alloc: VirtualRegAllocator,
    /// Block ID allocator.
    pub block_alloc: BlockIdAllocator,
    /// Completed blocks.
    pub blocks: HashMap<BlockId, BasicBlock<'src>>,
    /// Current block being built.
    current_block: Option<BlockId>,
    current_instructions: Vec<TirInstr<'src>>,
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

    /// Allocate a fresh virtual register.
    pub fn fresh_reg(&mut self) -> VirtualReg {
        self.reg_alloc.fresh()
    }

    /// Create a new block ID.
    pub fn new_block(&mut self) -> BlockId {
        self.block_alloc.fresh()
    }

    /// Start building a block.
    pub fn start_block(&mut self, id: BlockId) {
        assert!(
            self.current_block.is_none(),
            "Must finish current block before starting a new one"
        );
        self.current_block = Some(id);
        self.current_instructions.clear();
        self.current_phi_nodes.clear();
    }

    /// Add a phi node to the current block.
    pub fn add_phi(&mut self, phi: PhiNode<'src>) {
        self.current_phi_nodes.push(phi);
    }

    /// Add an instruction to the current block.
    pub fn add_instr(&mut self, instr: TirInstr<'src>) {
        self.current_instructions.push(instr);
    }

    /// Finish the current block with a terminator.
    pub fn finish_block(&mut self, terminator: Terminator, predecessors: Vec<BlockId>) {
        let id = self.current_block.take().expect("No block to finish");

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

    /// Return whether a block is currently being built.
    pub fn is_building(&self) -> bool {
        self.current_block.is_some()
    }

    /// Return the current block ID, if any.
    pub fn current_block_id(&self) -> Option<BlockId> {
        self.current_block
    }

    /// Build the function.
    #[allow(clippy::too_many_arguments)]
    pub fn build(
        self,
        name: String,
        params: Vec<(VirtualReg, IType<'src>)>,
        parameter_kinds: Vec<ParameterKind>,
        param_names: Vec<String>,
        return_type: IType<'src>,
        returns_owned: bool,
        precondition: Option<Constraint>,
        postcondition: Option<Constraint>,
        entry_block: BlockId,
    ) -> TirFunction<'src> {
        TirFunction {
            name,
            params,
            parameter_kinds,
            param_names,
            return_type,
            returns_owned,
            precondition,
            postcondition,
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

/// Create a constraint from a comparison binary operation.
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
        _ => Constraint::True,
    }
}

/// Negate a constraint, preserving quantifier structure where possible.
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
        Constraint::And(l, r) => Constraint::Or(
            Box::new(negate_constraint(*l)),
            Box::new(negate_constraint(*r)),
        ),
        Constraint::Or(l, r) => Constraint::And(
            Box::new(negate_constraint(*l)),
            Box::new(negate_constraint(*r)),
        ),
        Constraint::Not(c) => *c,
        Constraint::Implies(l, r) => Constraint::And(l, Box::new(negate_constraint(*r))),
        Constraint::Forall {
            var,
            lower,
            upper,
            body,
        } => Constraint::Exists {
            var,
            lower,
            upper,
            body: Box::new(negate_constraint(*body)),
        },
        Constraint::Exists {
            var,
            lower,
            upper,
            body,
        } => Constraint::Forall {
            var,
            lower,
            upper,
            body: Box::new(negate_constraint(*body)),
        },
    }
}

/// Combine two constraints with `AND`.
pub fn and_constraints(c1: Constraint, c2: Constraint) -> Constraint {
    match (&c1, &c2) {
        (Constraint::True, _) => c2,
        (_, Constraint::True) => c1,
        (Constraint::False, _) | (_, Constraint::False) => Constraint::False,
        _ => Constraint::And(Box::new(c1), Box::new(c2)),
    }
}

/// Combine two constraints with `OR`.
pub fn or_constraints(c1: Constraint, c2: Constraint) -> Constraint {
    match (&c1, &c2) {
        (Constraint::False, _) => c2,
        (_, Constraint::False) => c1,
        (Constraint::True, _) | (_, Constraint::True) => Constraint::True,
        _ => Constraint::Or(Box::new(c1), Box::new(c2)),
    }
}
