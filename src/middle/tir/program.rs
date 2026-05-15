//! TIR program, function, and basic block structures.
//!
//! A TIR function is a typed SSA control-flow graph. Blocks contain phi nodes,
//! ordinary instructions, and a terminator; functions also keep contract and
//! ownership metadata needed by DTAL generation.
//!
//! # Control Flow
//!
//! ```text
//! entry
//!  | \
//!  |  `-> then
//!  |      |
//!  `-> else
//!         |
//!         v
//!       join(phi)
//! ```
//!
//! [`BasicBlock::successors`] derives graph edges from the terminator. The
//! predecessor list is stored separately because phi nodes and join-state
//! checks need to know which incoming edge produced each value.
//!
//! # Related Modules
//!
//! [`crate::middle::tir::instr`] defines block contents and terminators, while
//! [`crate::middle::tir::phi`] defines the join nodes attached to block entry.

use crate::common::ownership::ParameterKind;
use crate::common::types::IType;
use crate::dtal::{Constraint, VirtualReg};
use crate::middle::tir::instr::{Terminator, TirInstr};
use crate::middle::tir::phi::PhiNode;
use crate::middle::tir::types::{BlockId, RegisterState};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct TirProgram<'src> {
    pub functions: Vec<TirFunction<'src>>,
}

impl<'src> TirProgram<'src> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, func: TirFunction<'src>) {
        self.functions.push(func);
    }
}

impl<'src> Default for TirProgram<'src> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct TirFunction<'src> {
    pub name: String,
    pub params: Vec<(VirtualReg, IType<'src>)>,
    pub parameter_kinds: Vec<ParameterKind>,
    pub param_names: Vec<String>,
    pub return_type: IType<'src>,
    pub returns_owned: bool,
    pub precondition: Option<Constraint>,
    pub postcondition: Option<Constraint>,
    pub entry_block: BlockId,
    pub blocks: HashMap<BlockId, BasicBlock<'src>>,
}

impl<'src> TirFunction<'src> {
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock<'src>> {
        self.blocks.get(&id)
    }

    pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock<'src>> {
        self.blocks.get_mut(&id)
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item = (&BlockId, &BasicBlock<'src>)> {
        self.blocks.iter()
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlock<'src> {
    pub id: BlockId,
    pub phi_nodes: Vec<PhiNode<'src>>,
    pub instructions: Vec<TirInstr<'src>>,
    pub terminator: Terminator,
    pub predecessors: Vec<BlockId>,
    pub entry_state: RegisterState<'src>,
}

impl<'src> BasicBlock<'src> {
    pub fn new(id: BlockId, terminator: Terminator) -> Self {
        Self {
            id,
            phi_nodes: Vec::new(),
            instructions: Vec::new(),
            terminator,
            predecessors: Vec::new(),
            entry_state: RegisterState::new(),
        }
    }

    pub fn add_instruction(&mut self, instr: TirInstr<'src>) {
        self.instructions.push(instr);
    }

    pub fn add_phi(&mut self, phi: PhiNode<'src>) {
        self.phi_nodes.push(phi);
    }

    pub fn add_predecessor(&mut self, pred: BlockId) {
        if !self.predecessors.contains(&pred) {
            self.predecessors.push(pred);
        }
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match &self.terminator {
            Terminator::Jump { target } => vec![*target],
            Terminator::Branch {
                true_target,
                false_target,
                ..
            } => vec![*true_target, *false_target],
            Terminator::Return { .. } => vec![],
            Terminator::Unreachable => vec![],
        }
    }
}
