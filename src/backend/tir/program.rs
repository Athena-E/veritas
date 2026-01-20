//! TIR program structures
//!
//! This module defines the program, function, and basic block structures for TIR.

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::backend::tir::instr::{Terminator, TirInstr};
use crate::backend::tir::phi::PhiNode;
use crate::backend::tir::types::{BlockId, RegisterState};
use crate::common::types::IType;
use std::collections::HashMap;

/// A TIR program consists of functions
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
    /// Postcondition (optional)
    pub postcondition: Option<Constraint>,
    /// Entry block ID
    pub entry_block: BlockId,
    /// All basic blocks
    pub blocks: HashMap<BlockId, BasicBlock<'src>>,
}

impl<'src> TirFunction<'src> {
    /// Get a reference to a block by ID
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock<'src>> {
        self.blocks.get(&id)
    }

    /// Get a mutable reference to a block by ID
    pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock<'src>> {
        self.blocks.get_mut(&id)
    }

    /// Iterate over all blocks
    pub fn iter_blocks(&self) -> impl Iterator<Item = (&BlockId, &BasicBlock<'src>)> {
        self.blocks.iter()
    }
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

impl<'src> BasicBlock<'src> {
    /// Create a new basic block with the given ID and terminator
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

    /// Add an instruction to this block
    pub fn add_instruction(&mut self, instr: TirInstr<'src>) {
        self.instructions.push(instr);
    }

    /// Add a phi node to this block
    pub fn add_phi(&mut self, phi: PhiNode<'src>) {
        self.phi_nodes.push(phi);
    }

    /// Add a predecessor block
    pub fn add_predecessor(&mut self, pred: BlockId) {
        if !self.predecessors.contains(&pred) {
            self.predecessors.push(pred);
        }
    }

    /// Get successor blocks from the terminator
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
