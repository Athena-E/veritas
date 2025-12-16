//! Lowering context for TAST to TIR translation
//!
//! This module provides the context that tracks state during lowering,
//! including variable-to-register mappings and the current block.

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::backend::tir::{
    BasicBlock, BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr,
};
use crate::common::types::IType;
use std::collections::HashMap;

/// Context for lowering TAST to TIR
///
/// Tracks the current state during lowering, including:
/// - Variable name to SSA register mappings
/// - The TIR builder for creating blocks and instructions
pub struct LoweringContext<'src> {
    /// The TIR builder
    pub builder: TirBuilder<'src>,

    /// Map from variable names to their current SSA register
    /// In SSA, each assignment creates a new register, so this
    /// always points to the "current" version of each variable.
    var_map: HashMap<String, VirtualReg>,

    /// Stack of variable maps for nested scopes
    /// Used to restore variable bindings when exiting a scope
    scope_stack: Vec<HashMap<String, VirtualReg>>,
}

impl<'src> LoweringContext<'src> {
    /// Create a new lowering context
    pub fn new() -> Self {
        Self {
            builder: TirBuilder::new(),
            var_map: HashMap::new(),
            scope_stack: Vec::new(),
        }
    }

    // ========================================================================
    // Register allocation
    // ========================================================================

    /// Allocate a fresh virtual register
    pub fn fresh_reg(&mut self) -> VirtualReg {
        self.builder.fresh_reg()
    }

    // ========================================================================
    // Variable mapping (SSA)
    // ========================================================================

    /// Bind a variable name to an SSA register
    pub fn bind_var(&mut self, name: &str, reg: VirtualReg) {
        self.var_map.insert(name.to_string(), reg);
    }

    /// Look up the current SSA register for a variable
    pub fn lookup_var(&self, name: &str) -> Option<VirtualReg> {
        self.var_map.get(name).copied()
    }

    /// Get a snapshot of the current variable map
    /// Used for tracking which variables are modified in branches
    pub fn snapshot_var_map(&self) -> HashMap<String, VirtualReg> {
        self.var_map.clone()
    }

    /// Get all variables that differ between two snapshots
    /// Returns: Vec<(var_name, reg_in_snapshot1, reg_in_snapshot2)>
    pub fn diff_var_maps(
        &self,
        before: &HashMap<String, VirtualReg>,
        after: &HashMap<String, VirtualReg>,
    ) -> Vec<(String, VirtualReg, VirtualReg)> {
        let mut diffs = Vec::new();
        for (name, &after_reg) in after {
            if let Some(&before_reg) = before.get(name) {
                if before_reg != after_reg {
                    diffs.push((name.clone(), before_reg, after_reg));
                }
            }
        }
        diffs
    }

    // ========================================================================
    // Scope management
    // ========================================================================

    /// Enter a new scope (pushes current var_map)
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(self.var_map.clone());
    }

    /// Exit a scope (restores previous var_map)
    pub fn exit_scope(&mut self) {
        if let Some(old_map) = self.scope_stack.pop() {
            self.var_map = old_map;
        }
    }

    // ========================================================================
    // Block management
    // ========================================================================

    /// Create a new block and return its ID
    pub fn new_block(&mut self) -> BlockId {
        self.builder.new_block()
    }

    /// Start building a block
    pub fn start_block(&mut self, id: BlockId) {
        self.builder.start_block(id);
    }

    /// Add an instruction to the current block
    pub fn emit(&mut self, instr: TirInstr<'src>) {
        self.builder.add_instr(instr);
    }

    /// Add a phi node to the current block
    pub fn emit_phi(&mut self, phi: PhiNode<'src>) {
        self.builder.add_phi(phi);
    }

    /// Finish the current block with a terminator
    pub fn finish_block(&mut self, terminator: Terminator, predecessors: Vec<BlockId>) {
        self.builder.finish_block(terminator, predecessors);
    }

    /// Check if currently building a block
    pub fn is_building(&self) -> bool {
        self.builder.is_building()
    }

    /// Get the current block ID
    pub fn current_block(&self) -> Option<BlockId> {
        self.builder.current_block_id()
    }

    // ========================================================================
    // Function building
    // ========================================================================

    /// Build the final TIR function
    pub fn build_function(
        self,
        name: String,
        params: Vec<(VirtualReg, IType<'src>)>,
        return_type: IType<'src>,
        precondition: Option<Constraint>,
        entry_block: BlockId,
    ) -> TirFunction<'src> {
        self.builder
            .build(name, params, return_type, precondition, entry_block)
    }
}

impl<'src> Default for LoweringContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
