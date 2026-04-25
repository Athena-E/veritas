//! Lowering context for TAST to TIR translation
//!
//! This module provides the context that tracks state during lowering,
//! including variable-to-register mappings and the current block.

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::backend::tir::{BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr};
use crate::common::types::IType;
use std::collections::{BTreeMap, BTreeSet};

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
    var_map: BTreeMap<String, VirtualReg>,

    /// Map from variable names to their types
    /// Used to create phi nodes with correct types (not IType::Int placeholders)
    var_type_map: BTreeMap<String, IType<'src>>,

    /// Stack of variable maps for nested scopes
    /// Used to restore variable bindings when exiting a scope
    scope_stack: Vec<ScopeSnapshot<'src>>,

    /// Whether the current binding for a variable still owns its value.
    owned_live_map: BTreeMap<String, bool>,

    /// Current nested lexical region handle, if any.
    current_region: Option<VirtualReg>,

    /// Stack of outer region handles when entering nested regions.
    region_stack: Vec<Option<VirtualReg>>,
}

struct ScopeSnapshot<'src> {
    var_map: BTreeMap<String, VirtualReg>,
    var_type_map: BTreeMap<String, IType<'src>>,
    owned_live_map: BTreeMap<String, bool>,
    declared_names: BTreeSet<String>,
}

impl<'src> LoweringContext<'src> {
    /// Create a new lowering context
    pub fn new() -> Self {
        Self {
            builder: TirBuilder::new(),
            var_map: BTreeMap::new(),
            var_type_map: BTreeMap::new(),
            scope_stack: Vec::new(),
            owned_live_map: BTreeMap::new(),
            current_region: None,
            region_stack: Vec::new(),
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

    /// Bind a variable name to an SSA register with its type
    pub fn bind_var_typed(&mut self, name: &str, reg: VirtualReg, ty: IType<'src>) {
        self.var_map.insert(name.to_string(), reg);
        self.var_type_map.insert(name.to_string(), ty);
        self.owned_live_map
            .insert(name.to_string(), is_owned_type(&self.lookup_var_type(name)));
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.declared_names.insert(name.to_string());
        }
    }

    /// Look up the current SSA register for a variable
    pub fn lookup_var(&self, name: &str) -> Option<VirtualReg> {
        self.var_map.get(name).copied()
    }

    /// Get variable name → register name substitution pairs for all bound variables
    pub fn var_substitutions(&self) -> Vec<(String, String)> {
        self.var_map
            .iter()
            .map(|(name, reg)| (name.clone(), format!("v{}", reg.0)))
            .collect()
    }

    /// Look up the type of a variable
    pub fn lookup_var_type(&self, name: &str) -> IType<'src> {
        self.var_type_map.get(name).cloned().unwrap_or(IType::Int)
    }

    pub fn is_owned_live(&self, name: &str) -> bool {
        self.owned_live_map.get(name).copied().unwrap_or(false)
    }

    pub fn mark_var_moved(&mut self, name: &str) {
        if self.var_map.contains_key(name) {
            self.owned_live_map.insert(name.to_string(), false);
        }
    }

    /// Get a snapshot of the current variable map
    /// Used for tracking which variables are modified in branches
    pub fn snapshot_var_map(&self) -> BTreeMap<String, VirtualReg> {
        self.var_map.clone()
    }

    /// Get a snapshot of the current variable type map.
    pub fn snapshot_var_type_map(&self) -> BTreeMap<String, IType<'src>> {
        self.var_type_map.clone()
    }

    /// Restore the variable map from a previous snapshot
    /// Used to reset state before lowering alternative control flow paths
    pub fn restore_var_map(&mut self, snapshot: BTreeMap<String, VirtualReg>) {
        self.var_map = snapshot;
    }

    /// Restore the variable type map from a previous snapshot.
    pub fn restore_var_type_map(&mut self, snapshot: BTreeMap<String, IType<'src>>) {
        self.var_type_map = snapshot;
    }

    /// Get all variables that differ between two snapshots
    /// Returns: Vec<(var_name, reg_in_snapshot1, reg_in_snapshot2)>
    pub fn diff_var_maps(
        &self,
        before: &BTreeMap<String, VirtualReg>,
        after: &BTreeMap<String, VirtualReg>,
    ) -> Vec<(String, VirtualReg, VirtualReg)> {
        let mut diffs = Vec::new();
        for (name, &after_reg) in after {
            if let Some(&before_reg) = before.get(name)
                && before_reg != after_reg
            {
                diffs.push((name.clone(), before_reg, after_reg));
            }
        }
        diffs
    }

    // ========================================================================
    // Scope management
    // ========================================================================

    /// Enter a new scope (pushes current var_map)
    pub fn enter_scope(&mut self) {
        self.scope_stack.push(ScopeSnapshot {
            var_map: self.var_map.clone(),
            var_type_map: self.var_type_map.clone(),
            owned_live_map: self.owned_live_map.clone(),
            declared_names: BTreeSet::new(),
        });
    }

    /// Emit drops for owned locals declared in the current scope.
    pub fn emit_scope_exit_drops(&mut self) {
        let Some(scope) = self.scope_stack.last() else {
            return;
        };
        let mut drops = Vec::new();
        for name in &scope.declared_names {
            if self.is_owned_live(name)
                && let Some(reg) = self.lookup_var(name)
            {
                let ty = self.lookup_var_type(name);
                if is_owned_type(&ty) {
                    drops.push((name.clone(), reg, ty));
                }
            }
        }

        for (name, reg, ty) in drops {
            self.emit(TirInstr::DropOwned { src: reg, ty });
            self.owned_live_map.insert(name, false);
        }
    }

    /// Exit a scope (restores previous variable state)
    pub fn exit_scope(&mut self) {
        if let Some(snapshot) = self.scope_stack.pop() {
            self.var_map = snapshot.var_map;
            self.var_type_map = snapshot.var_type_map;
            self.owned_live_map = snapshot.owned_live_map;
        }
    }

    pub fn current_region(&self) -> Option<VirtualReg> {
        self.current_region
    }

    pub fn enter_region(&mut self, region: VirtualReg) {
        self.region_stack.push(self.current_region);
        self.current_region = Some(region);
    }

    pub fn exit_region(&mut self) {
        self.current_region = self.region_stack.pop().unwrap_or(None);
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

    /// Update a phi node in a finished block by adding an incoming edge
    ///
    /// This is needed for loops where the phi node is created before
    /// the body is lowered (and thus before we know the incoming value).
    pub fn update_phi_incoming(
        &mut self,
        block_id: BlockId,
        phi_index: usize,
        pred_block: BlockId,
        incoming_reg: VirtualReg,
    ) {
        if let Some(block) = self.builder.blocks.get_mut(&block_id)
            && let Some(phi) = block.phi_nodes.get_mut(phi_index)
        {
            phi.add_incoming(pred_block, incoming_reg);
        }
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
    #[allow(clippy::too_many_arguments)]
    pub fn build_function(
        self,
        name: String,
        params: Vec<(VirtualReg, IType<'src>)>,
        param_names: Vec<String>,
        return_type: IType<'src>,
        returns_owned: bool,
        precondition: Option<Constraint>,
        postcondition: Option<Constraint>,
        entry_block: BlockId,
    ) -> TirFunction<'src> {
        self.builder.build(
            name,
            params,
            param_names,
            return_type,
            returns_owned,
            precondition,
            postcondition,
            entry_block,
        )
    }
}

fn is_owned_type<'src>(ty: &IType<'src>) -> bool {
    matches!(ty, IType::Array { .. })
}

impl<'src> Default for LoweringContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
