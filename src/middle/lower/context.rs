//! Lowering context for TAST to TIR translation
//!
//! This module provides the context that tracks state during lowering,
//! including variable-to-register mappings and the current block.

use crate::common::ownership::{BorrowKind, LifetimeId, ParameterKind};
use crate::common::types::IType;
use crate::dtal::{Constraint, VirtualReg};
use crate::middle::tir::{BlockId, PhiNode, Terminator, TirBuilder, TirFunction, TirInstr};
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

pub struct LoweringContext<'src> {
    pub builder: TirBuilder<'src>,

    var_map: BTreeMap<String, VirtualReg>,

    var_type_map: BTreeMap<String, IType<'src>>,

    scope_stack: Vec<ScopeSnapshot<'src>>,

    owned_live_map: BTreeMap<String, bool>,

    borrow_live_map: BTreeMap<String, bool>,

    scalar_borrow_map: BTreeMap<String, ScalarBorrowBinding<'src>>,

    current_region: Option<VirtualReg>,

    region_stack: Vec<Option<VirtualReg>>,
}

struct ScopeSnapshot<'src> {
    var_map: BTreeMap<String, VirtualReg>,
    var_type_map: BTreeMap<String, IType<'src>>,
    owned_live_map: BTreeMap<String, bool>,
    borrow_live_map: BTreeMap<String, bool>,
    scalar_borrow_map: BTreeMap<String, ScalarBorrowBinding<'src>>,
    declared_names: BTreeSet<String>,
}
#[derive(Clone)]

pub struct ScalarBorrowBinding<'src> {
    pub owner_name: String,
    pub cell_reg: VirtualReg,
    pub kind: BorrowKind,
    pub lifetime: Option<LifetimeId>,
    pub pointee_ty: IType<'src>,
}

impl<'src> LoweringContext<'src> {
    pub fn new() -> Self {
        Self {
            builder: TirBuilder::new(),
            var_map: BTreeMap::new(),
            var_type_map: BTreeMap::new(),
            scope_stack: Vec::new(),
            owned_live_map: BTreeMap::new(),
            borrow_live_map: BTreeMap::new(),
            scalar_borrow_map: BTreeMap::new(),
            current_region: None,
            region_stack: Vec::new(),
        }
    }

    pub fn fresh_reg(&mut self) -> VirtualReg {
        self.builder.fresh_reg()
    }

    pub fn bind_var(&mut self, name: &str, reg: VirtualReg) {
        self.var_map.insert(name.to_string(), reg);
    }

    pub fn bind_var_typed(&mut self, name: &str, reg: VirtualReg, ty: IType<'src>) {
        self.var_map.insert(name.to_string(), reg);
        self.var_type_map.insert(name.to_string(), ty);
        self.owned_live_map
            .insert(name.to_string(), is_owned_type(&self.lookup_var_type(name)));
        self.borrow_live_map.insert(
            name.to_string(),
            is_borrow_type(&self.lookup_var_type(name)),
        );
    }

    pub fn declare_var_typed(&mut self, name: &str, reg: VirtualReg, ty: IType<'src>) {
        self.bind_var_typed(name, reg, ty);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.declared_names.insert(name.to_string());
        }
    }

    pub fn lookup_var(&self, name: &str) -> Option<VirtualReg> {
        self.var_map.get(name).copied()
    }

    pub fn var_substitutions(&self) -> Vec<(String, String)> {
        self.var_map
            .iter()
            .map(|(name, reg)| (name.clone(), format!("v{}", reg.0)))
            .collect()
    }

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

    pub fn is_borrow_live(&self, name: &str) -> bool {
        self.borrow_live_map.get(name).copied().unwrap_or(false)
    }

    pub fn mark_borrow_ended(&mut self, name: &str) {
        if self.var_map.contains_key(name) {
            self.borrow_live_map.insert(name.to_string(), false);
        }
        self.scalar_borrow_map.remove(name);
    }

    pub fn bind_scalar_borrow(
        &mut self,
        name: &str,
        borrow_reg: VirtualReg,
        lowered_ref_ty: IType<'src>,
        binding: ScalarBorrowBinding<'src>,
    ) {
        self.var_map.insert(name.to_string(), borrow_reg);
        self.var_type_map.insert(name.to_string(), lowered_ref_ty);
        self.owned_live_map.insert(name.to_string(), false);
        self.borrow_live_map.insert(name.to_string(), true);
        self.scalar_borrow_map.insert(name.to_string(), binding);
    }

    pub fn declare_scalar_borrow(
        &mut self,
        name: &str,
        borrow_reg: VirtualReg,
        lowered_ref_ty: IType<'src>,
        binding: ScalarBorrowBinding<'src>,
    ) {
        self.bind_scalar_borrow(name, borrow_reg, lowered_ref_ty, binding);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.declared_names.insert(name.to_string());
        }
    }

    pub fn lookup_scalar_borrow(&self, name: &str) -> Option<&ScalarBorrowBinding<'src>> {
        self.scalar_borrow_map.get(name)
    }

    pub fn snapshot_var_map(&self) -> BTreeMap<String, VirtualReg> {
        self.var_map.clone()
    }

    pub fn snapshot_var_type_map(&self) -> BTreeMap<String, IType<'src>> {
        self.var_type_map.clone()
    }

    pub fn restore_var_map(&mut self, snapshot: BTreeMap<String, VirtualReg>) {
        self.var_map = snapshot;
    }

    pub fn restore_var_type_map(&mut self, snapshot: BTreeMap<String, IType<'src>>) {
        self.var_type_map = snapshot;
    }

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

    pub fn enter_scope(&mut self) {
        self.scope_stack.push(ScopeSnapshot {
            var_map: self.var_map.clone(),
            var_type_map: self.var_type_map.clone(),
            owned_live_map: self.owned_live_map.clone(),
            borrow_live_map: self.borrow_live_map.clone(),
            scalar_borrow_map: self.scalar_borrow_map.clone(),
            declared_names: BTreeSet::new(),
        });
    }

    pub fn emit_scope_exit_drops(&mut self) {
        let Some(scope) = self.scope_stack.last() else {
            return;
        };
        let mut drops = Vec::new();
        let mut borrow_ends = Vec::new();
        for name in &scope.declared_names {
            if self.is_owned_live(name)
                && let Some(reg) = self.lookup_var(name)
            {
                let ty = self.lookup_var_type(name);
                if is_owned_type(&ty) {
                    drops.push((name.clone(), reg, ty));
                }
            }
            if self.is_borrow_live(name) && is_borrow_type(&self.lookup_var_type(name)) {
                borrow_ends.push(name.clone());
            }
        }

        for (name, reg, ty) in drops {
            self.emit(TirInstr::DropOwned { src: reg, ty });
            self.owned_live_map.insert(name, false);
        }
        for name in borrow_ends {
            self.emit_borrow_end_for_binding(&name);
        }
    }

    pub fn exit_scope(&mut self) {
        if let Some(snapshot) = self.scope_stack.pop() {
            for name in snapshot.declared_names {
                if let Some(reg) = snapshot.var_map.get(&name).copied() {
                    self.var_map.insert(name.clone(), reg);
                } else {
                    self.var_map.remove(&name);
                }

                if let Some(ty) = snapshot.var_type_map.get(&name).cloned() {
                    self.var_type_map.insert(name.clone(), ty);
                } else {
                    self.var_type_map.remove(&name);
                }

                if let Some(live) = snapshot.owned_live_map.get(&name).copied() {
                    self.owned_live_map.insert(name.clone(), live);
                } else {
                    self.owned_live_map.remove(&name);
                }

                if let Some(live) = snapshot.borrow_live_map.get(&name).copied() {
                    self.borrow_live_map.insert(name.clone(), live);
                } else {
                    self.borrow_live_map.remove(&name);
                }

                if let Some(binding) = snapshot.scalar_borrow_map.get(&name).cloned() {
                    self.scalar_borrow_map.insert(name, binding);
                } else {
                    self.scalar_borrow_map.remove(&name);
                }
            }
        }
    }

    pub fn lowered_ref_storage_type(ty: &IType<'src>) -> IType<'src> {
        match ty {
            IType::Ref(inner) if !matches!(inner.as_ref(), IType::Array { .. }) => {
                IType::Ref(Arc::new(IType::Array {
                    element_type: inner.clone(),
                    size: crate::common::types::IValue::Int(1),
                }))
            }
            IType::RefMut(inner) if !matches!(inner.as_ref(), IType::Array { .. }) => {
                IType::RefMut(Arc::new(IType::Array {
                    element_type: inner.clone(),
                    size: crate::common::types::IValue::Int(1),
                }))
            }
            _ => ty.clone(),
        }
    }

    pub fn create_scalar_borrow_value(
        &mut self,
        owner_reg: VirtualReg,
        pointee_ty: IType<'src>,
        kind: BorrowKind,
        lifetime: Option<LifetimeId>,
    ) -> (VirtualReg, VirtualReg, IType<'src>) {
        let (cell_reg, cell_ty) = self.create_scalar_borrow_cell(owner_reg, pointee_ty.clone());
        let lowered_ref_ty = match kind {
            BorrowKind::Shared => IType::Ref(Arc::new(cell_ty.clone())),
            BorrowKind::Mutable => IType::RefMut(Arc::new(cell_ty.clone())),
        };

        let borrow_reg = self.fresh_reg();
        match kind {
            BorrowKind::Shared => self.emit(TirInstr::BorrowShared {
                lifetime,
                dst: borrow_reg,
                src: cell_reg,
                ty: lowered_ref_ty.clone(),
            }),
            BorrowKind::Mutable => self.emit(TirInstr::BorrowMut {
                lifetime,
                dst: borrow_reg,
                src: cell_reg,
                ty: lowered_ref_ty.clone(),
            }),
        }

        (borrow_reg, cell_reg, lowered_ref_ty)
    }

    pub fn create_scalar_borrow_cell(
        &mut self,
        owner_reg: VirtualReg,
        pointee_ty: IType<'src>,
    ) -> (VirtualReg, IType<'src>) {
        let cell_reg = self.fresh_reg();
        let cell_ty = IType::Array {
            element_type: Arc::new(pointee_ty.clone()),
            size: crate::common::types::IValue::Int(1),
        };
        self.emit(TirInstr::AllocArray {
            dst: cell_reg,
            element_ty: pointee_ty.clone(),
            size: 1,
            region: self.current_region(),
        });

        let zero_reg = self.fresh_reg();
        self.emit(TirInstr::LoadImm {
            dst: zero_reg,
            value: 0,
            ty: IType::Int,
        });
        self.emit(TirInstr::ArrayStore {
            base: cell_reg,
            index: zero_reg,
            value: owner_reg,
            bounds_constraint: Constraint::True,
        });

        (cell_reg, cell_ty)
    }

    pub fn sync_scalar_borrow_owner(
        &mut self,
        owner_name: &str,
        cell_reg: VirtualReg,
        pointee_ty: IType<'src>,
    ) {
        let zero_reg = self.fresh_reg();
        self.emit(TirInstr::LoadImm {
            dst: zero_reg,
            value: 0,
            ty: IType::Int,
        });
        let loaded_reg = self.fresh_reg();
        self.emit(TirInstr::ArrayLoad {
            dst: loaded_reg,
            base: cell_reg,
            index: zero_reg,
            element_ty: pointee_ty,
            bounds_constraint: Constraint::True,
        });
        let owner_ty = self.lookup_var_type(owner_name);
        self.bind_var_typed(owner_name, loaded_reg, owner_ty);
    }

    pub fn emit_borrow_end_for_binding(&mut self, name: &str) {
        self.emit_borrow_end_for_binding_with_lifetime(name, None);
    }

    pub fn emit_borrow_end_for_binding_with_lifetime(
        &mut self,
        name: &str,
        lifetime: Option<crate::common::ownership::LifetimeId>,
    ) {
        let Some(reg) = self.lookup_var(name) else {
            return;
        };
        let ty = self.lookup_var_type(name);
        let scalar_binding = self.lookup_scalar_borrow(name).cloned();
        if let Some(binding) = scalar_binding
            && binding.kind.is_mutable()
        {
            self.sync_scalar_borrow_owner(
                &binding.owner_name,
                binding.cell_reg,
                binding.pointee_ty,
            );
        }
        self.emit(TirInstr::BorrowEnd {
            lifetime,
            src: reg,
            ty,
        });
        self.borrow_live_map.insert(name.to_string(), false);
        self.scalar_borrow_map.remove(name);
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

    pub fn new_block(&mut self) -> BlockId {
        self.builder.new_block()
    }

    pub fn start_block(&mut self, id: BlockId) {
        self.builder.start_block(id);
    }

    pub fn emit(&mut self, instr: TirInstr<'src>) {
        self.builder.add_instr(instr);
    }

    pub fn emit_phi(&mut self, phi: PhiNode<'src>) {
        self.builder.add_phi(phi);
    }

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

    pub fn finish_block(&mut self, terminator: Terminator, predecessors: Vec<BlockId>) {
        self.builder.finish_block(terminator, predecessors);
    }

    pub fn is_building(&self) -> bool {
        self.builder.is_building()
    }

    pub fn current_block(&self) -> Option<BlockId> {
        self.builder.current_block_id()
    }

    #[allow(clippy::too_many_arguments)]
    pub fn build_function(
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
        self.builder.build(
            name,
            params,
            parameter_kinds,
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

fn is_borrow_type<'src>(ty: &IType<'src>) -> bool {
    matches!(ty, IType::Ref(_) | IType::RefMut(_))
}

impl<'src> Default for LoweringContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
