use crate::common::ast::{BinOp, Expr, Literal};
use crate::common::ownership::{BorrowKind, LifetimeId};
use crate::common::types::{FunctionSignature, IProposition, IType, IValue};
use crate::frontend::typechecker::helpers::rename_prop_var;
use chumsky::prelude::SimpleSpan;
use im::{HashMap, Vector};
use std::collections::HashSet;
use std::sync::Arc;
#[derive(Clone, Debug)]

pub struct MutableBinding<'src> {
    pub current_type: IType<'src>,
    pub master_type: IType<'src>,
}
#[derive(Clone, Debug)]

pub enum VarBinding<'src> {
    Immutable(IType<'src>),
    Mutable(MutableBinding<'src>),
}
#[derive(Clone, Debug, Default)]

struct BorrowState {
    shared: usize,
    mutable: bool,
}
#[derive(Clone, Debug)]

struct BorrowBinding {
    owner: String,
    kind: BorrowKind,
    lifetime: LifetimeId,
}
#[derive(Clone, Debug)]

pub struct TypingContext<'src> {
    phi: Vector<IProposition<'src>>,

    gamma: HashMap<String, IType<'src>>,

    delta: HashMap<String, MutableBinding<'src>>,

    sigma_f: HashMap<String, FunctionSignature<'src>>,

    expected_return: Option<IType<'src>>,

    postcondition: Option<IProposition<'src>>,

    current_function: Option<String>,

    pub allow_quantifiers: bool,

    pub bare_metal: bool,

    pub region_depth: usize,

    region_local_arrays: Vec<HashSet<String>>,

    region_scoped_arrays: Vec<HashSet<String>>,

    region_scoped_borrows: Vec<HashSet<String>>,

    moved_bindings: HashSet<String>,

    active_borrows: HashMap<String, BorrowState>,

    borrow_bindings: HashMap<String, BorrowBinding>,

    borrow_scopes: Vec<Vec<String>>,

    next_lifetime_id: u32,
}

impl<'src> TypingContext<'src> {
    pub fn new() -> Self {
        Self {
            phi: Vector::new(),
            gamma: HashMap::new(),
            delta: HashMap::new(),
            sigma_f: HashMap::new(),
            expected_return: None,
            postcondition: None,
            current_function: None,
            allow_quantifiers: false,
            bare_metal: false,
            region_depth: 0,
            region_local_arrays: Vec::new(),
            region_scoped_arrays: Vec::new(),
            region_scoped_borrows: Vec::new(),
            moved_bindings: HashSet::new(),
            active_borrows: HashMap::new(),
            borrow_bindings: HashMap::new(),
            borrow_scopes: Vec::new(),
            next_lifetime_id: 0,
        }
    }

    pub fn with_functions(functions: HashMap<String, FunctionSignature<'src>>) -> Self {
        Self {
            phi: Vector::new(),
            gamma: HashMap::new(),
            delta: HashMap::new(),
            sigma_f: functions,
            expected_return: None,
            postcondition: None,
            current_function: None,
            allow_quantifiers: false,
            bare_metal: false,
            region_depth: 0,
            region_local_arrays: Vec::new(),
            region_scoped_arrays: Vec::new(),
            region_scoped_borrows: Vec::new(),
            moved_bindings: HashSet::new(),
            active_borrows: HashMap::new(),
            borrow_bindings: HashMap::new(),
            borrow_scopes: Vec::new(),
            next_lifetime_id: 0,
        }
    }

    pub fn enter_borrow_scope(&self) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.borrow_scopes.push(Vec::new());
        new_ctx
    }

    pub fn exit_borrow_scope(&self) -> Self {
        let mut new_ctx = self.clone();
        if let Some(bindings) = new_ctx.borrow_scopes.pop() {
            for binding in bindings {
                new_ctx = new_ctx.release_borrow_binding(&binding);
            }
        }
        new_ctx
    }

    pub fn enter_region_scope(&self) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.region_depth += 1;
        new_ctx.region_local_arrays.push(HashSet::new());
        new_ctx.region_scoped_arrays.push(HashSet::new());
        new_ctx.region_scoped_borrows.push(HashSet::new());
        new_ctx
    }

    pub fn in_region_scope(&self) -> bool {
        self.region_depth > 0
    }

    pub fn merge_region_exit(&self, region_ctx: &Self) -> Self {
        let mut merged = self.clone();
        for name in self.delta.keys() {
            if let Some(updated_binding) = region_ctx.delta.get(name) {
                merged.delta.insert(name.clone(), updated_binding.clone());
            }
        }

        merged.region_local_arrays = self.region_local_arrays.clone();
        merged.region_scoped_arrays = self.region_scoped_arrays.clone();
        merged.region_scoped_borrows = self.region_scoped_borrows.clone();
        merged.moved_bindings = region_ctx.moved_bindings.clone();
        merged.active_borrows = self.active_borrows.clone();
        merged.borrow_bindings = self.borrow_bindings.clone();
        merged.borrow_scopes = self.borrow_scopes.clone();
        merged
    }

    pub fn mark_region_local_array(&self, name: &str) -> Self {
        let mut new_ctx = self.clone();
        if let Some(scope) = new_ctx.region_local_arrays.last_mut() {
            scope.insert(name.to_string());
        }
        new_ctx
    }

    pub fn clear_region_local_array(&self, name: &str) -> Self {
        let mut new_ctx = self.clone();
        for scope in new_ctx.region_local_arrays.iter_mut().rev() {
            if scope.remove(name) {
                break;
            }
        }
        new_ctx
    }

    pub fn mark_region_scoped_array(&self, name: &str) -> Self {
        let mut new_ctx = self.clone();
        if let Some(scope) = new_ctx.region_scoped_arrays.last_mut() {
            scope.insert(name.to_string());
        }
        new_ctx
    }

    pub fn is_region_local_array(&self, name: &str) -> bool {
        self.region_local_arrays
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    pub fn is_region_scoped_array(&self, name: &str) -> bool {
        self.region_scoped_arrays
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    pub fn mark_region_scoped_borrow(&self, name: &str) -> Self {
        let mut new_ctx = self.clone();
        if let Some(scope) = new_ctx.region_scoped_borrows.last_mut() {
            scope.insert(name.to_string());
        }
        new_ctx
    }

    pub fn is_region_scoped_borrow(&self, name: &str) -> bool {
        self.region_scoped_borrows
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    pub fn with_expected_return(&self, ty: IType<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.expected_return = Some(ty);
        new_ctx
    }

    pub fn get_expected_return(&self) -> Option<&IType<'src>> {
        self.expected_return.as_ref()
    }

    pub fn with_postcondition(&self, postcond: IProposition<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.postcondition = Some(postcond);
        new_ctx
    }

    pub fn get_postcondition(&self) -> Option<&IProposition<'src>> {
        self.postcondition.as_ref()
    }

    pub fn with_current_function(&self, name: String) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.current_function = Some(name);
        new_ctx
    }

    pub fn get_current_function(&self) -> Option<&String> {
        self.current_function.as_ref()
    }

    pub fn with_proposition(&self, prop: IProposition<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi.push_back(prop);
        new_ctx
    }

    pub fn get_propositions(&self) -> &Vector<IProposition<'src>> {
        &self.phi
    }

    pub fn retain_propositions<F>(&self, predicate: F) -> Self
    where
        F: Fn(&IProposition<'src>) -> bool,
    {
        let mut new_ctx = self.clone();
        new_ctx.phi = new_ctx.phi.into_iter().filter(|p| predicate(p)).collect();
        new_ctx
    }

    pub fn with_immutable(&self, name: String, ty: IType<'src>) -> Self {
        let mut new_ctx = self.release_borrow_binding(&name);
        new_ctx.gamma.insert(name.clone(), ty);
        new_ctx.moved_bindings.remove(&name);
        new_ctx
    }
    #[allow(dead_code)]

    pub fn lookup_immutable(&self, name: &str) -> Option<&IType<'src>> {
        if self.moved_bindings.contains(name) {
            return None;
        }
        self.gamma.get(name)
    }

    pub fn with_mutable(
        &self,
        name: String,
        current_type: IType<'src>,
        master_type: IType<'src>,
    ) -> Self {
        let mut new_ctx = self.release_borrow_binding(&name);
        new_ctx.delta.insert(
            name.clone(),
            MutableBinding {
                current_type,
                master_type,
            },
        );
        new_ctx.moved_bindings.remove(&name);
        new_ctx
    }

    pub fn with_mutable_update(&self, name: &str, new_type: IType<'src>) -> Result<Self, String> {
        let binding = self
            .delta
            .get(name)
            .ok_or_else(|| format!("Variable '{}' not found in mutable context", name))?;

        let mut new_ctx = self.clone();
        new_ctx.delta.insert(
            name.to_string(),
            MutableBinding {
                current_type: new_type,
                master_type: binding.master_type.clone(),
            },
        );
        new_ctx.moved_bindings.remove(name);

        Ok(new_ctx)
    }

    pub fn lookup_mutable(&self, name: &str) -> Option<&MutableBinding<'src>> {
        self.delta.get(name)
    }
    #[allow(dead_code)]

    pub fn get_master_type(&self, name: &str) -> Option<&IType<'src>> {
        self.delta.get(name).map(|binding| &binding.master_type)
    }

    pub fn lookup_var(&self, name: &str) -> Option<VarBinding<'src>> {
        if self.moved_bindings.contains(name) {
            return None;
        }
        if let Some(ty) = self.gamma.get(name) {
            return Some(VarBinding::Immutable(ty.clone()));
        }

        if let Some(binding) = self.delta.get(name) {
            return Some(VarBinding::Mutable(binding.clone()));
        }

        None
    }

    pub fn mark_moved(&self, name: &str) -> Self {
        let mut new_ctx = self.clone();
        if new_ctx.gamma.contains_key(name) || new_ctx.delta.contains_key(name) {
            new_ctx.moved_bindings.insert(name.to_string());
        }
        new_ctx
    }

    pub fn is_moved(&self, name: &str) -> bool {
        self.moved_bindings.contains(name)
    }

    pub fn add_borrow_binding(
        &self,
        binding_name: &str,
        owner_name: &str,
        kind: BorrowKind,
    ) -> Self {
        let mut new_ctx = self.release_borrow_binding(binding_name);
        let lifetime = LifetimeId(new_ctx.next_lifetime_id);
        new_ctx.next_lifetime_id += 1;
        let mut state = new_ctx
            .active_borrows
            .get(owner_name)
            .cloned()
            .unwrap_or_default();
        match kind {
            BorrowKind::Shared => state.shared += 1,
            BorrowKind::Mutable => state.mutable = true,
        }
        new_ctx.active_borrows.insert(owner_name.to_string(), state);
        new_ctx.borrow_bindings.insert(
            binding_name.to_string(),
            BorrowBinding {
                owner: owner_name.to_string(),
                kind,
                lifetime,
            },
        );
        if let Some(scope) = new_ctx.borrow_scopes.last_mut() {
            scope.push(binding_name.to_string());
        }
        new_ctx
    }

    pub fn release_borrow_binding(&self, binding_name: &str) -> Self {
        let mut new_ctx = self.clone();
        let Some(binding) = new_ctx.borrow_bindings.remove(binding_name) else {
            return new_ctx;
        };
        if let Some(state) = new_ctx.active_borrows.get_mut(&binding.owner) {
            match binding.kind {
                BorrowKind::Shared => {
                    state.shared = state.shared.saturating_sub(1);
                }
                BorrowKind::Mutable => {
                    state.mutable = false;
                }
            }
            if state.shared == 0 && !state.mutable {
                new_ctx.active_borrows.remove(&binding.owner);
            }
        }
        for scope in &mut new_ctx.borrow_scopes {
            scope.retain(|name| name != binding_name);
        }
        new_ctx
    }

    pub fn lookup_borrow_binding(&self, name: &str) -> Option<(&str, BorrowKind)> {
        self.borrow_bindings
            .get(name)
            .map(|binding| (binding.owner.as_str(), binding.kind))
    }

    pub fn lookup_borrow_lifetime(&self, name: &str) -> Option<LifetimeId> {
        self.borrow_bindings
            .get(name)
            .map(|binding| binding.lifetime)
    }

    pub fn live_borrow_binding_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self.borrow_bindings.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn has_shared_borrows(&self, owner: &str) -> bool {
        self.active_borrows
            .get(owner)
            .is_some_and(|state| state.shared > 0)
    }

    pub fn has_mutable_borrow(&self, owner: &str) -> bool {
        self.active_borrows
            .get(owner)
            .is_some_and(|state| state.mutable)
    }

    pub fn is_borrowed(&self, owner: &str) -> bool {
        self.has_shared_borrows(owner) || self.has_mutable_borrow(owner)
    }

    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSignature<'src>> {
        self.sigma_f.get(name)
    }

    pub fn get_all_variable_types(&self) -> Vec<(&str, &IType<'src>)> {
        let mut result = Vec::new();

        for (name, ty) in self.gamma.iter() {
            result.push((name.as_str(), ty));
        }

        for (name, binding) in self.delta.iter() {
            result.push((name.as_str(), &binding.current_type));
        }

        result
    }

    pub fn join_mutable_contexts(ctx1: &Self, ctx2: &Self) -> Self {
        Self::join_mutable_contexts_with_base(ctx1, ctx2, None)
    }

    pub fn join_mutable_contexts_with_base(
        ctx1: &Self,
        ctx2: &Self,
        base_ctx: Option<&Self>,
    ) -> Self {
        let mut joined = ctx1.clone();

        joined.phi = intersect_propositions(&ctx1.phi, &ctx2.phi, base_ctx);

        for (name, binding1) in ctx1.delta.iter() {
            if let Some(binding2) = ctx2.delta.get(name) {
                let joined_current = join_types(&binding1.current_type, &binding2.current_type);

                joined.delta.insert(
                    name.clone(),
                    MutableBinding {
                        current_type: joined_current,
                        master_type: binding1.master_type.clone(),
                    },
                );
            }
        }

        joined.region_local_arrays = ctx1.region_local_arrays.clone();
        for (scope_idx, scope) in ctx2.region_local_arrays.iter().enumerate() {
            if let Some(joined_scope) = joined.region_local_arrays.get_mut(scope_idx) {
                joined_scope.extend(scope.iter().cloned());
            } else {
                joined.region_local_arrays.push(scope.clone());
            }
        }

        joined.region_scoped_arrays = ctx1.region_scoped_arrays.clone();
        for (scope_idx, scope) in ctx2.region_scoped_arrays.iter().enumerate() {
            if let Some(joined_scope) = joined.region_scoped_arrays.get_mut(scope_idx) {
                joined_scope.extend(scope.iter().cloned());
            } else {
                joined.region_scoped_arrays.push(scope.clone());
            }
        }

        joined.region_scoped_borrows = ctx1.region_scoped_borrows.clone();
        for (scope_idx, scope) in ctx2.region_scoped_borrows.iter().enumerate() {
            if let Some(joined_scope) = joined.region_scoped_borrows.get_mut(scope_idx) {
                joined_scope.extend(scope.iter().cloned());
            } else {
                joined.region_scoped_borrows.push(scope.clone());
            }
        }

        joined
            .moved_bindings
            .extend(ctx2.moved_bindings.iter().cloned());

        joined
    }

    pub fn resolve_expr_to_int(&self, expr: &Expr<'src>) -> Option<i128> {
        match expr {
            Expr::Literal(Literal::Int(n)) => Some(*n),
            Expr::Variable(name) => match self.lookup_var(name) {
                Some(VarBinding::Immutable(IType::SingletonInt(IValue::Int(n)))) => Some(n),
                Some(VarBinding::Mutable(ref b)) => match &b.current_type {
                    IType::SingletonInt(IValue::Int(n)) => Some(*n),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub fn resolve_array_element_value<'a>(
        &'a self,
        arr_name: &str,
        index_expr: &Expr<'src>,
    ) -> Option<Expr<'src>> {
        let lookup_idx = self.resolve_expr_to_int(index_expr);

        for prop in self.phi.iter() {
            if prop.var != arr_name {
                continue;
            }
            if let Expr::BinOp {
                op: BinOp::Eq,
                lhs,
                rhs,
            } = &prop.predicate.0
                && let Expr::Index { base, index } = &lhs.0
                && matches!(&base.0, Expr::Variable(n) if *n == arr_name)
            {
                if let Some(li) = lookup_idx
                    && let Some(pi) = self.resolve_expr_to_int(&index.0)
                    && li == pi
                {
                    return Some(rhs.0.clone());
                }
                if lookup_idx.is_none() && format!("{:?}", index.0) == format!("{:?}", index_expr) {
                    return Some(rhs.0.clone());
                }
            }
        }
        None
    }

    pub fn without_all_array_element_props(&self, arr_name: &str) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi = new_ctx
            .phi
            .into_iter()
            .filter(|prop| {
                if prop.var != arr_name {
                    return true;
                }
                !expr_references_array_index(&prop.predicate.0, arr_name)
            })
            .collect();
        new_ctx
    }

    pub fn without_array_element_prop(&self, arr_name: &str, idx_val: i128) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi = new_ctx
            .phi
            .into_iter()
            .filter(|prop| {
                if prop.var != arr_name {
                    return true;
                }
                match &prop.predicate.0 {
                    Expr::Forall { .. } | Expr::Exists { .. } => {
                        !expr_references_array_index(&prop.predicate.0, arr_name)
                    }
                    Expr::BinOp {
                        op: BinOp::Eq, lhs, ..
                    } => match &lhs.0 {
                        Expr::Index { base, index } => {
                            let base_matches =
                                matches!(&base.0, Expr::Variable(n) if *n == arr_name);
                            let idx_matches =
                                matches!(&index.0, Expr::Literal(Literal::Int(v)) if *v == idx_val);
                            !(base_matches && idx_matches)
                        }
                        _ => true,
                    },
                    _ => true,
                }
            })
            .collect();
        new_ctx
    }
}

fn expr_references_array_index(expr: &Expr, arr_name: &str) -> bool {
    match expr {
        Expr::Index { base, index } => {
            matches!(&base.0, Expr::Variable(n) if *n == arr_name)
                || expr_references_array_index(&index.0, arr_name)
        }
        Expr::BinOp { lhs, rhs, .. } => {
            expr_references_array_index(&lhs.0, arr_name)
                || expr_references_array_index(&rhs.0, arr_name)
        }
        Expr::UnaryOp { cond, .. } => expr_references_array_index(&cond.0, arr_name),
        Expr::Forall {
            start, end, body, ..
        }
        | Expr::Exists {
            start, end, body, ..
        } => {
            expr_references_array_index(&start.0, arr_name)
                || expr_references_array_index(&end.0, arr_name)
                || expr_references_array_index(&body.0, arr_name)
        }
        _ => false,
    }
}

pub(crate) fn join_types<'src>(t1: &IType<'src>, t2: &IType<'src>) -> IType<'src> {
    match (t1, t2) {
        (IType::Unit, IType::Unit) => IType::Unit,
        (IType::Int, IType::Int) => IType::Int,
        (IType::Bool, IType::Bool) => IType::Bool,

        (IType::SingletonInt(v1), IType::SingletonInt(v2)) if v1 == v2 => {
            IType::SingletonInt(v1.clone())
        }

        (IType::SingletonInt(IValue::Int(_)), IType::SingletonInt(IValue::Int(_))) => {
            let prop1 = singleton_to_proposition(t1);
            let prop2 = singleton_to_proposition(t2);
            IType::RefinedInt {
                base: std::sync::Arc::new(IType::Int),
                prop: disjoin_propositions(&prop1, &prop2),
            }
        }
        (IType::SingletonInt(IValue::Bool(_)), IType::SingletonInt(IValue::Bool(_))) => IType::Bool,

        (IType::SingletonInt(_), IType::Int) | (IType::Int, IType::SingletonInt(_)) => IType::Int,

        (IType::RefinedInt { base: b1, prop: p1 }, IType::RefinedInt { prop: p2, .. }) => {
            let disjoined = disjoin_propositions(p1, p2);
            IType::RefinedInt {
                base: b1.clone(),
                prop: disjoined,
            }
        }

        (IType::RefinedInt { base, prop }, IType::SingletonInt(_))
        | (IType::SingletonInt(_), IType::RefinedInt { base, prop }) => {
            let singleton_prop = singleton_to_proposition(match (t1, t2) {
                (IType::SingletonInt(_), _) => t1,
                _ => t2,
            });
            let disjoined = disjoin_propositions(prop, &singleton_prop);
            IType::RefinedInt {
                base: base.clone(),
                prop: disjoined,
            }
        }

        (IType::RefinedInt { .. }, IType::Int) | (IType::Int, IType::RefinedInt { .. }) => {
            IType::Int
        }

        (
            IType::Array {
                element_type: e1,
                size: s1,
            },
            IType::Array {
                element_type: e2,
                size: s2,
            },
        ) if s1 == s2 => IType::Array {
            element_type: std::sync::Arc::new(join_types(e1, e2)),
            size: s1.clone(),
        },

        (IType::Ref(_), IType::Ref(_)) => t1.clone(),
        (IType::RefMut(_), IType::RefMut(_)) => t1.clone(),

        (IType::Master(inner1), IType::Master(inner2)) => {
            IType::Master(std::sync::Arc::new(join_types(inner1, inner2)))
        }

        _ => t1.clone(),
    }
}

fn disjoin_propositions<'src>(
    p1: &IProposition<'src>,
    p2: &IProposition<'src>,
) -> IProposition<'src> {
    let p2_renamed = if p1.var != p2.var {
        rename_prop_var(p2, &p2.var, &p1.var)
    } else {
        p2.clone()
    };

    let dummy_span = SimpleSpan::new(0, 0);

    let disjoined_expr = Expr::BinOp {
        op: BinOp::Or,
        lhs: Box::new((p1.predicate.0.clone(), dummy_span)),
        rhs: Box::new((p2_renamed.predicate.0.clone(), dummy_span)),
    };

    IProposition {
        var: p1.var.clone(),
        predicate: Arc::new((disjoined_expr, dummy_span)),
    }
}

fn singleton_to_proposition<'src>(ty: &IType<'src>) -> IProposition<'src> {
    let var = "v".to_string();
    let dummy_span = SimpleSpan::new(0, 0);

    let value_literal = match ty {
        IType::SingletonInt(IValue::Int(n)) => Expr::Literal(Literal::Int(*n)),
        IType::SingletonInt(IValue::Symbolic(s)) => {
            let leaked: &'src str = Box::leak(s.clone().into_boxed_str());
            Expr::Variable(leaked)
        }
        IType::SingletonInt(IValue::Bool(b)) => Expr::Literal(Literal::Bool(*b)),
        _ => unreachable!("singleton_to_proposition called with non-singleton type"),
    };

    let eq_expr = Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((
            {
                let leaked: &'src str = Box::leak(var.clone().into_boxed_str());
                Expr::Variable(leaked)
            },
            dummy_span,
        )),
        rhs: Box::new((value_literal, dummy_span)),
    };

    IProposition {
        var,
        predicate: Arc::new((eq_expr, dummy_span)),
    }
}

fn intersect_propositions<'src>(
    phi1: &Vector<IProposition<'src>>,
    phi2: &Vector<IProposition<'src>>,
    base_ctx: Option<&TypingContext<'src>>,
) -> Vector<IProposition<'src>> {
    let mut result = Vector::new();
    let mut matched_in_phi2 = std::collections::HashSet::new();

    for p1 in phi1.iter() {
        let mut found_exact = false;
        for (i2, p2) in phi2.iter().enumerate() {
            if propositions_equivalent(p1, p2) {
                result.push_back(p1.clone());
                matched_in_phi2.insert(i2);
                found_exact = true;
                break;
            }
        }
        if !found_exact {
            if let Some(p1_parts) = extract_array_eq_parts(p1) {
                for (i2, p2) in phi2.iter().enumerate() {
                    if matched_in_phi2.contains(&i2) {
                        continue;
                    }
                    if let Some(p2_parts) = extract_array_eq_parts(p2) {
                        let indices_match = if let Some(ctx) = base_ctx
                            && let Some(i1) = ctx.resolve_expr_to_int(p1_parts.index)
                            && let Some(i2) = ctx.resolve_expr_to_int(p2_parts.index)
                        {
                            i1 == i2
                        } else {
                            format!("{:?}", p1_parts.index) == format!("{:?}", p2_parts.index)
                        };

                        if p1_parts.arr_name == p2_parts.arr_name && indices_match {
                            let disj_expr = Expr::BinOp {
                                op: BinOp::Or,
                                lhs: Box::new(p1.predicate.as_ref().clone()),
                                rhs: Box::new(p2.predicate.as_ref().clone()),
                            };
                            let dummy_span = SimpleSpan::new(0, 0);
                            result.push_back(IProposition {
                                var: p1.var.clone(),
                                predicate: Arc::new((disj_expr, dummy_span)),
                            });
                            matched_in_phi2.insert(i2);
                            break;
                        }
                    }
                }
            }
        }
    }
    result
}

struct ArrayEqParts<'a, 'src> {
    arr_name: &'a str,
    index: &'a Expr<'src>,
}

fn extract_array_eq_parts<'a, 'src>(
    prop: &'a IProposition<'src>,
) -> Option<ArrayEqParts<'a, 'src>> {
    if let Expr::BinOp {
        op: BinOp::Eq, lhs, ..
    } = &prop.predicate.0
        && let Expr::Index { base, index } = &lhs.0
        && let Expr::Variable(name) = &base.0
    {
        return Some(ArrayEqParts {
            arr_name: name,
            index: &index.0,
        });
    }
    None
}

fn propositions_equivalent<'src>(p1: &IProposition<'src>, p2: &IProposition<'src>) -> bool {
    p1.var == p2.var && format!("{:?}", p1.predicate.0) == format!("{:?}", p2.predicate.0)
}

impl<'src> Default for TypingContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
