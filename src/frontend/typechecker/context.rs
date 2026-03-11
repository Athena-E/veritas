use crate::common::ast::{BinOp, Expr, Literal};
use crate::common::types::{FunctionSignature, IProposition, IType, IValue};
use crate::frontend::typechecker::helpers::rename_prop_var;
use chumsky::prelude::SimpleSpan;
use im::{HashMap, Vector};
use std::sync::Arc;

// phi: Refinement propositions known to be true
// gamma: Immutable variable bindings
// delta: Mutable variable bindings with master types
// sigma_f: Global function signatures

// delta: Mutable variable bindings with master types
#[derive(Clone, Debug)]
pub struct MutableBinding<'src> {
    pub current_type: IType<'src>,
    pub master_type: IType<'src>,
}

// Result of looking up a mutable/immutable variable in delta/gamma
#[derive(Clone, Debug)]
pub enum VarBinding<'src> {
    Immutable(IType<'src>),
    Mutable(MutableBinding<'src>),
}

// Typing context
#[derive(Clone, Debug)]
pub struct TypingContext<'src> {
    // Refinement propositions
    phi: Vector<IProposition<'src>>,

    // Immutable variable bindings (name -> type)
    // let x: T = e
    gamma: HashMap<String, IType<'src>>,

    // Mutable variable bindings (name -> (current_type, master_type))
    // let mut x: T = e
    delta: HashMap<String, MutableBinding<'src>>,

    // Global function signatures (name -> signature)
    // Populated in first pass over program, immutable during type checking
    sigma_f: HashMap<String, FunctionSignature<'src>>,

    // Expected return type for current function (for checking return statements)
    expected_return: Option<IType<'src>>,

    // Postcondition for current function (for checking at return points)
    postcondition: Option<IProposition<'src>>,

    // Current function name (for error reporting)
    current_function: Option<String>,

    // Whether quantifier expressions (forall/exists) are allowed
    // True in specification contexts (invariants, requires, ensures)
    pub allow_quantifiers: bool,
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
        }
    }

    // Create context with top-level functions
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
        }
    }

    // Set expected return type for current function
    pub fn with_expected_return(&self, ty: IType<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.expected_return = Some(ty);
        new_ctx
    }

    // Get expected return type
    pub fn get_expected_return(&self) -> Option<&IType<'src>> {
        self.expected_return.as_ref()
    }

    // Set postcondition for current function
    pub fn with_postcondition(&self, postcond: IProposition<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.postcondition = Some(postcond);
        new_ctx
    }

    // Get postcondition
    pub fn get_postcondition(&self) -> Option<&IProposition<'src>> {
        self.postcondition.as_ref()
    }

    // Set current function name
    pub fn with_current_function(&self, name: String) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.current_function = Some(name);
        new_ctx
    }

    // Get current function name
    pub fn get_current_function(&self) -> Option<&String> {
        self.current_function.as_ref()
    }

    // Propositions context

    pub fn with_proposition(&self, prop: IProposition<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi.push_back(prop);
        new_ctx
    }

    pub fn get_propositions(&self) -> &Vector<IProposition<'src>> {
        &self.phi
    }

    /// Keep only propositions that satisfy the predicate.
    pub fn retain_propositions<F>(&self, predicate: F) -> Self
    where
        F: Fn(&IProposition<'src>) -> bool,
    {
        let mut new_ctx = self.clone();
        new_ctx.phi = new_ctx.phi.into_iter().filter(|p| predicate(p)).collect();
        new_ctx
    }

    // Immutable context

    pub fn with_immutable(&self, name: String, ty: IType<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.gamma.insert(name, ty);
        new_ctx
    }

    #[allow(dead_code)]
    pub fn lookup_immutable(&self, name: &str) -> Option<&IType<'src>> {
        self.gamma.get(name)
    }

    // Mutable context

    pub fn with_mutable(
        &self,
        name: String,
        current_type: IType<'src>,
        master_type: IType<'src>,
    ) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.delta.insert(
            name,
            MutableBinding {
                current_type,
                master_type,
            },
        );
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

        Ok(new_ctx)
    }

    pub fn lookup_mutable(&self, name: &str) -> Option<&MutableBinding<'src>> {
        self.delta.get(name)
    }

    #[allow(dead_code)]
    pub fn get_master_type(&self, name: &str) -> Option<&IType<'src>> {
        self.delta.get(name).map(|binding| &binding.master_type)
    }

    // Returns the type and whether it's immutable or mutable.
    pub fn lookup_var(&self, name: &str) -> Option<VarBinding<'src>> {
        // Check immutable bindings first
        if let Some(ty) = self.gamma.get(name) {
            return Some(VarBinding::Immutable(ty.clone()));
        }

        // Check mutable bindings
        if let Some(binding) = self.delta.get(name) {
            return Some(VarBinding::Mutable(binding.clone()));
        }

        None
    }

    // Function signatures

    pub fn lookup_function(&self, name: &str) -> Option<&FunctionSignature<'src>> {
        self.sigma_f.get(name)
    }

    /// Get all variables (both immutable and mutable) with their types
    /// Used by SMT solver to include refinements from variable types in the context
    pub fn get_all_variable_types(&self) -> Vec<(&str, &IType<'src>)> {
        let mut result = Vec::new();

        // Add immutable variables
        for (name, ty) in self.gamma.iter() {
            result.push((name.as_str(), ty));
        }

        // Add mutable variables (using their current type)
        for (name, binding) in self.delta.iter() {
            result.push((name.as_str(), &binding.current_type));
        }

        result
    }

    /// Join mutable contexts from two branches (after if-else)
    /// Computes the least upper bound of types for mutable variables
    ///
    /// For each mutable variable present in both contexts:
    /// - If types are equal, keep that type
    /// - Otherwise, widen to the base type (least upper bound)
    ///
    /// Propositions: Keep intersection (propositions that appear in both branches)
    pub fn join_mutable_contexts(ctx1: &Self, ctx2: &Self) -> Self {
        let mut joined = ctx1.clone();

        // Keep intersection of propositions (those provable in both branches)
        joined.phi = intersect_propositions(&ctx1.phi, &ctx2.phi);

        // Join mutable variable types
        for (name, binding1) in ctx1.delta.iter() {
            if let Some(binding2) = ctx2.delta.get(name) {
                // Variable exists in both branches - compute join of current types
                let joined_current = join_types(&binding1.current_type, &binding2.current_type);

                joined.delta.insert(
                    name.clone(),
                    MutableBinding {
                        current_type: joined_current,
                        master_type: binding1.master_type.clone(), // Master unchanged
                    },
                );
            }
            // If variable only in ctx1, it stays as-is (already cloned)
        }

        // Note: Variables only in ctx2 but not ctx1 shouldn't happen in well-scoped code
        // Both branches start from same context, so they should have same variables

        joined
    }

    /// Try to resolve an expression to a concrete i64 value.
    /// Handles literal ints and variables with singleton int types.
    pub fn resolve_expr_to_int(&self, expr: &Expr<'src>) -> Option<i64> {
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

    /// Look up the RHS of a pointwise proposition `arr_name[index] == <rhs>`.
    /// Returns the RHS expression if found, so it can be snapshotted into a let binding.
    /// Resolves both the lookup index and proposition indices to concrete values
    /// when possible, so `arr[i]` with `i: int(5)` matches a proposition about `arr[5]`.
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
                // Try concrete comparison first (handles cross-representation matches)
                if let Some(li) = lookup_idx
                    && let Some(pi) = self.resolve_expr_to_int(&index.0)
                    && li == pi
                {
                    return Some(rhs.0.clone());
                }
                // Fall back to structural equality for non-resolvable expressions
                if lookup_idx.is_none() && format!("{:?}", index.0) == format!("{:?}", index_expr) {
                    return Some(rhs.0.clone());
                }
            }
        }
        None
    }

    /// Remove ALL propositions about arr_name elements — both pointwise
    /// `arr[k] == v` and quantified `forall i { ... arr[i] ... }`.
    /// Used when a symbolic index assignment invalidates all element knowledge.
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

    /// Remove the pointwise proposition for arr_name[idx_val] == ... and any
    /// quantified propositions over arr_name (since modifying one element
    /// invalidates a universal claim).
    pub fn without_array_element_prop(&self, arr_name: &str, idx_val: i64) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi = new_ctx
            .phi
            .into_iter()
            .filter(|prop| {
                if prop.var != arr_name {
                    return true;
                }
                match &prop.predicate.0 {
                    // Remove quantified propositions over this array
                    Expr::Forall { .. } | Expr::Exists { .. } => {
                        !expr_references_array_index(&prop.predicate.0, arr_name)
                    }
                    // Remove matching pointwise proposition
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

/// Check if an expression contains an array index reference `arr_name[...]`
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

/// Compute the join (least upper bound) of two types
/// Used after if-else to merge mutable variable types from both branches
fn join_types<'src>(t1: &IType<'src>, t2: &IType<'src>) -> IType<'src> {
    match (t1, t2) {
        // Same types - keep as is
        (IType::Unit, IType::Unit) => IType::Unit,
        (IType::Int, IType::Int) => IType::Int,
        (IType::Bool, IType::Bool) => IType::Bool,

        // Same singleton - keep singleton
        (IType::SingletonInt(v1), IType::SingletonInt(v2)) if v1 == v2 => {
            IType::SingletonInt(v1.clone())
        }

        // Different singletons - widen to int
        (IType::SingletonInt(_), IType::SingletonInt(_)) => IType::Int,

        // Singleton and int - widen to int
        (IType::SingletonInt(_), IType::Int) | (IType::Int, IType::SingletonInt(_)) => IType::Int,

        // Refined types - compute disjunction of predicates
        (IType::RefinedInt { base: b1, prop: p1 }, IType::RefinedInt { prop: p2, .. }) => {
            let disjoined = disjoin_propositions(p1, p2);
            IType::RefinedInt {
                base: b1.clone(),
                prop: disjoined,
            }
        }

        // Refined + Singleton: promote singleton to refined, then join
        (IType::RefinedInt { base, prop }, IType::SingletonInt(v))
        | (IType::SingletonInt(v), IType::RefinedInt { base, prop }) => {
            let singleton_prop = singleton_to_proposition(v);
            let disjoined = disjoin_propositions(prop, &singleton_prop);
            IType::RefinedInt {
                base: base.clone(),
                prop: disjoined,
            }
        }

        // Refined + Int: widen to int (can't disjoin with unconstrained)
        (IType::RefinedInt { .. }, IType::Int) | (IType::Int, IType::RefinedInt { .. }) => {
            IType::Int
        }

        // Arrays with same structure
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

        // References - must be exactly equal, otherwise error
        // For now, just return t1 (caller should ensure compatibility)
        (IType::Ref(_), IType::Ref(_)) => t1.clone(),
        (IType::RefMut(_), IType::RefMut(_)) => t1.clone(),

        // Master types - unwrap and join
        (IType::Master(inner1), IType::Master(inner2)) => {
            IType::Master(std::sync::Arc::new(join_types(inner1, inner2)))
        }

        // Fallback - return first type (should not happen in well-typed code)
        _ => t1.clone(),
    }
}

/// Compute the disjunction of two propositions with the same bound variable
fn disjoin_propositions<'src>(
    p1: &IProposition<'src>,
    p2: &IProposition<'src>,
) -> IProposition<'src> {
    // Rename p2's variable to match p1 if different
    let p2_renamed = if p1.var != p2.var {
        rename_prop_var(p2, &p2.var, &p1.var)
    } else {
        p2.clone()
    };

    let dummy_span = SimpleSpan::new(0, 0);

    // Create disjunctive expression: p1.predicate OR p2_renamed.predicate
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

/// Convert a singleton type int(n) to a proposition {v: int | v == n}
fn singleton_to_proposition<'src>(value: &IValue) -> IProposition<'src> {
    let var = "v".to_string();
    let dummy_span = SimpleSpan::new(0, 0);

    let value_literal = match value {
        IValue::Int(n) => Expr::Literal(Literal::Int(*n)),
        IValue::Symbolic(s) => {
            let leaked: &'src str = Box::leak(s.clone().into_boxed_str());
            Expr::Variable(leaked)
        }
        IValue::Bool(b) => Expr::Literal(Literal::Bool(*b)),
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

/// Keep propositions that appear in both contexts
/// Uses simple structural equality; could use SMT for semantic equality
fn intersect_propositions<'src>(
    phi1: &Vector<IProposition<'src>>,
    phi2: &Vector<IProposition<'src>>,
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
            // Try to form a disjunction with a matching array element prop
            if let Some(p1_parts) = extract_array_eq_parts(p1) {
                for (i2, p2) in phi2.iter().enumerate() {
                    if matched_in_phi2.contains(&i2) {
                        continue;
                    }
                    if let Some(p2_parts) = extract_array_eq_parts(p2) {
                        // Same array, same index → disjoin the equalities
                        if p1_parts.arr_name == p2_parts.arr_name
                            && format!("{:?}", p1_parts.index) == format!("{:?}", p2_parts.index)
                        {
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

/// Parts of an array element equality proposition: arr[index] == rhs
struct ArrayEqParts<'a, 'src> {
    arr_name: &'a str,
    index: &'a Expr<'src>,
}

/// Extract arr_name and index from a proposition of the form `arr[index] == rhs`
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

/// Check if two propositions are structurally equivalent
fn propositions_equivalent<'src>(p1: &IProposition<'src>, p2: &IProposition<'src>) -> bool {
    // Simple structural equality for now
    // Could be enhanced with SMT equivalence checking
    p1.var == p2.var && format!("{:?}", p1.predicate.0) == format!("{:?}", p2.predicate.0)
}

impl<'src> Default for TypingContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
