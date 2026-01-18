use crate::common::types::{FunctionSignature, IProposition, IType};
use im::{HashMap, Vector};

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
}

impl<'src> TypingContext<'src> {
    pub fn new() -> Self {
        Self {
            phi: Vector::new(),
            gamma: HashMap::new(),
            delta: HashMap::new(),
            sigma_f: HashMap::new(),
            expected_return: None,
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

    // Propositions context

    pub fn with_proposition(&self, prop: IProposition<'src>) -> Self {
        let mut new_ctx = self.clone();
        new_ctx.phi.push_back(prop);
        new_ctx
    }

    pub fn get_propositions(&self) -> &Vector<IProposition<'src>> {
        &self.phi
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

    /// Join mutable contexts from two branches (after if-else)
    /// Computes the least upper bound of types for mutable variables
    ///
    /// For each mutable variable present in both contexts:
    /// - If types are equal, keep that type
    /// - Otherwise, widen to the base type (least upper bound)
    ///
    /// Propositions are cleared since we can't assume either branch's props
    pub fn join_mutable_contexts(ctx1: &Self, ctx2: &Self) -> Self {
        let mut joined = ctx1.clone();

        // Clear propositions - after branch merge, we can't assume either's props
        joined.phi = Vector::new();

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

        // Refined types - conservatively widen to int
        // (A proper implementation would compute the disjunction of predicates)
        (IType::RefinedInt { .. }, _) | (_, IType::RefinedInt { .. }) => IType::Int,

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

impl<'src> Default for TypingContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
