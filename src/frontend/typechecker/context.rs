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
}

impl<'src> TypingContext<'src> {

    pub fn new() -> Self {
        Self {
            phi: Vector::new(),
            gamma: HashMap::new(),
            delta: HashMap::new(),
            sigma_f: HashMap::new(),
        }
    }

    // Create context with top-level functions
    pub fn with_functions(functions: HashMap<String, FunctionSignature<'src>>) -> Self {
        Self {
            phi: Vector::new(),
            gamma: HashMap::new(),
            delta: HashMap::new(),
            sigma_f: functions,
        }
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

    pub fn with_mutable_update(
        &self,
        name: &str,
        new_type: IType<'src>,
    ) -> Result<Self, String> {
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

    /// Join mutable contexts from two branches
    /// TODO: implement proper joining logic:
    #[allow(unused_variables)]
    pub fn join_mutable_contexts(ctx1: &Self, ctx2: &Self) -> Self {
        // TODO: Implement proper context joining
        ctx1.clone()
    }
}

impl<'src> Default for TypingContext<'src> {
    fn default() -> Self {
        Self::new()
    }
}
