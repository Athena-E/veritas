//! Optimization passes for DTAL programs
//!
//! This module provides optimization passes that operate on DTAL IR
//! after codegen but before register allocation and emission.
//!
//! # Available Passes
//!
//! - **Copy Propagation**: Replaces uses of `dst` with `src` when `MovReg { dst, src }` is encountered
//! - **Dead Code Elimination**: Removes instructions whose destination registers are never used

pub mod const_fold;
pub mod copy_prop;
pub mod dce;
pub mod licm;
pub mod load_fusion;
pub mod peephole;

use crate::backend::dtal::instr::DtalProgram;

/// Configuration for optimization passes
#[derive(Clone, Debug, Default)]
pub struct OptConfig {
    /// Enable constant folding pass
    pub constant_folding: bool,
    /// Enable peephole optimisation pass
    pub peephole: bool,
    /// Enable copy propagation pass
    pub copy_propagation: bool,
    /// Enable dead code elimination pass
    pub dead_code_elimination: bool,
    /// Enable loop-invariant code motion pass
    pub licm: bool,
    /// Enable load-op fusion pass
    pub load_fusion: bool,
    /// Maximum number of iterations for the optimization loop (None = unlimited)
    pub max_iterations: Option<usize>,
}

impl OptConfig {
    /// Create config with all stable optimizations enabled by default.
    ///
    /// LICM is disabled: it produces incorrect code on non-trivial programs
    /// (constants hoisted from loop bodies interact badly with register
    /// allocation across multiple call sites).
    ///
    /// Load-fusion is disabled: it triggers non-deterministic compilation
    /// failures on larger programs (intermittent SIGSEGV in output binary
    /// even when the generated DTAL looks correct).
    ///
    /// Both can be re-enabled via `--licm` / `--load-fusion` flags.
    pub fn all() -> Self {
        Self {
            constant_folding: true,
            peephole: true,
            copy_propagation: true,
            dead_code_elimination: true,
            licm: false,
            load_fusion: false,
            max_iterations: Some(10),
        }
    }

    /// Create config with no optimizations enabled
    pub fn none() -> Self {
        Self::default()
    }

    /// Check if any optimization is enabled
    pub fn any_enabled(&self) -> bool {
        self.constant_folding
            || self.peephole
            || self.copy_propagation
            || self.dead_code_elimination
            || self.licm
            || self.load_fusion
    }
}

/// Optimize a DTAL program according to the given configuration
///
/// Runs optimization passes in a loop until no more changes are made
/// or the maximum iteration count is reached.
///
/// # Pass Ordering
///
/// 1. Constant folding (evaluates compile-time constants, folds immediates)
/// 2. Peephole (structural rewrites: algebraic identities, degenerate immediates)
/// 3. Copy propagation (exposes dead copies)
/// 4. Dead code elimination (removes useless copies and folded-away MovImms)
/// 5. LICM (hoists loop-invariant computations to before the loop)
/// 6. Load-op fusion (fuses Load + BinOp Add/Sub into LoadOp)
pub fn optimize_program(program: &mut DtalProgram, config: &OptConfig) {
    if !config.any_enabled() {
        return;
    }

    let max_iters = config.max_iterations.unwrap_or(10);

    for _ in 0..max_iters {
        let mut changed = false;

        // Run constant folding (before peephole so folded constants expose patterns)
        if config.constant_folding {
            for func in &mut program.functions {
                changed |= const_fold::constant_fold_function(func);
            }
        }

        // Run peephole (before copy prop so new MovRegs get propagated)
        if config.peephole {
            for func in &mut program.functions {
                changed |= peephole::peephole_function(func);
            }
        }

        // Run copy propagation
        if config.copy_propagation {
            for func in &mut program.functions {
                changed |= copy_prop::copy_propagate_function(func);
            }
        }

        // Run dead code elimination
        if config.dead_code_elimination {
            for func in &mut program.functions {
                changed |= dce::eliminate_dead_code(func);
            }
        }

        // Run loop-invariant code motion
        if config.licm {
            for func in &mut program.functions {
                changed |= licm::licm_function(func);
            }
        }

        // Run load-op fusion (after LICM so any hoisted loads can be fused)
        if config.load_fusion {
            for func in &mut program.functions {
                changed |= load_fusion::fuse_loads_function(func);
            }
        }

        // Fixed-point reached
        if !changed {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, TypeState};
    use crate::backend::dtal::regs::{Reg, VirtualReg};
    use crate::backend::dtal::types::DtalType;

    fn make_copy_chain_function() -> DtalFunction {
        // v0 = 42
        // v1 = v0
        // v2 = v1
        // ret (using v2)
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        DtalFunction {
            name: "copy_chain".to_string(),
            params: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovReg {
                        dst: v1,
                        src: v0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::MovReg {
                        dst: v2,
                        src: v1,
                        ty: DtalType::Int,
                    },
                    // Push v2 to use it (simulating return value setup)
                    DtalInstr::Push {
                        src: v2,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        }
    }

    #[test]
    fn test_opt_config_all() {
        let config = OptConfig::all();
        assert!(config.copy_propagation);
        assert!(config.dead_code_elimination);
        assert!(config.any_enabled());
    }

    #[test]
    fn test_opt_config_none() {
        let config = OptConfig::none();
        assert!(!config.copy_propagation);
        assert!(!config.dead_code_elimination);
        assert!(!config.any_enabled());
    }

    #[test]
    fn test_optimize_program_reduces_copies() {
        let mut program = DtalProgram {
            functions: vec![make_copy_chain_function()],
        };

        let config = OptConfig::all();
        optimize_program(&mut program, &config);

        // After optimization, the push should use v0 directly
        let func = &program.functions[0];
        let push_instr = func.blocks[0]
            .instructions
            .iter()
            .find(|i| matches!(i, DtalInstr::Push { .. }));

        if let Some(DtalInstr::Push { src, .. }) = push_instr {
            // Copy propagation should have replaced v2 with v0
            assert_eq!(*src, Reg::Virtual(VirtualReg(0)));
        }
    }
}
