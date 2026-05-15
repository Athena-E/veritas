//! Optimization passes for DTAL programs
//!
//! This module provides optimization passes that operate on DTAL IR
//! after codegen but before register allocation and emission.
//!
//! # Available Passes
//!
//! - **Copy Propagation**: Replaces uses of `dst` with `src` when `MovReg { dst, src }` is encountered
//! - **Dead Code Elimination**: Removes instructions whose destination registers are never used
//! - **Loop-Invariant Code Motion**: Hoists pure loop-invariant instructions into a unique preheader
//! - **Load-Op Fusion**: Fuses single-use load plus add into a memory-operand add

pub mod const_fold;
pub mod copy_prop;
pub mod dce;
pub mod licm;
pub mod load_fusion;
pub mod peephole;

use crate::dtal::instr::DtalProgram;

#[derive(Clone, Debug, Default)]
pub struct OptConfig {
    pub constant_folding: bool,
    pub peephole: bool,
    pub copy_propagation: bool,
    pub dead_code_elimination: bool,
    pub licm: bool,
    pub load_fusion: bool,
    pub max_iterations: Option<usize>,
}

impl OptConfig {
    pub fn all() -> Self {
        Self {
            constant_folding: true,
            peephole: true,
            copy_propagation: true,
            dead_code_elimination: true,
            licm: true,
            load_fusion: true,
            max_iterations: Some(10),
        }
    }

    pub fn none() -> Self {
        Self::default()
    }

    pub fn any_enabled(&self) -> bool {
        self.constant_folding
            || self.peephole
            || self.copy_propagation
            || self.dead_code_elimination
            || self.licm
            || self.load_fusion
    }
}

pub fn optimize_program(program: &mut DtalProgram, config: &OptConfig) {
    if !config.any_enabled() {
        return;
    }

    let max_iters = config.max_iterations.unwrap_or(10);

    for _ in 0..max_iters {
        let mut changed = false;

        if config.constant_folding {
            for func in &mut program.functions {
                changed |= const_fold::constant_fold_function(func);
            }
        }

        if config.peephole {
            for func in &mut program.functions {
                changed |= peephole::peephole_function(func);
            }
        }

        if config.copy_propagation {
            for func in &mut program.functions {
                changed |= copy_prop::copy_propagate_function(func);
            }
        }

        if config.dead_code_elimination {
            for func in &mut program.functions {
                changed |= dce::eliminate_dead_code(func);
            }
        }

        if config.licm {
            for func in &mut program.functions {
                changed |= licm::licm_function(func);
            }
        }

        if config.load_fusion {
            for func in &mut program.functions {
                changed |= load_fusion::fuse_loads_function(func);
            }
        }

        if !changed {
            break;
        }
    }
}
#[cfg(test)]

mod tests {
    use super::*;
    use crate::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, TypeState};
    use crate::dtal::regs::{Reg, VirtualReg};
    use crate::dtal::types::DtalType;

    fn make_copy_chain_function() -> DtalFunction {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        DtalFunction {
            name: "copy_chain".to_string(),
            params: vec![],
            parameter_kinds: vec![],
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
        assert!(config.constant_folding);
        assert!(config.peephole);
        assert!(config.copy_propagation);
        assert!(config.dead_code_elimination);
        assert!(config.licm);
        assert!(config.load_fusion);
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

        let func = &program.functions[0];
        let push_instr = func.blocks[0]
            .instructions
            .iter()
            .find(|i| matches!(i, DtalInstr::Push { .. }));

        if let Some(DtalInstr::Push { src, .. }) = push_instr {
            assert_eq!(*src, Reg::Virtual(VirtualReg(0)));
        }
    }
}
