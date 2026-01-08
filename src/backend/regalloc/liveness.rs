//! Liveness Analysis
//!
//! This module computes liveness information for virtual registers in DTAL programs.
//! Liveness analysis determines which registers are "live" (may be used later) at each
//! program point, which is essential for register allocation.
//!
//! # Algorithm
//!
//! We use backward dataflow analysis:
//! - live_in[B] = USE[B] ∪ (live_out[B] - DEF[B])
//! - live_out[B] = ∪ live_in[S] for all successors S of B

use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use std::collections::{HashMap, HashSet};

/// Liveness information for a single basic block
#[derive(Clone, Debug, Default)]
pub struct BlockLiveness {
    /// Registers live at block entry
    pub live_in: HashSet<VirtualReg>,
    /// Registers live at block exit
    pub live_out: HashSet<VirtualReg>,
    /// Registers defined in this block
    pub defs: HashSet<VirtualReg>,
    /// Registers used in this block (before any local def)
    pub uses: HashSet<VirtualReg>,
}

/// Liveness information for a function
#[derive(Clone, Debug)]
pub struct LivenessInfo {
    /// Per-block liveness
    pub blocks: HashMap<String, BlockLiveness>,
    /// Control flow graph: block -> successors
    pub successors: HashMap<String, Vec<String>>,
    /// Control flow graph: block -> predecessors
    pub predecessors: HashMap<String, Vec<String>>,
}

/// Liveness analysis engine
pub struct LivenessAnalysis;

impl LivenessAnalysis {
    /// Analyze liveness for a function
    pub fn analyze(func: &DtalFunction) -> LivenessInfo {
        // Build CFG
        let (successors, predecessors) = Self::build_cfg(func);

        // Compute local USE and DEF sets for each block
        let mut blocks: HashMap<String, BlockLiveness> = HashMap::new();
        for block in &func.blocks {
            let (uses, defs) = Self::compute_use_def(block);
            blocks.insert(
                block.label.clone(),
                BlockLiveness {
                    live_in: HashSet::new(),
                    live_out: HashSet::new(),
                    defs,
                    uses,
                },
            );
        }

        // Fixed-point iteration for liveness
        let mut changed = true;
        while changed {
            changed = false;

            // Process blocks in reverse order (more efficient for backward analysis)
            for block in func.blocks.iter().rev() {
                let label = &block.label;

                // Compute live_out = union of live_in of all successors
                let mut new_live_out = HashSet::new();
                if let Some(succs) = successors.get(label) {
                    for succ in succs {
                        if let Some(succ_info) = blocks.get(succ) {
                            new_live_out.extend(succ_info.live_in.iter().cloned());
                        }
                    }
                }

                // Compute live_in = uses ∪ (live_out - defs)
                let block_info = blocks.get(label).unwrap();
                let mut new_live_in = block_info.uses.clone();
                for reg in &new_live_out {
                    if !block_info.defs.contains(reg) {
                        new_live_in.insert(*reg);
                    }
                }

                // Check for changes
                let block_info = blocks.get_mut(label).unwrap();
                if new_live_in != block_info.live_in || new_live_out != block_info.live_out {
                    changed = true;
                    block_info.live_in = new_live_in;
                    block_info.live_out = new_live_out;
                }
            }
        }

        LivenessInfo {
            blocks,
            successors,
            predecessors,
        }
    }

    /// Build control flow graph from function
    fn build_cfg(
        func: &DtalFunction,
    ) -> (HashMap<String, Vec<String>>, HashMap<String, Vec<String>>) {
        let mut successors: HashMap<String, Vec<String>> = HashMap::new();
        let mut predecessors: HashMap<String, Vec<String>> = HashMap::new();

        // Initialize maps
        for block in &func.blocks {
            successors.insert(block.label.clone(), Vec::new());
            predecessors.insert(block.label.clone(), Vec::new());
        }

        // Build edges based on terminators
        for (i, block) in func.blocks.iter().enumerate() {
            let succs = Self::get_block_successors(block, func, i);
            for succ in &succs {
                predecessors
                    .get_mut(succ)
                    .unwrap()
                    .push(block.label.clone());
            }
            successors.insert(block.label.clone(), succs);
        }

        (successors, predecessors)
    }

    /// Get successors of a block based on its terminator instruction
    fn get_block_successors(
        block: &DtalBlock,
        func: &DtalFunction,
        block_idx: usize,
    ) -> Vec<String> {
        let mut succs = Vec::new();

        // Find terminator instruction
        if let Some(last) = block.instructions.last() {
            match last {
                DtalInstr::Jmp { target } => {
                    succs.push(target.clone());
                }
                DtalInstr::Branch { target, .. } => {
                    // Conditional branch: fall through + target
                    succs.push(target.clone());
                    // Fall through to next block
                    if block_idx + 1 < func.blocks.len() {
                        succs.push(func.blocks[block_idx + 1].label.clone());
                    }
                }
                DtalInstr::Ret | DtalInstr::Call { .. } => {
                    // Ret has no successors (within function)
                    // Call may return to next instruction, but for now treat as end
                    // For calls, fall through to next block
                    if !matches!(last, DtalInstr::Ret) && block_idx + 1 < func.blocks.len() {
                        succs.push(func.blocks[block_idx + 1].label.clone());
                    }
                }
                _ => {
                    // Non-terminator at end: fall through
                    if block_idx + 1 < func.blocks.len() {
                        succs.push(func.blocks[block_idx + 1].label.clone());
                    }
                }
            }
        } else if block_idx + 1 < func.blocks.len() {
            // Empty block: fall through
            succs.push(func.blocks[block_idx + 1].label.clone());
        }

        succs
    }

    /// Compute USE and DEF sets for a block
    fn compute_use_def(block: &DtalBlock) -> (HashSet<VirtualReg>, HashSet<VirtualReg>) {
        let mut uses = HashSet::new();
        let mut defs = HashSet::new();

        for instr in &block.instructions {
            // Get uses before defs (order matters for local liveness)
            for reg in Self::instruction_uses(instr) {
                if !defs.contains(&reg) {
                    uses.insert(reg);
                }
            }

            // Get defs
            if let Some(reg) = Self::instruction_def(instr) {
                defs.insert(reg);
            }
        }

        (uses, defs)
    }

    /// Get the register defined by an instruction (if any)
    fn instruction_def(instr: &DtalInstr) -> Option<VirtualReg> {
        let reg = match instr {
            DtalInstr::MovImm { dst, .. } => Some(*dst),
            DtalInstr::MovReg { dst, .. } => Some(*dst),
            DtalInstr::Load { dst, .. } => Some(*dst),
            DtalInstr::BinOp { dst, .. } => Some(*dst),
            DtalInstr::AddImm { dst, .. } => Some(*dst),
            DtalInstr::Not { dst, .. } => Some(*dst),
            DtalInstr::Pop { dst, .. } => Some(*dst),
            DtalInstr::Alloca { dst, .. } => Some(*dst),
            DtalInstr::SetCC { dst, .. } => Some(*dst),
            _ => None,
        };

        // Only return virtual registers
        reg.and_then(|r| match r {
            Reg::Virtual(v) => Some(v),
            Reg::Physical(_) => None,
        })
    }

    /// Get the registers used by an instruction
    fn instruction_uses(instr: &DtalInstr) -> Vec<VirtualReg> {
        let regs: Vec<Reg> = match instr {
            DtalInstr::MovReg { src, .. } => vec![*src],
            DtalInstr::Load { base, offset, .. } => vec![*base, *offset],
            DtalInstr::Store { base, offset, src } => vec![*base, *offset, *src],
            DtalInstr::BinOp { lhs, rhs, .. } => vec![*lhs, *rhs],
            DtalInstr::AddImm { src, .. } => vec![*src],
            DtalInstr::Cmp { lhs, rhs } => vec![*lhs, *rhs],
            DtalInstr::CmpImm { lhs, .. } => vec![*lhs],
            DtalInstr::Not { src, .. } => vec![*src],
            DtalInstr::Push { src, .. } => vec![*src],
            _ => vec![],
        };

        // Only return virtual registers
        regs.into_iter()
            .filter_map(|r| match r {
                Reg::Virtual(v) => Some(v),
                Reg::Physical(_) => None,
            })
            .collect()
    }

    /// Compute per-instruction liveness (more detailed than block-level)
    pub fn compute_instruction_liveness(
        block: &DtalBlock,
        live_out: &HashSet<VirtualReg>,
    ) -> Vec<HashSet<VirtualReg>> {
        let n = block.instructions.len();
        if n == 0 {
            return vec![];
        }

        // live[i] = registers live AFTER instruction i
        let mut live: Vec<HashSet<VirtualReg>> = vec![HashSet::new(); n];
        live[n - 1] = live_out.clone();

        // Work backward
        for i in (0..n).rev() {
            let instr = &block.instructions[i];

            // live_before = (live_after - def) ∪ uses
            let mut live_before = if i == n - 1 {
                live_out.clone()
            } else {
                live[i].clone()
            };

            // Remove def
            if let Some(def) = Self::instruction_def(instr) {
                live_before.remove(&def);
            }

            // Add uses
            for reg in Self::instruction_uses(instr) {
                live_before.insert(reg);
            }

            // Store live_before as live_after of previous instruction
            if i > 0 {
                live[i - 1] = live_before;
            }
        }

        live
    }
}

/// Build an interference graph from liveness information
#[derive(Debug, Default)]
pub struct InterferenceGraph {
    /// Adjacency list: reg -> set of interfering regs
    pub edges: HashMap<VirtualReg, HashSet<VirtualReg>>,
    /// All registers in the graph
    pub nodes: HashSet<VirtualReg>,
}

impl InterferenceGraph {
    /// Build interference graph from a function's liveness info
    pub fn build(func: &DtalFunction, liveness: &LivenessInfo) -> Self {
        let mut graph = InterferenceGraph::default();

        for block in &func.blocks {
            let block_info = &liveness.blocks[&block.label];

            // Get per-instruction liveness
            let live_sets =
                LivenessAnalysis::compute_instruction_liveness(block, &block_info.live_out);

            // Add interference edges for each instruction point
            for live_set in &live_sets {
                let regs: Vec<_> = live_set.iter().copied().collect();
                for i in 0..regs.len() {
                    for j in (i + 1)..regs.len() {
                        graph.add_edge(regs[i], regs[j]);
                    }
                }
            }
        }

        graph
    }

    /// Add an edge between two registers
    pub fn add_edge(&mut self, a: VirtualReg, b: VirtualReg) {
        self.nodes.insert(a);
        self.nodes.insert(b);
        self.edges.entry(a).or_default().insert(b);
        self.edges.entry(b).or_default().insert(a);
    }

    /// Get neighbors of a register
    pub fn neighbors(&self, reg: VirtualReg) -> impl Iterator<Item = VirtualReg> + '_ {
        self.edges
            .get(&reg)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }

    /// Get the degree (number of neighbors) of a register
    pub fn degree(&self, reg: VirtualReg) -> usize {
        self.edges.get(&reg).map_or(0, |s| s.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{BinaryOp, TypeState};
    use crate::common::types::IType;

    fn make_test_function() -> DtalFunction<'static> {
        // Simple function: result = a + b
        // Block 0:
        //   v0 = 10        ; a
        //   v1 = 20        ; b
        //   v2 = v0 + v1   ; result
        //   ret
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions: vec![
                    DtalInstr::MovImm {
                        dst: v0,
                        imm: 10,
                        ty: IType::Int,
                    },
                    DtalInstr::MovImm {
                        dst: v1,
                        imm: 20,
                        ty: IType::Int,
                    },
                    DtalInstr::BinOp {
                        op: BinaryOp::Add,
                        dst: v2,
                        lhs: v0,
                        rhs: v1,
                        ty: IType::Int,
                    },
                    DtalInstr::Ret,
                ],
            }],
        }
    }

    #[test]
    fn test_liveness_simple() {
        let func = make_test_function();
        let liveness = LivenessAnalysis::analyze(&func);

        // Entry block should exist
        assert!(liveness.blocks.contains_key("entry"));

        let entry = &liveness.blocks["entry"];

        // v0 and v1 are defined and used locally
        // v2 is defined at the end and not used
        assert!(entry.defs.contains(&VirtualReg(0)));
        assert!(entry.defs.contains(&VirtualReg(1)));
        assert!(entry.defs.contains(&VirtualReg(2)));

        // v0 and v1 are used in the add instruction
        // But they're defined before use, so not in uses set
        // uses should be empty since all uses are after local defs
    }

    #[test]
    fn test_instruction_liveness() {
        let func = make_test_function();
        let liveness = LivenessAnalysis::analyze(&func);
        let block = &func.blocks[0];
        let block_info = &liveness.blocks["entry"];

        let live_sets = LivenessAnalysis::compute_instruction_liveness(block, &block_info.live_out);

        // After v0 = 10: v0 is live
        // After v1 = 20: v0, v1 are live
        // After v2 = v0 + v1: v2 is live (but not used, so maybe not)
        // After ret: nothing live

        assert_eq!(live_sets.len(), 4);
    }

    #[test]
    fn test_interference_graph() {
        let func = make_test_function();
        let liveness = LivenessAnalysis::analyze(&func);
        let graph = InterferenceGraph::build(&func, &liveness);

        // v0 and v1 should interfere (both live at the add instruction)
        let v0 = VirtualReg(0);
        let v1 = VirtualReg(1);

        if graph.nodes.contains(&v0) && graph.nodes.contains(&v1) {
            // Check if they interfere
            let v0_neighbors: HashSet<_> = graph.neighbors(v0).collect();
            let v1_neighbors: HashSet<_> = graph.neighbors(v1).collect();

            // v0 and v1 are both live when v2 = v0 + v1 is computed
            // So they should interfere
            assert!(
                v0_neighbors.contains(&v1) || v1_neighbors.contains(&v0),
                "v0 and v1 should interfere"
            );
        }
    }

    fn make_branching_function() -> DtalFunction<'static> {
        // Function with control flow:
        // entry:
        //   v0 = 1
        //   branch eq, then
        // else:
        //   v1 = 2
        //   jmp exit
        // then:
        //   v1 = 3
        //   jmp exit
        // exit:
        //   v2 = v0 + v1
        //   ret
        use crate::backend::dtal::instr::CmpOp;

        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        DtalFunction {
            name: "branching".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            blocks: vec![
                DtalBlock {
                    label: "entry".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v0,
                            imm: 1,
                            ty: IType::Int,
                        },
                        DtalInstr::Branch {
                            cond: CmpOp::Eq,
                            target: "then".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "else".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v1,
                            imm: 2,
                            ty: IType::Int,
                        },
                        DtalInstr::Jmp {
                            target: "exit".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "then".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: v1,
                            imm: 3,
                            ty: IType::Int,
                        },
                        DtalInstr::Jmp {
                            target: "exit".to_string(),
                        },
                    ],
                },
                DtalBlock {
                    label: "exit".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::BinOp {
                            op: BinaryOp::Add,
                            dst: v2,
                            lhs: v0,
                            rhs: v1,
                            ty: IType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                },
            ],
        }
    }

    #[test]
    fn test_cfg_construction() {
        let func = make_branching_function();
        let liveness = LivenessAnalysis::analyze(&func);

        // Check successors
        let entry_succs = &liveness.successors["entry"];
        assert!(entry_succs.contains(&"then".to_string()));
        assert!(entry_succs.contains(&"else".to_string()));

        let else_succs = &liveness.successors["else"];
        assert!(else_succs.contains(&"exit".to_string()));

        let then_succs = &liveness.successors["then"];
        assert!(then_succs.contains(&"exit".to_string()));

        // Check predecessors
        let exit_preds = &liveness.predecessors["exit"];
        assert!(exit_preds.contains(&"else".to_string()));
        assert!(exit_preds.contains(&"then".to_string()));
    }

    #[test]
    fn test_liveness_across_branches() {
        let func = make_branching_function();
        let liveness = LivenessAnalysis::analyze(&func);

        // v0 is defined in entry and used in exit
        // So v0 should be live at exit of entry, and live at entry of else/then/exit
        let v0 = VirtualReg(0);

        // v0 should be live out of entry
        assert!(
            liveness.blocks["entry"].live_out.contains(&v0),
            "v0 should be live out of entry"
        );

        // v0 should be live in of exit (used there)
        assert!(
            liveness.blocks["exit"].live_in.contains(&v0),
            "v0 should be live in of exit"
        );
    }
}
