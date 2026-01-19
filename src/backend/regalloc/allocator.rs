//! Register Allocator
//!
//! This module implements a linear scan register allocator for mapping DTAL
//! virtual registers to x86-64 physical registers.
//!
//! # Algorithm
//!
//! Linear scan allocation:
//! 1. Compute live intervals for each virtual register
//! 2. Sort intervals by start position
//! 3. For each interval, try to find a free physical register
//! 4. If no free register, spill the register with furthest next use

use super::liveness::{InterferenceGraph, LivenessAnalysis, LivenessInfo};
use crate::backend::dtal::instr::{DtalFunction, DtalInstr};
use crate::backend::dtal::regs::{Reg, VirtualReg};
use crate::backend::x86_64::regs::{Location, X86Reg};
use std::collections::{HashMap, HashSet};

/// A live interval for a virtual register
#[derive(Clone, Debug)]
pub struct LiveInterval {
    /// The virtual register
    pub vreg: VirtualReg,
    /// Start position (instruction index)
    pub start: usize,
    /// End position (instruction index)
    pub end: usize,
    /// All use positions within the interval
    pub uses: Vec<usize>,
}

/// Result of register allocation
#[derive(Clone, Debug)]
pub struct AllocationResult {
    /// Mapping from virtual registers to locations (register or stack)
    pub allocation: HashMap<VirtualReg, Location>,
    /// Number of stack slots needed for spills
    pub spill_slots: usize,
    /// Registers that need to be saved/restored (callee-saved)
    pub callee_saved_used: Vec<X86Reg>,
}

/// Linear scan register allocator
pub struct LinearScanAllocator {
    /// Available physical registers for allocation
    available_regs: Vec<X86Reg>,
    /// Currently active intervals (sorted by end point)
    active: Vec<(LiveInterval, X86Reg)>,
    /// Free registers pool
    free_regs: Vec<X86Reg>,
    /// Allocation result
    allocation: HashMap<VirtualReg, Location>,
    /// Next spill slot
    next_spill_slot: i32,
    /// Set of callee-saved registers actually used
    callee_saved_used: HashSet<X86Reg>,
}

impl LinearScanAllocator {
    /// Create a new allocator with default x86-64 registers
    pub fn new() -> Self {
        Self {
            available_regs: X86Reg::ALLOCATABLE.to_vec(),
            active: Vec::new(),
            free_regs: X86Reg::ALLOCATABLE.to_vec(),
            allocation: HashMap::new(),
            next_spill_slot: -8, // Start at [rbp-8]
            callee_saved_used: HashSet::new(),
        }
    }

    /// Allocate registers for a function
    pub fn allocate(&mut self, func: &DtalFunction) -> AllocationResult {
        // Reset state
        self.active.clear();
        self.free_regs = self.available_regs.clone();
        self.allocation.clear();
        self.next_spill_slot = -8;
        self.callee_saved_used.clear();

        // Compute liveness
        let liveness = LivenessAnalysis::analyze(func);

        // Build live intervals
        let mut intervals = self.compute_live_intervals(func, &liveness);

        // Sort intervals by start position
        intervals.sort_by_key(|i| i.start);

        // Process each interval
        for interval in intervals {
            // Expire old intervals
            self.expire_old_intervals(interval.start);

            if self.free_regs.is_empty() {
                // Need to spill
                self.spill_at_interval(&interval);
            } else {
                // Allocate a free register
                let reg = self.free_regs.pop().unwrap();
                self.allocation.insert(interval.vreg, Location::Reg(reg));
                self.active.push((interval, reg));
                self.active.sort_by_key(|(i, _)| i.end);

                // Track callee-saved usage
                if reg.is_callee_saved() {
                    self.callee_saved_used.insert(reg);
                }
            }
        }

        AllocationResult {
            allocation: self.allocation.clone(),
            spill_slots: ((-self.next_spill_slot - 8) / 8) as usize,
            callee_saved_used: self.callee_saved_used.iter().copied().collect(),
        }
    }

    /// Compute live intervals from liveness information
    fn compute_live_intervals(
        &self,
        func: &DtalFunction,
        liveness: &LivenessInfo,
    ) -> Vec<LiveInterval> {
        // Track first def and last use for each register
        let mut intervals: HashMap<VirtualReg, LiveInterval> = HashMap::new();

        let mut position = 0usize;

        for block in &func.blocks {
            let block_info = &liveness.blocks[&block.label];

            // Registers live at block entry extend their intervals
            for &vreg in &block_info.live_in {
                intervals.entry(vreg).or_insert(LiveInterval {
                    vreg,
                    start: position,
                    end: position,
                    uses: Vec::new(),
                });
            }

            for instr in &block.instructions {
                // Handle definitions
                if let Some(vreg) = Self::get_def(instr) {
                    let interval = intervals.entry(vreg).or_insert(LiveInterval {
                        vreg,
                        start: position,
                        end: position,
                        uses: Vec::new(),
                    });
                    interval.start = interval.start.min(position);
                }

                // Handle uses
                for vreg in Self::get_uses(instr) {
                    let interval = intervals.entry(vreg).or_insert(LiveInterval {
                        vreg,
                        start: position,
                        end: position,
                        uses: Vec::new(),
                    });
                    interval.end = interval.end.max(position);
                    interval.uses.push(position);
                }

                position += 1;
            }

            // Registers live at block exit extend their intervals
            for &vreg in &block_info.live_out {
                if let Some(interval) = intervals.get_mut(&vreg) {
                    interval.end = interval.end.max(position.saturating_sub(1));
                }
            }
        }

        intervals.into_values().collect()
    }

    /// Expire intervals that end before the current position
    fn expire_old_intervals(&mut self, position: usize) {
        // Find intervals that have ended
        let (expired, still_active): (Vec<_>, Vec<_>) = self
            .active
            .drain(..)
            .partition(|(interval, _)| interval.end < position);

        // Return expired registers to free pool
        for (_, reg) in expired {
            self.free_regs.push(reg);
        }

        self.active = still_active;
    }

    /// Spill a register to make room for a new interval
    fn spill_at_interval(&mut self, interval: &LiveInterval) {
        if let Some(last_idx) = self.active.iter().position(|(i, _)| i.end > interval.end) {
            // Spill the interval with furthest endpoint
            let (spilled_interval, reg) = self.active.remove(last_idx);

            // Allocate stack slot for spilled register
            let slot = self.next_spill_slot;
            self.next_spill_slot -= 8;
            self.allocation
                .insert(spilled_interval.vreg, Location::Stack(slot));

            // Give the freed register to the new interval
            self.allocation.insert(interval.vreg, Location::Reg(reg));
            self.active.push((interval.clone(), reg));
            self.active.sort_by_key(|(i, _)| i.end);

            if reg.is_callee_saved() {
                self.callee_saved_used.insert(reg);
            }
        } else {
            // Spill the new interval instead
            let slot = self.next_spill_slot;
            self.next_spill_slot -= 8;
            self.allocation.insert(interval.vreg, Location::Stack(slot));
        }
    }

    /// Get the virtual register defined by an instruction
    fn get_def(instr: &DtalInstr) -> Option<VirtualReg> {
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

        reg.and_then(|r| match r {
            Reg::Virtual(v) => Some(v),
            Reg::Physical(_) => None,
        })
    }

    /// Get the virtual registers used by an instruction
    fn get_uses(instr: &DtalInstr) -> Vec<VirtualReg> {
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

        regs.into_iter()
            .filter_map(|r| match r {
                Reg::Virtual(v) => Some(v),
                Reg::Physical(_) => None,
            })
            .collect()
    }
}

impl Default for LinearScanAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// Graph coloring register allocator (alternative to linear scan)
/// Uses the interference graph to find a valid k-coloring
pub struct GraphColoringAllocator {
    /// Number of available registers
    num_regs: usize,
    /// Available physical registers
    available_regs: Vec<X86Reg>,
}

impl GraphColoringAllocator {
    pub fn new() -> Self {
        Self {
            num_regs: X86Reg::ALLOCATABLE.len(),
            available_regs: X86Reg::ALLOCATABLE.to_vec(),
        }
    }

    /// Allocate registers using graph coloring
    pub fn allocate(&self, func: &DtalFunction) -> AllocationResult {
        let liveness = LivenessAnalysis::analyze(func);
        let graph = InterferenceGraph::build(func, &liveness);

        // Collect all virtual registers from the function
        let all_vregs: HashSet<VirtualReg> = Self::collect_all_vregs(func);

        // Simplify: repeatedly remove nodes with degree < k
        let mut stack: Vec<VirtualReg> = Vec::new();
        let mut removed: HashSet<VirtualReg> = HashSet::new();
        let mut current_degree: HashMap<VirtualReg, usize> = HashMap::new();

        // Initialize degrees (registers not in graph have degree 0)
        for &vreg in &all_vregs {
            current_degree.insert(vreg, graph.degree(vreg));
        }

        // Simplify phase
        while removed.len() < all_vregs.len() {
            // Find a node with degree < k
            let candidate = current_degree
                .iter()
                .filter(|(node, _)| !removed.contains(node))
                .find(|&(_, deg)| *deg < self.num_regs)
                .map(|(node, _)| *node);

            if let Some(node) = candidate {
                // Remove this node
                stack.push(node);
                removed.insert(node);

                // Decrease degree of neighbors
                for neighbor in graph.neighbors(node) {
                    if !removed.contains(&neighbor) {
                        *current_degree.get_mut(&neighbor).unwrap() -= 1;
                    }
                }
            } else {
                // No low-degree node found - need to spill
                // Choose node with highest degree
                let spill_candidate = current_degree
                    .iter()
                    .filter(|(node, _)| !removed.contains(node))
                    .max_by_key(|&(_, deg)| *deg)
                    .map(|(node, _)| *node);

                if let Some(node) = spill_candidate {
                    stack.push(node);
                    removed.insert(node);

                    for neighbor in graph.neighbors(node) {
                        if !removed.contains(&neighbor)
                            && let Some(deg) = current_degree.get_mut(&neighbor)
                        {
                            *deg = deg.saturating_sub(1);
                        }
                    }
                }
            }
        }

        // Select phase: assign colors by popping from stack
        let mut allocation: HashMap<VirtualReg, Location> = HashMap::new();
        let mut callee_saved_used: HashSet<X86Reg> = HashSet::new();
        let mut next_spill_slot: i32 = -8;

        while let Some(node) = stack.pop() {
            // Find colors used by neighbors
            let neighbor_colors: HashSet<X86Reg> = graph
                .neighbors(node)
                .filter_map(|n| {
                    allocation.get(&n).and_then(|loc| match loc {
                        Location::Reg(r) => Some(*r),
                        Location::Stack(_) => None,
                    })
                })
                .collect();

            // Find an available color
            let available_color = self
                .available_regs
                .iter()
                .find(|r| !neighbor_colors.contains(r));

            if let Some(&reg) = available_color {
                allocation.insert(node, Location::Reg(reg));
                if reg.is_callee_saved() {
                    callee_saved_used.insert(reg);
                }
            } else {
                // Must spill
                allocation.insert(node, Location::Stack(next_spill_slot));
                next_spill_slot -= 8;
            }
        }

        AllocationResult {
            allocation,
            spill_slots: ((-next_spill_slot - 8) / 8) as usize,
            callee_saved_used: callee_saved_used.into_iter().collect(),
        }
    }

    /// Collect all virtual registers used in a function
    fn collect_all_vregs(func: &DtalFunction) -> HashSet<VirtualReg> {
        let mut vregs = HashSet::new();

        for block in &func.blocks {
            for instr in &block.instructions {
                // Collect defs
                if let Some(vreg) = Self::get_def(instr) {
                    vregs.insert(vreg);
                }
                // Collect uses
                for vreg in Self::get_uses(instr) {
                    vregs.insert(vreg);
                }
            }
        }

        vregs
    }

    /// Get the virtual register defined by an instruction
    fn get_def(instr: &DtalInstr) -> Option<VirtualReg> {
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

        reg.and_then(|r| match r {
            Reg::Virtual(v) => Some(v),
            Reg::Physical(_) => None,
        })
    }

    /// Get the virtual registers used by an instruction
    fn get_uses(instr: &DtalInstr) -> Vec<VirtualReg> {
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

        regs.into_iter()
            .filter_map(|r| match r {
                Reg::Virtual(v) => Some(v),
                Reg::Physical(_) => None,
            })
            .collect()
    }
}

impl Default for GraphColoringAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::dtal::instr::{BinaryOp, DtalBlock, TypeState};
    use crate::common::types::IType;

    fn make_test_function() -> DtalFunction<'static> {
        let v0 = Reg::Virtual(VirtualReg(0));
        let v1 = Reg::Virtual(VirtualReg(1));
        let v2 = Reg::Virtual(VirtualReg(2));

        DtalFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
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
    fn test_linear_scan_basic() {
        let func = make_test_function();
        let mut allocator = LinearScanAllocator::new();
        let result = allocator.allocate(&func);

        // All three virtual registers should be allocated
        assert!(result.allocation.contains_key(&VirtualReg(0)));
        assert!(result.allocation.contains_key(&VirtualReg(1)));
        assert!(result.allocation.contains_key(&VirtualReg(2)));

        // With only 3 registers and 14 available, no spills needed
        assert_eq!(result.spill_slots, 0);
    }

    #[test]
    fn test_graph_coloring_basic() {
        let func = make_test_function();
        let allocator = GraphColoringAllocator::new();
        let result = allocator.allocate(&func);

        // All three virtual registers should be allocated
        assert!(result.allocation.contains_key(&VirtualReg(0)));
        assert!(result.allocation.contains_key(&VirtualReg(1)));
        assert!(result.allocation.contains_key(&VirtualReg(2)));

        // No spills needed for simple case
        assert_eq!(result.spill_slots, 0);
    }

    #[test]
    fn test_interference_respected() {
        let func = make_test_function();
        let allocator = GraphColoringAllocator::new();
        let result = allocator.allocate(&func);

        // v0 and v1 interfere (both live at add instruction)
        // They should get different registers
        let loc0 = &result.allocation[&VirtualReg(0)];
        let loc1 = &result.allocation[&VirtualReg(1)];

        match (loc0, loc1) {
            (Location::Reg(r0), Location::Reg(r1)) => {
                assert_ne!(r0, r1, "v0 and v1 should have different registers");
            }
            _ => {
                // If either is spilled, that's also valid
            }
        }
    }

    fn make_high_pressure_function() -> DtalFunction<'static> {
        // Create a function that needs more registers than available
        // to test spilling
        let mut instructions = Vec::new();

        // Define 20 virtual registers (more than the 14 allocatable)
        for i in 0..20 {
            instructions.push(DtalInstr::MovImm {
                dst: Reg::Virtual(VirtualReg(i)),
                imm: i as i64,
                ty: IType::Int,
            });
        }

        // Use all of them to keep them live
        for i in 0..19 {
            instructions.push(DtalInstr::BinOp {
                op: BinaryOp::Add,
                dst: Reg::Virtual(VirtualReg(20 + i)),
                lhs: Reg::Virtual(VirtualReg(i)),
                rhs: Reg::Virtual(VirtualReg(i + 1)),
                ty: IType::Int,
            });
        }

        instructions.push(DtalInstr::Ret);

        DtalFunction {
            name: "high_pressure".to_string(),
            params: vec![],
            return_type: IType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![DtalBlock {
                label: "entry".to_string(),
                entry_state: TypeState::new(),
                instructions,
            }],
        }
    }

    #[test]
    fn test_spilling() {
        let func = make_high_pressure_function();
        let mut allocator = LinearScanAllocator::new();
        let result = allocator.allocate(&func);

        // Should have some spills since we use more virtual regs than physical
        // Not asserting exact count as it depends on allocation order
        println!(
            "Allocated {} registers, {} spills",
            result.allocation.len(),
            result.spill_slots
        );

        // All virtual registers should have an allocation
        for i in 0..39 {
            assert!(
                result.allocation.contains_key(&VirtualReg(i)),
                "v{} should be allocated",
                i
            );
        }
    }
}
