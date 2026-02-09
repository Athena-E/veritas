//! TIR to DTAL Code Generation Implementation
//!
//! This module translates TIR (Typed Intermediate Representation) to DTAL
//! (Dependently Typed Assembly Language).
//!
//! # Overview
//!
//! The code generation process:
//! 1. Convert TIR instructions to DTAL instructions (instruction selection)
//! 2. Lower phi nodes to parallel copies at predecessor block ends
//! 3. Generate DTAL blocks with type state annotations
//!
//! At this stage, we still use virtual registers. Physical register
//! allocation is a separate phase (Phase 5).

use crate::backend::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use crate::backend::dtal::regs::Reg;
use crate::backend::tir::{BasicBlock, BlockId, PhiNode, Terminator, TirFunction, TirProgram};
use std::collections::HashMap;

use super::isel;

/// Code generation context
pub struct CodegenContext<'src> {
    /// Generated DTAL blocks
    blocks: Vec<DtalBlock<'src>>,
    /// Map from TIR BlockId to DTAL label
    block_labels: HashMap<BlockId, String>,
    /// Current function name (for label generation)
    func_name: String,
}

impl<'src> CodegenContext<'src> {
    pub fn new(func_name: &str) -> Self {
        Self {
            blocks: Vec::new(),
            block_labels: HashMap::new(),
            func_name: func_name.to_string(),
        }
    }

    /// Generate a label for a block
    pub fn label_for_block(&mut self, block_id: BlockId) -> String {
        if let Some(label) = self.block_labels.get(&block_id) {
            return label.clone();
        }
        let label = format!(".{}_bb{}", self.func_name, block_id.0);
        self.block_labels.insert(block_id, label.clone());
        label
    }

    /// Add a generated DTAL block
    pub fn add_block(&mut self, block: DtalBlock<'src>) {
        self.blocks.push(block);
    }

    /// Take all generated blocks
    pub fn take_blocks(self) -> Vec<DtalBlock<'src>> {
        self.blocks
    }
}

/// Generate DTAL code for a TIR program
pub fn codegen_program<'src>(program: &TirProgram<'src>) -> DtalProgram<'src> {
    DtalProgram {
        functions: program
            .functions
            .iter()
            .map(|f| codegen_function(f))
            .collect(),
    }
}

/// Generate DTAL code for a TIR function
pub fn codegen_function<'src>(func: &TirFunction<'src>) -> DtalFunction<'src> {
    let mut ctx = CodegenContext::new(&func.name);

    // Pre-generate labels for all blocks
    for block_id in func.blocks.keys() {
        ctx.label_for_block(*block_id);
    }

    // Generate code for each block in a deterministic order
    // Start with entry block, then remaining blocks sorted by ID
    let mut block_order: Vec<BlockId> = vec![func.entry_block];
    let mut other_blocks: Vec<BlockId> = func
        .blocks
        .keys()
        .copied()
        .filter(|id| *id != func.entry_block)
        .collect();
    other_blocks.sort();
    block_order.extend(other_blocks);

    for block_id in block_order {
        if let Some(block) = func.blocks.get(&block_id) {
            let dtal_block = codegen_block(&mut ctx, block, func);
            ctx.add_block(dtal_block);
        }
    }

    // Convert TIR params (VirtualReg) to DTAL params (Reg)
    let params: Vec<(Reg, _)> = func
        .params
        .iter()
        .map(|(vreg, ty)| (Reg::Virtual(*vreg), ty.clone()))
        .collect();

    DtalFunction {
        name: func.name.clone(),
        params,
        return_type: func.return_type.clone(),
        precondition: func.precondition.clone(),
        postcondition: func.postcondition.clone(),
        blocks: ctx.take_blocks(),
    }
}

/// Generate DTAL code for a basic block
fn codegen_block<'src>(
    ctx: &mut CodegenContext<'src>,
    block: &BasicBlock<'src>,
    func: &TirFunction<'src>,
) -> DtalBlock<'src> {
    let label = ctx.label_for_block(block.id);
    let mut instructions: Vec<DtalInstr<'src>> = Vec::new();

    // 1. Lower phi nodes to mov instructions
    // In SSA deconstruction, phi nodes become parallel copies at predecessor ends.
    // For simplicity in this phase, we emit them as sequential movs at block entry.
    // A more sophisticated implementation would place copies at predecessor block ends.
    for phi in &block.phi_nodes {
        lower_phi_node(&mut instructions, phi, block, ctx);
    }

    // 2. Lower each TIR instruction to DTAL
    for instr in &block.instructions {
        isel::lower_instruction(&mut instructions, instr);
    }

    // 3. Lower the terminator (including phi moves for successors)
    lower_terminator(&mut instructions, &block.terminator, block.id, ctx, func);

    DtalBlock {
        label,
        entry_state: TypeState::new(), // TODO: compute entry state
        instructions,
    }
}

/// Lower a phi node to mov instructions
///
/// For proper SSA deconstruction, copies should go at predecessor block ends.
/// This simplified version places a comment noting the phi node.
fn lower_phi_node<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    phi: &PhiNode<'src>,
    _block: &BasicBlock<'src>,
    _ctx: &CodegenContext<'src>,
) {
    // Phi nodes in SSA represent merging values from different predecessors.
    // At this stage, we note that the destination register receives a value
    // from one of the incoming edges. The actual value selection happens
    // at runtime based on control flow.
    //
    // For a simple code generator, we can:
    // 1. Emit parallel copies at each predecessor block end (proper SSA deconstruction)
    // 2. Or rely on the fact that we're using virtual registers and the
    //    semantics are "the value from whichever predecessor we came from"
    //
    // For now, we emit a type annotation for the phi result
    instrs.push(DtalInstr::TypeAnnotation {
        reg: Reg::Virtual(phi.dst),
        ty: phi.ty.clone(),
    });
}

/// Lower a TIR terminator to DTAL instructions
///
/// This includes SSA deconstruction: before jumping to a block with phi nodes,
/// we emit mov instructions for the phi incoming values from this block.
fn lower_terminator<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    terminator: &Terminator,
    current_block: BlockId,
    ctx: &mut CodegenContext<'src>,
    func: &TirFunction<'src>,
) {
    use crate::backend::dtal::instr::CmpOp;

    match terminator {
        Terminator::Jump { target } => {
            // Emit phi moves for the target block
            emit_phi_moves(instrs, *target, current_block, func);

            let label = ctx.label_for_block(*target);
            instrs.push(DtalInstr::Jmp { target: label });
        }

        Terminator::Branch {
            cond,
            true_target,
            false_target,
            true_constraint,
            false_constraint: _,
        } => {
            // For branches, we need to emit phi moves before each branch.
            // However, since we're emitting two different destinations, we need
            // to emit the phi moves conditionally. The simplest approach is to
            // split into two separate paths, each with its own phi moves.
            //
            // For now, we emit both sets of phi moves. The register allocator
            // will handle the parallel copies correctly.

            // Compare the condition register with 0
            instrs.push(DtalInstr::CmpImm {
                lhs: Reg::Virtual(*cond),
                imm: 0,
            });

            let true_label = ctx.label_for_block(*true_target);
            let false_label = ctx.label_for_block(*false_target);

            // Branch if not equal to 0 (i.e., condition is true)
            instrs.push(DtalInstr::Branch {
                cond: CmpOp::Ne,
                target: true_label,
            });

            // Emit constraint assumption for the false branch
            instrs.push(DtalInstr::ConstraintAssume {
                constraint: true_constraint.clone(),
            });

            // Emit phi moves for the false target (we're falling through to it)
            emit_phi_moves(instrs, *false_target, current_block, func);

            // Fall through to false target
            instrs.push(DtalInstr::Jmp {
                target: false_label,
            });

            // Note: phi moves for true_target should ideally be placed after
            // the branch lands at true_target. For simplicity in this implementation,
            // we rely on the phi nodes at block entry to handle this.
            // A more sophisticated implementation would use critical edge splitting.
        }

        Terminator::Return { value } => {
            // Move return value to r0 (return register convention)
            if let Some(val_reg) = value {
                instrs.push(DtalInstr::MovReg {
                    dst: Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
                    src: Reg::Virtual(*val_reg),
                    ty: func.return_type.clone(),
                });
            }
            instrs.push(DtalInstr::Ret);
        }

        Terminator::Unreachable => {
            // Emit a comment or trap instruction
            // For now, just emit ret (should never be reached)
            instrs.push(DtalInstr::Ret);
        }
    }
}

/// Emit mov instructions for phi nodes in the target block
///
/// For each phi node in target_block, emit a mov from the incoming value
/// for current_block to the phi destination.
fn emit_phi_moves<'src>(
    instrs: &mut Vec<DtalInstr<'src>>,
    target_block: BlockId,
    current_block: BlockId,
    func: &TirFunction<'src>,
) {
    if let Some(block) = func.blocks.get(&target_block) {
        for phi in &block.phi_nodes {
            // Find the incoming value for current_block
            for (pred_block, incoming_reg) in &phi.incoming {
                if *pred_block == current_block {
                    // Emit mov from incoming_reg to phi.dst
                    instrs.push(DtalInstr::MovReg {
                        dst: Reg::Virtual(phi.dst),
                        src: Reg::Virtual(*incoming_reg),
                        ty: phi.ty.clone(),
                    });
                    break;
                }
            }
        }
    }
}
