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
use crate::backend::dtal::types::DtalType;
use crate::backend::tir::instr::TirInstr;
use crate::backend::tir::{BasicBlock, BlockId, PhiNode, Terminator, TirFunction, TirProgram};
use crate::common::ownership::OwnershipMode;
use std::collections::HashMap;

use super::isel;

/// Code generation context
pub struct CodegenContext {
    /// Generated DTAL blocks
    blocks: Vec<DtalBlock>,
    /// Map from TIR BlockId to DTAL label
    block_labels: HashMap<BlockId, String>,
    /// Current function name (for label generation)
    func_name: String,
    /// Variable name → register name substitutions for constraints
    pub var_subs: Vec<(String, String)>,
    /// Whether we're targeting bare metal (no Linux syscalls available).
    /// Controls whether `AllocArray` lowers to a heap call or a stack alloca.
    pub bare_metal: bool,
    /// Whether this function needs a hosted function-local region.
    pub needs_hosted_region: bool,
    /// Whether this hosted function returns an owned array and therefore
    /// allocates into the caller's region instead of a callee-local region.
    pub returns_owned_array: bool,
}

impl CodegenContext {
    pub fn new(func_name: &str) -> Self {
        Self {
            blocks: Vec::new(),
            block_labels: HashMap::new(),
            func_name: func_name.to_string(),
            var_subs: Vec::new(),
            bare_metal: false,
            needs_hosted_region: false,
            returns_owned_array: false,
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
    pub fn add_block(&mut self, block: DtalBlock) {
        self.blocks.push(block);
    }

    /// Take all generated blocks
    pub fn take_blocks(self) -> Vec<DtalBlock> {
        self.blocks
    }
}

/// Generate DTAL code for a TIR program (default: hosted Linux target).
pub fn codegen_program<'src>(program: &TirProgram<'src>) -> DtalProgram {
    codegen_program_with_target(program, false)
}

/// Generate DTAL code, choosing host vs bare-metal target.
///
/// Bare-metal currently keeps `AllocArray` on the stack; hosted routes it
/// through a function-local mmap-backed region allocator.
pub fn codegen_program_with_target<'src>(
    program: &TirProgram<'src>,
    bare_metal: bool,
) -> DtalProgram {
    let mut functions: Vec<DtalFunction> = program
        .functions
        .iter()
        .map(|f| codegen_function_with_target(f, bare_metal))
        .collect();

    // Inject runtime function stubs so the verifier can check call-site
    // types without needing the runtime's implementation.
    functions.extend(runtime_function_stubs());

    DtalProgram { functions }
}

/// Create stub DtalFunction entries for runtime intrinsics.
///
/// These have correct signatures but empty block lists. The verifier
/// uses them to check preconditions and derive return types at call
/// sites, but never attempts to verify their bodies (no blocks to verify).
fn runtime_function_stubs() -> Vec<DtalFunction> {
    use crate::backend::dtal::regs::{PhysicalReg, Reg};
    use crate::backend::dtal::types::DtalType;

    vec![
        DtalFunction {
            name: "print_int".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_ownerships: vec![OwnershipMode::Plain],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "print_char".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_ownerships: vec![OwnershipMode::Plain],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "read_int".to_string(),
            params: vec![],
            parameter_ownerships: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "port_in".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_ownerships: vec![OwnershipMode::Plain],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "port_out".to_string(),
            params: vec![
                (Reg::Physical(PhysicalReg::R0), DtalType::Int),
                (Reg::Physical(PhysicalReg::R1), DtalType::Int),
            ],
            parameter_ownerships: vec![OwnershipMode::Plain, OwnershipMode::Plain],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        // Hosted function-local region helpers.
        DtalFunction {
            name: crate::backend::runtime::RT_REGION_ENTER.to_string(),
            params: vec![],
            parameter_ownerships: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: crate::backend::runtime::RT_REGION_ALLOC.to_string(),
            params: vec![
                (Reg::Physical(PhysicalReg::R0), DtalType::Int),
                (Reg::Physical(PhysicalReg::R1), DtalType::Int),
            ],
            parameter_ownerships: vec![OwnershipMode::Plain, OwnershipMode::Plain],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: crate::backend::runtime::RT_REGION_LEAVE.to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_ownerships: vec![OwnershipMode::Plain],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
    ]
}

/// Generate DTAL code for a TIR function with target selection.
pub fn codegen_function_with_target<'src>(
    func: &TirFunction<'src>,
    bare_metal: bool,
) -> DtalFunction {
    let mut ctx = CodegenContext::new(&func.name);
    ctx.bare_metal = bare_metal;
    ctx.returns_owned_array = !bare_metal && func.returns_owned;
    ctx.needs_hosted_region =
        !bare_metal && !ctx.returns_owned_array && function_needs_hosted_region(func);

    // Build param name → register name substitution map
    ctx.var_subs = func
        .param_names
        .iter()
        .zip(func.params.iter())
        .map(|(name, (vreg, _))| (name.clone(), format!("{}", Reg::Virtual(*vreg))))
        .collect();

    // Add "result" → return value register mapping for postcondition substitution
    for block in func.blocks.values() {
        if let Terminator::Return {
            value: Some(vreg), ..
        } = &block.terminator
        {
            ctx.var_subs
                .push(("result".to_string(), format!("{}", Reg::Virtual(*vreg))));
            break;
        }
    }

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
    let params: Vec<(Reg, DtalType)> = func
        .params
        .iter()
        .map(|(vreg, ty)| (Reg::Virtual(*vreg), DtalType::from_itype(ty)))
        .collect();

    let name_to_reg = ctx.var_subs.clone();

    let mut dtal_func = DtalFunction {
        name: func.name.clone(),
        params: params.clone(),
        parameter_ownerships: func.parameter_ownerships.clone(),
        return_type: DtalType::from_itype(&func.return_type),
        precondition: func
            .precondition
            .as_ref()
            .map(|c| substitute_constraint_vars(c, &name_to_reg)),
        postcondition: func
            .postcondition
            .as_ref()
            .map(|c| substitute_constraint_vars(c, &name_to_reg)),
        blocks: ctx.take_blocks(),
    };

    // Compute and stamp entry states on blocks using dataflow analysis.
    // This makes the DTAL program self-describing — the verifier can check
    // each block independently using the declared entry state.
    stamp_entry_states(&mut dtal_func);

    dtal_func
}

/// Compute entry states for all blocks and stamp them onto the DTAL function.
///
/// Stamps both register types and constraints from dataflow analysis.
/// The constraints include branch-derived constraints from predecessor edges,
/// which the verifier uses as the block's initial context.
fn stamp_entry_states(func: &mut DtalFunction) {
    use crate::verifier::dataflow::analyze_function;

    if let Ok(dataflow) = analyze_function(func) {
        for block in &mut func.blocks {
            if let Some(entry_state) = dataflow.entry_states.get(&block.label) {
                block.entry_state = entry_state.clone();
            }
        }
    }
}

/// Generate DTAL code for a basic block
fn codegen_block<'src>(
    ctx: &mut CodegenContext,
    block: &BasicBlock<'src>,
    func: &TirFunction<'src>,
) -> DtalBlock {
    let label = ctx.label_for_block(block.id);
    let mut instructions: Vec<DtalInstr> = Vec::new();

    if ctx.needs_hosted_region && block.id == func.entry_block {
        emit_region_enter(&mut instructions);
    } else if ctx.returns_owned_array && block.id == func.entry_block {
        instructions.push(DtalInstr::TypeAnnotation {
            reg: Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R12),
            ty: DtalType::Int,
        });
    }

    // 1. Lower phi nodes to mov instructions
    for phi in &block.phi_nodes {
        lower_phi_node(&mut instructions, phi, block, ctx);
    }

    // 2. Lower each TIR instruction to DTAL
    for instr in &block.instructions {
        isel::lower_instruction(&mut instructions, instr, ctx.bare_metal);
    }

    // 3. Lower the terminator (including phi moves for successors)
    lower_terminator(&mut instructions, &block.terminator, block.id, ctx, func);

    // 4. Substitute source-level variable names with register names in constraints
    if !ctx.var_subs.is_empty() {
        for instr in &mut instructions {
            if let DtalInstr::ConstraintAssert { constraint, .. } = instr {
                *constraint = substitute_constraint_vars(constraint, &ctx.var_subs);
            }
        }
    }

    DtalBlock {
        label,
        entry_state: TypeState::new(),
        instructions,
    }
}

fn function_needs_hosted_region<'src>(func: &TirFunction<'src>) -> bool {
    func.blocks.values().any(|block| {
        block.instructions.iter().any(|instr| {
            matches!(instr, TirInstr::AllocArray { region: None, .. })
                || matches!(
                    instr,
                    TirInstr::Call {
                        ownership: OwnershipMode::FreshOwned,
                        ..
                    }
                )
        })
    })
}

fn emit_region_enter(instrs: &mut Vec<DtalInstr>) {
    use crate::backend::dtal::regs::PhysicalReg;

    instrs.push(DtalInstr::Call {
        target: crate::backend::runtime::RT_REGION_ENTER.to_string(),
        arg_ownerships: vec![],
        return_ty: DtalType::Int,
        ownership: OwnershipMode::Plain,
    });
    instrs.push(DtalInstr::MovReg {
        dst: Reg::Physical(PhysicalReg::R12),
        src: Reg::Physical(PhysicalReg::R0),
        ty: DtalType::Int,
    });
}

fn emit_region_leave(instrs: &mut Vec<DtalInstr>) {
    use crate::backend::dtal::regs::PhysicalReg;

    instrs.push(DtalInstr::MovReg {
        dst: Reg::Physical(PhysicalReg::R0),
        src: Reg::Physical(PhysicalReg::R12),
        ty: DtalType::Int,
    });
    instrs.push(DtalInstr::Call {
        target: crate::backend::runtime::RT_REGION_LEAVE.to_string(),
        arg_ownerships: vec![],
        return_ty: DtalType::Unit,
        ownership: OwnershipMode::Plain,
    });
}

/// Lower a phi node to mov instructions
fn lower_phi_node<'src>(
    instrs: &mut Vec<DtalInstr>,
    phi: &PhiNode<'src>,
    _block: &BasicBlock<'src>,
    _ctx: &CodegenContext,
) {
    let ty = if let Some((witness_var, constraint)) = &phi.existential_constraint {
        DtalType::ExistentialInt {
            witness_var: witness_var.clone(),
            constraint: constraint.clone(),
        }
    } else {
        DtalType::from_itype(&phi.ty)
    };
    instrs.push(DtalInstr::TypeAnnotation {
        reg: Reg::Virtual(phi.dst),
        ty,
    });
}

/// Lower a TIR terminator to DTAL instructions
fn lower_terminator<'src>(
    instrs: &mut Vec<DtalInstr>,
    terminator: &Terminator,
    current_block: BlockId,
    ctx: &mut CodegenContext,
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
            true_constraint: _,
            false_constraint: _,
        } => {
            let true_label = ctx.label_for_block(*true_target);
            let false_label = ctx.label_for_block(*false_target);

            // Try to find the original comparison that produced the condition
            // register. If `cond` was defined by a SetCC, reuse the preceding
            // Cmp operands for the branch so the verifier derives the actual
            // comparison constraint (e.g., `v1 < v2`) instead of `v_cond != 0`.
            let (branch_cond, needs_cmp) = find_original_comparison(instrs, *cond);

            if needs_cmp {
                // Couldn't find original comparison — fall back to cmp cond, 0
                instrs.push(DtalInstr::CmpImm {
                    lhs: Reg::Virtual(*cond),
                    imm: 0,
                });
                instrs.push(DtalInstr::Branch {
                    cond: CmpOp::Ne,
                    target: true_label,
                });
            } else {
                // Reuse the original Cmp instruction's state — just emit the branch
                // with the original comparison condition
                instrs.push(DtalInstr::Branch {
                    cond: branch_cond,
                    target: true_label,
                });
            }

            // Branch constraints are derived independently by the verifier
            // from Cmp+Branch and existential types — no ConstraintAssume needed.

            // Emit phi moves for the false target (we're falling through to it)
            emit_phi_moves(instrs, *false_target, current_block, func);

            // Fall through to false target
            instrs.push(DtalInstr::Jmp {
                target: false_label,
            });
        }

        Terminator::Return { value, ownership } => {
            if let Some(val_reg) = value {
                let ret_ty = DtalType::from_itype(&func.return_type);
                if ctx.needs_hosted_region {
                    instrs.push(DtalInstr::Push {
                        src: Reg::Virtual(*val_reg),
                        ty: ret_ty.clone(),
                    });
                    emit_region_leave(instrs);
                    instrs.push(DtalInstr::Pop {
                        dst: Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
                        ty: ret_ty,
                    });
                } else if ownership.produces_owned_output() {
                    instrs.push(DtalInstr::MoveOwned {
                        dst: Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
                        src: Reg::Virtual(*val_reg),
                        ty: ret_ty,
                    });
                } else {
                    instrs.push(DtalInstr::MovReg {
                        dst: Reg::Physical(crate::backend::dtal::regs::PhysicalReg::R0),
                        src: Reg::Virtual(*val_reg),
                        ty: ret_ty,
                    });
                }
            } else if ctx.needs_hosted_region {
                emit_region_leave(instrs);
            }
            instrs.push(DtalInstr::Ret);
        }

        Terminator::Unreachable => {
            if ctx.needs_hosted_region {
                emit_region_leave(instrs);
            }
            instrs.push(DtalInstr::Ret);
        }
    }
}

/// Find the original comparison that produced a condition register.
///
/// Scans backwards through the instruction list for a `SetCC` that defined
/// `cond_reg`. If found, returns the `CmpOp` from the `SetCC` and `false`
/// (no new Cmp needed). The preceding `Cmp`/`CmpImm` instruction remains
/// in the stream, so the verifier's `last_cmp` is already set correctly.
///
/// If not found, returns `(Ne, true)` — caller should emit `CmpImm cond, 0`.
fn find_original_comparison(
    instrs: &[DtalInstr],
    cond_reg: crate::backend::dtal::VirtualReg,
) -> (crate::backend::dtal::instr::CmpOp, bool) {
    use crate::backend::dtal::instr::CmpOp;

    let target = Reg::Virtual(cond_reg);

    // Scan backwards for SetCC that defined cond_reg
    for instr in instrs.iter().rev() {
        match instr {
            DtalInstr::SetCC { dst, cond } if *dst == target => {
                return (*cond, false);
            }
            // If we hit an instruction that redefines the register via something
            // other than SetCC, stop looking
            DtalInstr::MovImm { dst, .. }
            | DtalInstr::MovReg { dst, .. }
            | DtalInstr::BinOp { dst, .. }
            | DtalInstr::AddImm { dst, .. }
            | DtalInstr::ShlImm { dst, .. }
            | DtalInstr::ShrImm { dst, .. }
            | DtalInstr::Load { dst, .. }
            | DtalInstr::LoadOp { dst, .. }
            | DtalInstr::Not { dst, .. }
            | DtalInstr::Neg { dst, .. }
            | DtalInstr::Pop { dst, .. }
            | DtalInstr::Alloca { dst, .. }
                if *dst == target =>
            {
                break;
            }
            _ => {}
        }
    }

    // Fallback: condition wasn't from a SetCC
    (CmpOp::Ne, true)
}

/// Emit mov instructions for phi nodes in the target block
fn emit_phi_moves<'src>(
    instrs: &mut Vec<DtalInstr>,
    target_block: BlockId,
    current_block: BlockId,
    func: &TirFunction<'src>,
) {
    if let Some(block) = func.blocks.get(&target_block) {
        for phi in &block.phi_nodes {
            // Find the incoming value for current_block
            for (pred_block, incoming_reg) in &phi.incoming {
                if *pred_block == current_block {
                    let ty = DtalType::from_itype(&phi.ty);
                    if matches!(&phi.ty, crate::common::types::IType::Array { .. }) {
                        instrs.push(DtalInstr::MoveOwned {
                            dst: Reg::Virtual(phi.dst),
                            src: Reg::Virtual(*incoming_reg),
                            ty,
                        });
                    } else {
                        instrs.push(DtalInstr::MovReg {
                            dst: Reg::Virtual(phi.dst),
                            src: Reg::Virtual(*incoming_reg),
                            ty,
                        });
                    }
                    break;
                }
            }
        }
    }
}

/// Substitute variable names in a constraint with register names.
///
/// Replaces `IndexExpr::Var("param_name")` with `IndexExpr::Var("v0")` etc.
pub(crate) fn substitute_constraint_vars(
    constraint: &crate::backend::dtal::Constraint,
    subs: &[(String, String)],
) -> crate::backend::dtal::Constraint {
    use crate::backend::dtal::Constraint;

    match constraint {
        Constraint::True => Constraint::True,
        Constraint::False => Constraint::False,
        Constraint::Eq(l, r) => Constraint::Eq(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::Lt(l, r) => Constraint::Lt(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::Le(l, r) => Constraint::Le(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::Gt(l, r) => Constraint::Gt(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::Ge(l, r) => Constraint::Ge(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::Ne(l, r) => Constraint::Ne(
            substitute_index_vars(l, subs),
            substitute_index_vars(r, subs),
        ),
        Constraint::And(l, r) => Constraint::And(
            Box::new(substitute_constraint_vars(l, subs)),
            Box::new(substitute_constraint_vars(r, subs)),
        ),
        Constraint::Or(l, r) => Constraint::Or(
            Box::new(substitute_constraint_vars(l, subs)),
            Box::new(substitute_constraint_vars(r, subs)),
        ),
        Constraint::Not(c) => Constraint::Not(Box::new(substitute_constraint_vars(c, subs))),
        Constraint::Implies(l, r) => Constraint::Implies(
            Box::new(substitute_constraint_vars(l, subs)),
            Box::new(substitute_constraint_vars(r, subs)),
        ),
        Constraint::Forall {
            var,
            lower,
            upper,
            body,
        } => {
            // Don't substitute the bound variable inside the body
            let filtered: Vec<_> = subs.iter().filter(|(n, _)| n != var).cloned().collect();
            Constraint::Forall {
                var: var.clone(),
                lower: substitute_index_vars(lower, subs),
                upper: substitute_index_vars(upper, subs),
                body: Box::new(substitute_constraint_vars(body, &filtered)),
            }
        }
        Constraint::Exists {
            var,
            lower,
            upper,
            body,
        } => {
            let filtered: Vec<_> = subs.iter().filter(|(n, _)| n != var).cloned().collect();
            Constraint::Exists {
                var: var.clone(),
                lower: substitute_index_vars(lower, subs),
                upper: substitute_index_vars(upper, subs),
                body: Box::new(substitute_constraint_vars(body, &filtered)),
            }
        }
    }
}

/// Substitute variable names in an index expression with register names.
pub(crate) fn substitute_index_vars(
    expr: &crate::backend::dtal::IndexExpr,
    subs: &[(String, String)],
) -> crate::backend::dtal::IndexExpr {
    use crate::backend::dtal::IndexExpr;

    match expr {
        IndexExpr::Const(n) => IndexExpr::Const(*n),
        IndexExpr::Var(name) => {
            for (from, to) in subs {
                if name == from {
                    return IndexExpr::Var(to.clone());
                }
            }
            IndexExpr::Var(name.clone())
        }
        IndexExpr::Add(l, r) => IndexExpr::Add(
            Box::new(substitute_index_vars(l, subs)),
            Box::new(substitute_index_vars(r, subs)),
        ),
        IndexExpr::Sub(l, r) => IndexExpr::Sub(
            Box::new(substitute_index_vars(l, subs)),
            Box::new(substitute_index_vars(r, subs)),
        ),
        IndexExpr::Mul(l, r) => IndexExpr::Mul(
            Box::new(substitute_index_vars(l, subs)),
            Box::new(substitute_index_vars(r, subs)),
        ),
        IndexExpr::Div(l, r) => IndexExpr::Div(
            Box::new(substitute_index_vars(l, subs)),
            Box::new(substitute_index_vars(r, subs)),
        ),
        IndexExpr::Mod(l, r) => IndexExpr::Mod(
            Box::new(substitute_index_vars(l, subs)),
            Box::new(substitute_index_vars(r, subs)),
        ),
        IndexExpr::Select(name, idx) => {
            let new_name = subs
                .iter()
                .find(|(from, _)| from == name)
                .map(|(_, to)| to.clone())
                .unwrap_or_else(|| name.clone());
            IndexExpr::Select(new_name, Box::new(substitute_index_vars(idx, subs)))
        }
    }
}
