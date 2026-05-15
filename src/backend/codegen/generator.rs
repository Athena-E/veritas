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

use crate::common::ownership::{OwnershipMode, ParameterKind};
use crate::dtal::instr::{DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use crate::dtal::regs::Reg;
use crate::dtal::types::DtalType;
use crate::middle::tir::instr::TirInstr;
use crate::middle::tir::{BasicBlock, BlockId, PhiNode, Terminator, TirFunction, TirProgram};
use std::collections::HashMap;

use super::isel;

pub struct CodegenContext {
    blocks: Vec<DtalBlock>,
    block_labels: HashMap<BlockId, String>,
    func_name: String,
    pub var_subs: Vec<(String, String)>,
    pub bare_metal: bool,
    pub needs_hosted_region: bool,
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

    pub fn label_for_block(&mut self, block_id: BlockId) -> String {
        if let Some(label) = self.block_labels.get(&block_id) {
            return label.clone();
        }
        let label = format!(".{}_bb{}", self.func_name, block_id.0);
        self.block_labels.insert(block_id, label.clone());
        label
    }

    pub fn add_block(&mut self, block: DtalBlock) {
        self.blocks.push(block);
    }

    pub fn take_blocks(self) -> Vec<DtalBlock> {
        self.blocks
    }
}

pub fn codegen_program<'src>(program: &TirProgram<'src>) -> DtalProgram {
    codegen_program_with_target(program, false)
}

pub fn codegen_program_with_target<'src>(
    program: &TirProgram<'src>,
    bare_metal: bool,
) -> DtalProgram {
    let mut functions: Vec<DtalFunction> = program
        .functions
        .iter()
        .map(|f| codegen_function_with_target(f, bare_metal))
        .collect();

    functions.extend(runtime_function_stubs());

    DtalProgram { functions }
}

fn runtime_function_stubs() -> Vec<DtalFunction> {
    use crate::dtal::regs::{PhysicalReg, Reg};
    use crate::dtal::types::DtalType;

    vec![
        DtalFunction {
            name: "print_int".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_kinds: vec![ParameterKind::PlainValue],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "print_char".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_kinds: vec![ParameterKind::PlainValue],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "read_int".to_string(),
            params: vec![],
            parameter_kinds: vec![],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: "port_in".to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_kinds: vec![ParameterKind::PlainValue],
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
            parameter_kinds: vec![ParameterKind::PlainValue, ParameterKind::PlainValue],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: crate::backend::runtime::RT_REGION_ENTER.to_string(),
            params: vec![],
            parameter_kinds: vec![],
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
            parameter_kinds: vec![ParameterKind::PlainValue, ParameterKind::PlainValue],
            return_type: DtalType::Int,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
        DtalFunction {
            name: crate::backend::runtime::RT_REGION_LEAVE.to_string(),
            params: vec![(Reg::Physical(PhysicalReg::R0), DtalType::Int)],
            parameter_kinds: vec![ParameterKind::PlainValue],
            return_type: DtalType::Unit,
            precondition: None,
            postcondition: None,
            blocks: vec![],
        },
    ]
}

pub fn codegen_function_with_target<'src>(
    func: &TirFunction<'src>,
    bare_metal: bool,
) -> DtalFunction {
    let mut ctx = CodegenContext::new(&func.name);
    ctx.bare_metal = bare_metal;
    ctx.returns_owned_array = !bare_metal && func.returns_owned;
    ctx.needs_hosted_region =
        !bare_metal && !ctx.returns_owned_array && function_needs_hosted_region(func);

    ctx.var_subs = func
        .param_names
        .iter()
        .zip(func.params.iter())
        .map(|(name, (vreg, _))| (name.clone(), format!("{}", Reg::Virtual(*vreg))))
        .collect();

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

    for block_id in func.blocks.keys() {
        ctx.label_for_block(*block_id);
    }

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

    let params: Vec<(Reg, DtalType)> = func
        .params
        .iter()
        .map(|(vreg, ty)| (Reg::Virtual(*vreg), DtalType::from_itype(ty)))
        .collect();

    let name_to_reg = ctx.var_subs.clone();

    let mut dtal_func = DtalFunction {
        name: func.name.clone(),
        params: params.clone(),
        parameter_kinds: func.parameter_kinds.clone(),
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

    stamp_entry_states(&mut dtal_func);

    dtal_func
}

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
            reg: Reg::Physical(crate::dtal::regs::PhysicalReg::R12),
            ty: DtalType::Int,
        });
    }

    for phi in &block.phi_nodes {
        lower_phi_node(&mut instructions, phi, block, ctx);
    }

    for instr in &block.instructions {
        isel::lower_instruction(&mut instructions, instr, ctx.bare_metal);
    }

    lower_terminator(&mut instructions, &block.terminator, block.id, ctx, func);

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
    use crate::dtal::regs::PhysicalReg;

    instrs.push(DtalInstr::Call {
        target: crate::backend::runtime::RT_REGION_ENTER.to_string(),
        arg_kinds: vec![],
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
    use crate::dtal::regs::PhysicalReg;

    instrs.push(DtalInstr::MovReg {
        dst: Reg::Physical(PhysicalReg::R0),
        src: Reg::Physical(PhysicalReg::R12),
        ty: DtalType::Int,
    });
    instrs.push(DtalInstr::Call {
        target: crate::backend::runtime::RT_REGION_LEAVE.to_string(),
        arg_kinds: vec![],
        return_ty: DtalType::Unit,
        ownership: OwnershipMode::Plain,
    });
}

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

fn lower_terminator<'src>(
    instrs: &mut Vec<DtalInstr>,
    terminator: &Terminator,
    current_block: BlockId,
    ctx: &mut CodegenContext,
    func: &TirFunction<'src>,
) {
    use crate::dtal::instr::CmpOp;

    match terminator {
        Terminator::Jump { target } => {
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

            let (branch_cond, needs_cmp) = find_original_comparison(instrs, *cond);

            if needs_cmp {
                instrs.push(DtalInstr::CmpImm {
                    lhs: Reg::Virtual(*cond),
                    imm: 0,
                });
                instrs.push(DtalInstr::Branch {
                    cond: CmpOp::Ne,
                    target: true_label,
                });
            } else {
                instrs.push(DtalInstr::Branch {
                    cond: branch_cond,
                    target: true_label,
                });
            }

            emit_phi_moves(instrs, *false_target, current_block, func);

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
                        dst: Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
                        ty: ret_ty,
                    });
                } else if ownership.produces_owned_output() {
                    instrs.push(DtalInstr::MoveOwned {
                        dst: Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
                        src: Reg::Virtual(*val_reg),
                        ty: ret_ty,
                    });
                } else {
                    instrs.push(DtalInstr::MovReg {
                        dst: Reg::Physical(crate::dtal::regs::PhysicalReg::R0),
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

fn find_original_comparison(
    instrs: &[DtalInstr],
    cond_reg: crate::dtal::VirtualReg,
) -> (crate::dtal::instr::CmpOp, bool) {
    use crate::dtal::instr::CmpOp;

    let target = Reg::Virtual(cond_reg);

    for instr in instrs.iter().rev() {
        match instr {
            DtalInstr::SetCC { dst, cond } if *dst == target => {
                return (*cond, false);
            }
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

    (CmpOp::Ne, true)
}

fn emit_phi_moves<'src>(
    instrs: &mut Vec<DtalInstr>,
    target_block: BlockId,
    current_block: BlockId,
    func: &TirFunction<'src>,
) {
    if let Some(block) = func.blocks.get(&target_block) {
        for phi in &block.phi_nodes {
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

pub(crate) fn substitute_constraint_vars(
    constraint: &crate::dtal::Constraint,
    subs: &[(String, String)],
) -> crate::dtal::Constraint {
    use crate::dtal::Constraint;

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

pub(crate) fn substitute_index_vars(
    expr: &crate::dtal::IndexExpr,
    subs: &[(String, String)],
) -> crate::dtal::IndexExpr {
    use crate::dtal::IndexExpr;

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
