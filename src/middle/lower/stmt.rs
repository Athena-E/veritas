//! Statement lowering from TAST to TIR
//!
//! This module converts typed statements (`TStmt`) into TIR instructions
//! and control flow structures.

use crate::common::ownership::OwnershipMode;
use crate::common::span::Spanned;
use crate::common::tast::{TBlock, TExpr, TStmt};
use crate::common::types::IType;
use crate::dtal::{Constraint, IndexExpr, VirtualReg};
use crate::middle::lower::context::{LoweringContext, ScalarBorrowBinding};
use crate::middle::lower::expr::{expr_to_index_expr, lower_expr};
use crate::middle::lower::widen_itype;
use crate::middle::tir::{BinaryOp, PhiNode, Terminator, TirInstr};

fn is_owned_type<'src>(ty: &IType<'src>) -> bool {
    matches!(ty, IType::Array { .. })
}

fn is_borrow_type<'src>(ty: &IType<'src>) -> bool {
    matches!(ty, IType::Ref(_) | IType::RefMut(_))
}

/// Lower a statement to TIR
///
/// This may emit instructions and/or create new basic blocks.
pub fn lower_stmt<'src>(ctx: &mut LoweringContext<'src>, stmt: &Spanned<TStmt<'src>>) {
    match &stmt.0 {
        TStmt::Let {
            is_mut: _,
            name,
            declared_ty,
            value,
            checked_ty: _,
            ownership,
        } => {
            lower_let(ctx, name, value, declared_ty, *ownership);
        }

        TStmt::Assignment {
            lhs,
            rhs,
            ownership,
        } => {
            lower_assignment(ctx, lhs, rhs, *ownership);
        }

        TStmt::Return { expr, ownership: _ } => {
            lower_return(ctx, expr);
        }

        TStmt::Expr(expr) => {
            lower_expr(ctx, expr);
        }

        TStmt::BorrowEnd { name, lifetime, .. } => {
            ctx.emit_borrow_end_for_binding_with_lifetime(name, Some(*lifetime));
        }

        TStmt::For {
            var,
            var_ty,
            start,
            end,
            invariant,
            body,
        } => {
            lower_for_loop(ctx, var, var_ty, start, end, invariant.as_ref(), body);
        }

        TStmt::While {
            condition,
            invariant,
            body,
        } => {
            lower_while_loop(ctx, condition, invariant.as_ref(), body);
        }

        TStmt::Region { body } => {
            lower_region(ctx, body);
        }
    }
}

/// Lower a sequence of statements
pub fn lower_stmts<'src>(ctx: &mut LoweringContext<'src>, stmts: &[Spanned<TStmt<'src>>]) {
    for stmt in stmts {
        lower_stmt(ctx, stmt);
    }
}

/// Lower a let statement
fn lower_let<'src>(
    ctx: &mut LoweringContext<'src>,
    name: &str,
    value: &Spanned<TExpr<'src>>,
    ty: &IType<'src>,
    ownership: OwnershipMode,
) {
    let prior_binding = ctx
        .lookup_var(name)
        .zip(Some(ctx.lookup_var_type(name)))
        .filter(|(_, prior_ty)| is_owned_type(prior_ty));
    let prior_borrow_binding = ctx
        .lookup_var(name)
        .zip(Some(ctx.lookup_var_type(name)))
        .filter(|(_, prior_ty)| is_borrow_type(prior_ty));

    if matches!(ty, IType::Ref(inner) | IType::RefMut(inner) if !matches!(inner.as_ref(), IType::Array { .. }))
        && let TExpr::Borrow { expr, lifetime, .. } = &value.0
        && let TExpr::Variable { name: owner, .. } = &expr.0
    {
        if let Some((prior_reg, prior_ty)) = prior_binding {
            ctx.emit(TirInstr::DropOwned {
                src: prior_reg,
                ty: prior_ty,
            });
        }
        if prior_borrow_binding.is_some() {
            ctx.emit_borrow_end_for_binding(name);
        }
        let owner_reg = ctx
            .lookup_var(owner)
            .unwrap_or_else(|| panic!("Undefined scalar borrow owner during lowering: {}", owner));
        let pointee_ty = expr.0.get_type().clone();
        let kind = if matches!(ty, IType::Ref(_)) {
            crate::common::ownership::BorrowKind::Shared
        } else {
            crate::common::ownership::BorrowKind::Mutable
        };
        let (borrow_reg, cell_reg, lowered_ref_ty) =
            ctx.create_scalar_borrow_value(owner_reg, pointee_ty.clone(), kind, *lifetime);
        ctx.declare_scalar_borrow(
            name,
            borrow_reg,
            lowered_ref_ty,
            ScalarBorrowBinding {
                owner_name: owner.clone(),
                cell_reg,
                kind,
                lifetime: *lifetime,
                pointee_ty,
            },
        );
        return;
    }

    let value_reg = lower_expr(ctx, value);
    let bound_reg = if ownership.consumes_input() && matches!(&value.0, TExpr::Variable { .. }) {
        let moved_reg = ctx.fresh_reg();
        ctx.emit(TirInstr::MoveOwned {
            dst: moved_reg,
            src: value_reg,
            ty: ty.clone(),
        });
        if let TExpr::Variable { name: rhs_name, .. } = &value.0 {
            ctx.mark_var_moved(rhs_name);
        }
        moved_reg
    } else {
        value_reg
    };

    if let Some((prior_reg, prior_ty)) = prior_binding {
        let transferred_from_shadowed = ownership.consumes_input()
            && matches!(&value.0, TExpr::Variable { name: rhs_name, .. } if rhs_name == name);
        if !transferred_from_shadowed {
            ctx.emit(TirInstr::DropOwned {
                src: prior_reg,
                ty: prior_ty,
            });
        }
    }
    if let Some((prior_reg, prior_ty)) = prior_borrow_binding
        && !matches!(&value.0, TExpr::Variable { name: rhs_name, .. } if rhs_name == name && is_borrow_type(&prior_ty))
    {
        let _ = prior_reg;
        let _ = prior_ty;
        ctx.emit_borrow_end_for_binding(name);
    }

    ctx.declare_var_typed(name, bound_reg, ty.clone());
}

/// Lower an assignment statement
fn lower_assignment<'src>(
    ctx: &mut LoweringContext<'src>,
    lhs: &Spanned<TExpr<'src>>,
    rhs: &Spanned<TExpr<'src>>,
    ownership: OwnershipMode,
) {
    match &lhs.0 {
        TExpr::Variable { name, ty: _ } => {
            let prior_reg = ctx.lookup_var(name);
            let prior_ty = ctx.lookup_var_type(name);
            let self_assignment =
                matches!(&rhs.0, TExpr::Variable { name: rhs_name, .. } if rhs_name == name);
            if matches!(ctx.lookup_var_type(name), IType::Ref(_) | IType::RefMut(_))
                && let TExpr::Borrow { expr, lifetime, .. } = &rhs.0
                && let TExpr::Variable { name: owner, .. } = &expr.0
            {
                let owner_reg = ctx.lookup_var(owner).unwrap_or_else(|| {
                    panic!("Undefined scalar borrow owner during lowering: {}", owner)
                });
                let pointee_ty = expr.0.get_type().clone();
                let kind = if matches!(rhs.0.get_type(), IType::Ref(_)) {
                    crate::common::ownership::BorrowKind::Shared
                } else {
                    crate::common::ownership::BorrowKind::Mutable
                };
                let (borrow_reg, cell_reg, lowered_ref_ty) =
                    ctx.create_scalar_borrow_value(owner_reg, pointee_ty.clone(), kind, *lifetime);
                if prior_reg.is_some() && is_borrow_type(&prior_ty) && !self_assignment {
                    ctx.emit_borrow_end_for_binding(name);
                }
                ctx.bind_scalar_borrow(
                    name,
                    borrow_reg,
                    lowered_ref_ty,
                    ScalarBorrowBinding {
                        owner_name: owner.clone(),
                        cell_reg,
                        kind,
                        lifetime: *lifetime,
                        pointee_ty,
                    },
                );
                return;
            }

            let rhs_reg = lower_expr(ctx, rhs);

            // The RHS type may be more precise than the LHS declaration.
            let new_reg = if matches!(&rhs.0, TExpr::Borrow { .. }) {
                rhs_reg
            } else {
                let new_reg = ctx.fresh_reg();
                if ownership.consumes_input() && matches!(&rhs.0, TExpr::Variable { .. }) {
                    ctx.emit(TirInstr::MoveOwned {
                        dst: new_reg,
                        src: rhs_reg,
                        ty: rhs.0.get_type().clone(),
                    });
                    if let TExpr::Variable { name: rhs_name, .. } = &rhs.0 {
                        ctx.mark_var_moved(rhs_name);
                    }
                } else {
                    ctx.emit(TirInstr::Copy {
                        dst: new_reg,
                        src: rhs_reg,
                        ty: rhs.0.get_type().clone(),
                    });
                }
                new_reg
            };

            if let Some(prior_reg) = prior_reg
                && is_owned_type(&prior_ty)
                && !self_assignment
            {
                ctx.emit(TirInstr::DropOwned {
                    src: prior_reg,
                    ty: prior_ty.clone(),
                });
            }
            if let Some(prior_reg) = prior_reg
                && is_borrow_type(&prior_ty)
                && !self_assignment
            {
                let _ = prior_reg;
                ctx.emit_borrow_end_for_binding(name);
            }

            ctx.bind_var_typed(name, new_reg, rhs.0.get_type().clone());
        }

        TExpr::Index { base, index, ty: _ } => {
            let base_reg = lower_expr(ctx, base);
            let index_reg = lower_expr(ctx, index);
            let rhs_reg = lower_expr(ctx, rhs);

            ctx.emit(TirInstr::ArrayStore {
                base: base_reg,
                index: index_reg,
                value: rhs_reg,
                bounds_constraint: Constraint::True,
            });
        }

        TExpr::Deref { expr, .. } => {
            let base_reg = lower_expr(ctx, expr);
            let index_reg = ctx.fresh_reg();
            ctx.emit(TirInstr::LoadImm {
                dst: index_reg,
                value: 0,
                ty: IType::Int,
            });
            let rhs_reg = lower_expr(ctx, rhs);
            ctx.emit(TirInstr::ArrayStore {
                base: base_reg,
                index: index_reg,
                value: rhs_reg,
                bounds_constraint: Constraint::True,
            });
        }

        _ => {
            panic!("Invalid assignment target: {:?}", lhs.0);
        }
    }
}

/// Lower a return statement
///
/// Note: This finishes the current block with a Return terminator.
/// The caller should handle any cleanup needed.
fn lower_return<'src>(ctx: &mut LoweringContext<'src>, expr: &Spanned<TExpr<'src>>) {
    let _value_reg = lower_expr(ctx, expr);
}

/// Lower a for loop to CFG with loop-carried phi nodes
///
/// CFG structure:
/// ```text
///            ┌─────────┐
///            │  entry  │
///            │ i = start│
///            └────┬────┘
///                 │
///          ┌──────▼──────┐
///          │ loop_header │◄────┐
///          │ i_φ = φ(...) │    │
///          │ cmp i < end │    │
///          └──────┬──────┘    │
///            true/ \false     │
///               /   \         │
///     ┌────────▼┐   ┌▼────┐   │
///     │  body   │   │exit │   │
///     │  ...    │   └─────┘   │
///     │ i_next  │             │
///     └────┬────┘             │
///          └──────────────────┘
/// ```
fn lower_for_loop<'src>(
    ctx: &mut LoweringContext<'src>,
    var: &str,
    var_ty: &IType<'src>,
    start: &Spanned<TExpr<'src>>,
    end: &Spanned<TExpr<'src>>,
    invariant: Option<&Constraint>,
    body: &TBlock<'src>,
) {
    let start_reg = lower_expr(ctx, start);
    let end_reg = lower_expr(ctx, end);
    let entry_block = ctx.current_block().expect("Should be in a block");

    let header_block = ctx.new_block();
    let body_block = ctx.new_block();
    let exit_block = ctx.new_block();

    let vars_before_loop = ctx.snapshot_var_map();

    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![],
    );

    ctx.start_block(header_block);

    // Loop-variable phi: entry provides start, body provides i_next.
    let i_phi_reg = ctx.fresh_reg();
    let mut i_phi = PhiNode::new(i_phi_reg, var_ty.clone());
    i_phi.add_incoming(entry_block, start_reg);

    // Use the register name as the existential witness for codegen.
    let phi_witness = format!("v{}", i_phi_reg.0);
    let phi_start_idx = expr_to_index_expr(start).unwrap_or(IndexExpr::Const(0));
    let phi_end_idx = expr_to_index_expr(end).unwrap_or(IndexExpr::Const(i64::MAX as i128));
    let existential_constraint = Constraint::And(
        Box::new(Constraint::Ge(
            IndexExpr::Var(phi_witness.clone()),
            phi_start_idx,
        )),
        Box::new(Constraint::Lt(
            IndexExpr::Var(phi_witness.clone()),
            phi_end_idx,
        )),
    );
    i_phi.existential_constraint = Some((phi_witness, existential_constraint));

    ctx.emit_phi(i_phi);

    ctx.bind_var_typed(var, i_phi_reg, var_ty.clone());

    // Loop-carried variables follow the loop-variable phi.
    let mut loop_carried_vars: Vec<(String, VirtualReg)> = Vec::new();
    for (name, &before_reg) in &vars_before_loop {
        if name != var {
            let phi_reg = ctx.fresh_reg();
            let original_ty = ctx.lookup_var_type(name);
            if matches!(&original_ty, IType::Array { .. }) && !ctx.is_owned_live(name) {
                continue;
            }

            // Preserve refined ints as phi existentials.
            let existential = if let IType::RefinedInt { prop, .. } = &original_ty {
                use crate::dtal::convert::expr_to_constraint;
                if let Some(constraint) = expr_to_constraint(&prop.predicate.0) {
                    let phi_witness = format!("_ex_v{}", phi_reg.0);
                    let renamed = crate::backend::codegen::generator::substitute_constraint_vars(
                        &constraint,
                        &[(prop.var.clone(), phi_witness.clone())],
                    );
                    Some((phi_witness, renamed))
                } else {
                    None
                }
            } else {
                None
            };

            let var_ty = widen_itype(original_ty);
            let mut phi = PhiNode::new(phi_reg, var_ty);
            phi.add_incoming(entry_block, before_reg);
            phi.existential_constraint = existential;
            ctx.emit_phi(phi);
            ctx.bind_var(name, phi_reg);
            loop_carried_vars.push((name.clone(), phi_reg));
        }
    }

    let cmp_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::BinOp {
        dst: cmp_reg,
        op: BinaryOp::Lt,
        lhs: i_phi_reg,
        rhs: end_reg,
        ty: IType::Bool,
    });

    // Branch constraints use register names because verification is register-based.
    let loop_var_idx = IndexExpr::Var(format!("v{}", i_phi_reg.0));
    let start_idx = expr_to_index_expr(start).unwrap_or(IndexExpr::Const(0));
    let end_idx = expr_to_index_expr(end).unwrap_or(IndexExpr::Const(i64::MAX as i128));
    let true_constraint = Constraint::And(
        Box::new(Constraint::Ge(loop_var_idx.clone(), start_idx)),
        Box::new(Constraint::Lt(loop_var_idx.clone(), end_idx.clone())),
    );
    let false_constraint = Constraint::Ge(loop_var_idx.clone(), end_idx);

    ctx.finish_block(
        Terminator::Branch {
            cond: cmp_reg,
            true_target: body_block,
            false_target: exit_block,
            true_constraint: Box::new(true_constraint),
            false_constraint: Box::new(false_constraint),
        },
        vec![entry_block], // Predecessor from entry's jump
    );

    ctx.start_block(body_block);
    ctx.enter_scope();

    if let Some(inv_constraint) = invariant {
        let subs = ctx.var_substitutions();
        let substituted =
            crate::backend::codegen::generator::substitute_constraint_vars(inv_constraint, &subs);
        ctx.emit(TirInstr::AssertConstraint {
            constraint: substituted,
        });
    }

    for stmt in &body.statements {
        lower_stmt(ctx, stmt);
    }

    ctx.emit_scope_exit_drops();

    let vars_after_body = ctx.snapshot_var_map();
    ctx.exit_scope();

    let one_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::LoadImm {
        dst: one_reg,
        value: 1,
        ty: IType::SingletonInt(crate::common::types::IValue::Int(1)),
    });

    let i_next_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::BinOp {
        dst: i_next_reg,
        op: BinaryOp::Add,
        lhs: i_phi_reg,
        rhs: one_reg,
        ty: var_ty.clone(),
    });

    let body_end_block = ctx.current_block().expect("Should be in body block");

    ctx.update_phi_incoming(header_block, 0, body_end_block, i_next_reg);

    for (i, (name, _phi_reg)) in loop_carried_vars.iter().enumerate() {
        if let Some(&after_reg) = vars_after_body.get(name) {
            ctx.update_phi_incoming(header_block, 1 + i, body_end_block, after_reg);
        }
    }

    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![header_block], // Body is a successor of header
    );

    ctx.start_block(exit_block);

    // Exit from the header sees header phi values.
    for (name, phi_reg) in &loop_carried_vars {
        ctx.bind_var(name, *phi_reg);
    }
}

/// Lower a while loop to CFG with loop-carried phi nodes
///
/// CFG structure:
/// ```text
///            ┌─────────┐
///            │  entry   │
///            └────┬─────┘
///                 │
///          ┌──────▼──────┐
///          │ loop_header │◄────┐
///          │  eval cond  │    │
///          │ branch      │    │
///          └──────┬──────┘    │
///            true/ \false     │
///               /   \         │
///     ┌────────▼┐   ┌▼────┐  │
///     │  body   │   │exit │  │
///     │  ...    │   └─────┘  │
///     └────┬────┘            │
///          └─────────────────┘
/// ```
fn lower_while_loop<'src>(
    ctx: &mut LoweringContext<'src>,
    condition: &Spanned<TExpr<'src>>,
    invariant: Option<&Constraint>,
    body: &TBlock<'src>,
) {
    let entry_block = ctx.current_block().expect("Should be in a block");

    let header_block = ctx.new_block();
    let body_block = ctx.new_block();
    let exit_block = ctx.new_block();

    let vars_before_loop = ctx.snapshot_var_map();

    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![],
    );

    ctx.start_block(header_block);

    let mut loop_carried_vars: Vec<(String, VirtualReg)> = Vec::new();
    for (name, &before_reg) in &vars_before_loop {
        let original_ty = ctx.lookup_var_type(name);
        if matches!(&original_ty, IType::Array { .. }) && !ctx.is_owned_live(name) {
            continue;
        }
        let phi_reg = ctx.fresh_reg();

        // Preserve refined types as phi existentials.
        let existential = if let IType::RefinedInt { prop, .. } = &original_ty {
            use crate::dtal::convert::expr_to_constraint;
            if let Some(constraint) = expr_to_constraint(&prop.predicate.0) {
                let phi_witness = format!("_ex_v{}", phi_reg.0);
                let renamed = crate::backend::codegen::generator::substitute_constraint_vars(
                    &constraint,
                    &[(prop.var.clone(), phi_witness.clone())],
                );
                Some((phi_witness, renamed))
            } else {
                None
            }
        } else {
            None
        };

        let var_ty = widen_itype(original_ty);
        let mut phi = PhiNode::new(phi_reg, var_ty);
        phi.add_incoming(entry_block, before_reg);
        phi.existential_constraint = existential;
        ctx.emit_phi(phi);
        ctx.bind_var(name, phi_reg);
        loop_carried_vars.push((name.clone(), phi_reg));
    }

    let cond_reg = lower_expr(ctx, condition);

    // Arbitrary boolean conditions do not carry comparison constraints yet.
    ctx.finish_block(
        Terminator::Branch {
            cond: cond_reg,
            true_target: body_block,
            false_target: exit_block,
            true_constraint: Box::new(Constraint::True),
            false_constraint: Box::new(Constraint::True),
        },
        vec![entry_block],
    );

    ctx.start_block(body_block);
    ctx.enter_scope();

    if let Some(inv_constraint) = invariant {
        let subs = ctx.var_substitutions();
        let substituted =
            crate::backend::codegen::generator::substitute_constraint_vars(inv_constraint, &subs);
        ctx.emit(TirInstr::AssertConstraint {
            constraint: substituted,
        });
    }

    for stmt in &body.statements {
        lower_stmt(ctx, stmt);
    }

    ctx.emit_scope_exit_drops();

    let vars_after_body = ctx.snapshot_var_map();
    ctx.exit_scope();
    let body_end_block = ctx.current_block().expect("Should be in body block");

    for (i, (name, _phi_reg)) in loop_carried_vars.iter().enumerate() {
        if let Some(&after_reg) = vars_after_body.get(name) {
            ctx.update_phi_incoming(header_block, i, body_end_block, after_reg);
        }
    }

    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![header_block],
    );

    ctx.start_block(exit_block);

    for (name, phi_reg) in &loop_carried_vars {
        ctx.bind_var(name, *phi_reg);
    }
}

fn lower_region<'src>(ctx: &mut LoweringContext<'src>, body: &TBlock<'src>) {
    let region_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::RegionEnter { dst: region_reg });
    ctx.enter_region(region_reg);
    ctx.enter_scope();
    lower_stmts(ctx, &body.statements);
    ctx.emit_scope_exit_drops();
    ctx.exit_scope();
    ctx.exit_region();
    ctx.emit(TirInstr::RegionLeave { region: region_reg });
}
