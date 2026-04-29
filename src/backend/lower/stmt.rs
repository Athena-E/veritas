//! Statement lowering from TAST to TIR
//!
//! This module converts typed statements (`TStmt`) into TIR instructions
//! and control flow structures.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg};
use crate::backend::lower::context::{LoweringContext, ScalarBorrowBinding};
use crate::backend::lower::expr::{expr_to_index_expr, lower_expr};
use crate::backend::lower::widen_itype;
use crate::backend::tir::{BinaryOp, PhiNode, Terminator, TirInstr};
use crate::common::ownership::OwnershipMode;
use crate::common::span::Spanned;
use crate::common::tast::{TBlock, TExpr, TStmt};
use crate::common::types::IType;

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
            declared_ty: _,
            value,
            checked_ty,
            ownership,
        } => {
            lower_let(ctx, name, value, checked_ty, *ownership);
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
            // Expression statement - evaluate for side effects
            lower_expr(ctx, expr);
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
        && let TExpr::Borrow { expr, .. } = &value.0
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
            ctx.create_scalar_borrow_value(owner_reg, pointee_ty.clone(), kind);
        ctx.bind_scalar_borrow(
            name,
            borrow_reg,
            lowered_ref_ty,
            ScalarBorrowBinding {
                owner_name: owner.clone(),
                cell_reg,
                kind,
                pointee_ty,
            },
        );
        return;
    }

    // Lower the value expression
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

    // Bind the variable name to this register with its type
    ctx.bind_var_typed(name, bound_reg, ty.clone());
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
            // Simple variable assignment
            // In SSA, we create a new register and update the binding
            let prior_reg = ctx.lookup_var(name);
            let prior_ty = ctx.lookup_var_type(name);
            let self_assignment =
                matches!(&rhs.0, TExpr::Variable { name: rhs_name, .. } if rhs_name == name);
            if matches!(ctx.lookup_var_type(name), IType::Ref(_) | IType::RefMut(_))
                && let TExpr::Borrow { expr, .. } = &rhs.0
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
                    ctx.create_scalar_borrow_value(owner_reg, pointee_ty.clone(), kind);
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
                        pointee_ty,
                    },
                );
                return;
            }

            let rhs_reg = lower_expr(ctx, rhs);

            // Create a copy to a new register (SSA form)
            // Use the RHS type, not the LHS declared type (which may be a stale singleton)
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

            // Update the variable binding with the RHS type
            ctx.bind_var_typed(name, new_reg, rhs.0.get_type().clone());
        }

        TExpr::Index { base, index, ty: _ } => {
            // Array element assignment: arr[i] = value
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

    // Note: We don't finish the block here because the function lowering
    // handles block termination. We just record that we have a return value.
    // The actual Return terminator is added by the function lowering logic.

    // For now, we'll store this in a temporary - the function lowering
    // will handle creating the actual Return terminator.
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
    // 1. In the entry block (current), lower start and end expressions
    let start_reg = lower_expr(ctx, start);
    let end_reg = lower_expr(ctx, end);
    let entry_block = ctx.current_block().expect("Should be in a block");

    // 2. Create blocks for header, body, and exit
    let header_block = ctx.new_block();
    let body_block = ctx.new_block();
    let exit_block = ctx.new_block();

    // Snapshot variable state before the loop
    let vars_before_loop = ctx.snapshot_var_map();

    // Jump from entry to header
    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![],
    );

    // 3. Start the header block
    ctx.start_block(header_block);

    // Create phi node for loop variable (index 0)
    // Initially: i_phi comes from entry (start_reg) or body (i_next)
    let i_phi_reg = ctx.fresh_reg();
    let mut i_phi = PhiNode::new(i_phi_reg, var_ty.clone());
    i_phi.add_incoming(entry_block, start_reg);

    // Attach existential constraint: ∃n. int(n) where (n >= start ∧ n < end)
    // The witness variable uses the register name so codegen can emit it directly.
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

    // Bind the loop variable to the phi result
    ctx.bind_var_typed(var, i_phi_reg, var_ty.clone());

    // Create phi nodes for ALL existing mutable variables (loop-carried state)
    // These phi nodes will be at indices 1, 2, 3, ... in the header block
    // We need these so that variables modified in the loop body carry their
    // values across iterations.
    let mut loop_carried_vars: Vec<(String, VirtualReg)> = Vec::new();
    for (name, &before_reg) in &vars_before_loop {
        if name != var {
            let phi_reg = ctx.fresh_reg();
            let original_ty = ctx.lookup_var_type(name);
            if matches!(&original_ty, IType::Array { .. }) && !ctx.is_owned_live(name) {
                continue;
            }

            // For refined types, preserve the refinement as an existential
            // constraint on the phi node. This allows the verifier to derive
            // the refinement from the type rather than losing it at the join.
            let existential = if let IType::RefinedInt { prop, .. } = &original_ty {
                use crate::backend::dtal::convert::expr_to_constraint;
                if let Some(constraint) = expr_to_constraint(&prop.predicate.0) {
                    // Use a fresh witness name that can't collide with register
                    // names (vN). The verifier opens the existential by
                    // substituting this witness with the register name.
                    let phi_witness = format!("_ex_v{}", phi_reg.0);
                    // Rename the refinement variable to the witness name
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

            // Widen the IType for the phi's type field (the existential
            // is carried separately via existential_constraint).
            let var_ty = widen_itype(original_ty);
            let mut phi = PhiNode::new(phi_reg, var_ty);
            phi.add_incoming(entry_block, before_reg);
            phi.existential_constraint = existential;
            ctx.emit_phi(phi);
            // Update var binding to use phi result
            ctx.bind_var(name, phi_reg);
            loop_carried_vars.push((name.clone(), phi_reg));
        }
    }

    // 4. Compare i < end
    let cmp_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::BinOp {
        dst: cmp_reg,
        op: BinaryOp::Lt,
        lhs: i_phi_reg,
        rhs: end_reg,
        ty: IType::Bool,
    });

    // Derive constraints for the loop condition: start <= i < end
    // Use register name (v3) for the loop variable so constraints match
    // the register-based verification context.
    let loop_var_idx = IndexExpr::Var(format!("v{}", i_phi_reg.0));
    let start_idx = expr_to_index_expr(start).unwrap_or(IndexExpr::Const(0));
    let end_idx = expr_to_index_expr(end).unwrap_or(IndexExpr::Const(i64::MAX as i128));
    // True branch (body): i >= start AND i < end
    let true_constraint = Constraint::And(
        Box::new(Constraint::Ge(loop_var_idx.clone(), start_idx)),
        Box::new(Constraint::Lt(loop_var_idx.clone(), end_idx.clone())),
    );
    let false_constraint = Constraint::Ge(loop_var_idx.clone(), end_idx);

    // Branch: if i < end, go to body; else go to exit
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

    // 5. Lower the body block
    ctx.start_block(body_block);
    ctx.enter_scope();

    // Emit loop invariant assertion (if present) in the body block,
    // where the ConstraintAssume provides loop counter bounds.
    if let Some(inv_constraint) = invariant {
        let subs = ctx.var_substitutions();
        let substituted =
            crate::backend::codegen::generator::substitute_constraint_vars(inv_constraint, &subs);
        ctx.emit(TirInstr::AssertConstraint {
            constraint: substituted,
        });
    }

    // Lower body statements
    for stmt in &body.statements {
        lower_stmt(ctx, stmt);
    }

    ctx.emit_scope_exit_drops();

    // Snapshot variables after body
    let vars_after_body = ctx.snapshot_var_map();
    ctx.exit_scope();

    // 6. Increment loop variable: i_next = i + 1
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

    // 7. Update the loop variable phi node with the body's incoming edge
    // The phi node is the first (index 0) phi in the header block
    ctx.update_phi_incoming(header_block, 0, body_end_block, i_next_reg);

    // 8. Update loop-carried variable phi nodes with body's incoming edges
    // Phi nodes are at indices 1, 2, 3... (after the loop variable at index 0)
    for (i, (name, _phi_reg)) in loop_carried_vars.iter().enumerate() {
        if let Some(&after_reg) = vars_after_body.get(name) {
            // phi_index = 1 + i (loop variable is at index 0)
            ctx.update_phi_incoming(header_block, 1 + i, body_end_block, after_reg);
        }
    }

    // Jump back to header
    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![header_block], // Body is a successor of header
    );

    // 9. Start the exit block
    ctx.start_block(exit_block);

    // At the exit block, bind variables to their header phi registers
    // This is correct because we exit from the header, so we use the header's phi values
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

    // Create blocks for header, body, and exit
    let header_block = ctx.new_block();
    let body_block = ctx.new_block();
    let exit_block = ctx.new_block();

    // Snapshot variable state before the loop
    let vars_before_loop = ctx.snapshot_var_map();

    // Jump from entry to header
    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![],
    );

    // Start the header block
    ctx.start_block(header_block);

    // Create phi nodes for ALL existing mutable variables (loop-carried state)
    let mut loop_carried_vars: Vec<(String, VirtualReg)> = Vec::new();
    for (name, &before_reg) in &vars_before_loop {
        let original_ty = ctx.lookup_var_type(name);
        if matches!(&original_ty, IType::Array { .. }) && !ctx.is_owned_live(name) {
            continue;
        }
        let phi_reg = ctx.fresh_reg();

        // Preserve refined types as existential constraints on phi nodes
        let existential = if let IType::RefinedInt { prop, .. } = &original_ty {
            use crate::backend::dtal::convert::expr_to_constraint;
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

    // Evaluate the condition in the header
    let cond_reg = lower_expr(ctx, condition);

    // Branch: if condition true, go to body; else go to exit
    // Use Constraint::True / Constraint::True as placeholders since the
    // condition is an arbitrary boolean expression (not a simple comparison)
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

    // Lower the body block
    ctx.start_block(body_block);
    ctx.enter_scope();

    // Emit loop invariant assertion if present
    if let Some(inv_constraint) = invariant {
        let subs = ctx.var_substitutions();
        let substituted =
            crate::backend::codegen::generator::substitute_constraint_vars(inv_constraint, &subs);
        ctx.emit(TirInstr::AssertConstraint {
            constraint: substituted,
        });
    }

    // Lower body statements
    for stmt in &body.statements {
        lower_stmt(ctx, stmt);
    }

    ctx.emit_scope_exit_drops();

    // Snapshot variables after body
    let vars_after_body = ctx.snapshot_var_map();
    ctx.exit_scope();
    let body_end_block = ctx.current_block().expect("Should be in body block");

    // Update loop-carried variable phi nodes with body's incoming edges
    for (i, (name, _phi_reg)) in loop_carried_vars.iter().enumerate() {
        if let Some(&after_reg) = vars_after_body.get(name) {
            ctx.update_phi_incoming(header_block, i, body_end_block, after_reg);
        }
    }

    // Jump back to header
    ctx.finish_block(
        Terminator::Jump {
            target: header_block,
        },
        vec![header_block],
    );

    // Start the exit block
    ctx.start_block(exit_block);

    // At exit, bind variables to their header phi registers
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
