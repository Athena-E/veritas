//! Statement lowering from TAST to TIR
//!
//! This module converts typed statements (`TStmt`) into TIR instructions
//! and control flow structures.

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::backend::lower::context::LoweringContext;
use crate::backend::lower::expr::lower_expr;
use crate::backend::tir::{
    BinaryOp, BlockId, BoundsProof, PhiNode, ProofJustification, Terminator, TirInstr,
};
use crate::common::span::Spanned;
use crate::common::tast::{TExpr, TStmt};
use crate::common::types::IType;

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
        } => {
            lower_let(ctx, name, value, checked_ty);
        }

        TStmt::Assignment { lhs, rhs } => {
            lower_assignment(ctx, lhs, rhs);
        }

        TStmt::Return { expr } => {
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
    _ty: &IType<'src>,
) {
    // Lower the value expression
    let value_reg = lower_expr(ctx, value);

    // Bind the variable name to this register
    ctx.bind_var(name, value_reg);
}

/// Lower an assignment statement
fn lower_assignment<'src>(
    ctx: &mut LoweringContext<'src>,
    lhs: &Spanned<TExpr<'src>>,
    rhs: &Spanned<TExpr<'src>>,
) {
    match &lhs.0 {
        TExpr::Variable { name, ty } => {
            // Simple variable assignment
            // In SSA, we create a new register and update the binding
            let rhs_reg = lower_expr(ctx, rhs);

            // Create a copy to a new register (SSA form)
            let new_reg = ctx.fresh_reg();
            ctx.emit(TirInstr::Copy {
                dst: new_reg,
                src: rhs_reg,
                ty: ty.clone(),
            });

            // Update the variable binding
            ctx.bind_var(name, new_reg);
        }

        TExpr::Index { base, index, ty: _ } => {
            // Array element assignment: arr[i] = value
            let base_reg = lower_expr(ctx, base);
            let index_reg = lower_expr(ctx, index);
            let rhs_reg = lower_expr(ctx, rhs);

            // Create bounds proof (frontend verified)
            let bounds_proof = BoundsProof {
                constraint: Constraint::True,
                justification: ProofJustification::FromFrontend,
            };

            ctx.emit(TirInstr::ArrayStore {
                base: base_reg,
                index: index_reg,
                value: rhs_reg,
                bounds_proof,
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
    _invariant: Option<&Spanned<TExpr<'src>>>,
    body: &[Spanned<TStmt<'src>>],
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
    ctx.emit_phi(i_phi);

    // Bind the loop variable to the phi result
    ctx.bind_var(var, i_phi_reg);

    // Create phi nodes for ALL existing mutable variables (loop-carried state)
    // These phi nodes will be at indices 1, 2, 3, ... in the header block
    // We need these so that variables modified in the loop body carry their
    // values across iterations.
    let mut loop_carried_vars: Vec<(String, VirtualReg)> = Vec::new();
    for (name, &before_reg) in &vars_before_loop {
        if name != var {
            // Create phi node for this variable
            let phi_reg = ctx.fresh_reg();
            let mut phi = PhiNode::new(phi_reg, IType::Int); // TODO: track actual types
            phi.add_incoming(entry_block, before_reg);
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

    // Branch: if i < end, go to body; else go to exit
    ctx.finish_block(
        Terminator::Branch {
            cond: cmp_reg,
            true_target: body_block,
            false_target: exit_block,
            true_constraint: Constraint::True, // TODO: derive constraint
            false_constraint: Constraint::True,
        },
        vec![entry_block], // Predecessor from entry's jump
    );

    // 5. Lower the body block
    ctx.start_block(body_block);

    // Lower body statements
    for stmt in body {
        lower_stmt(ctx, stmt);
    }

    // Snapshot variables after body
    let vars_after_body = ctx.snapshot_var_map();

    // 6. Increment loop variable: i_next = i + 1
    let one_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::LoadImm {
        dst: one_reg,
        value: 1,
        ty: var_ty.clone(),
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

/// Create phi nodes for variables modified in a loop
fn create_loop_exit_phi_nodes<'src>(
    ctx: &mut LoweringContext<'src>,
    vars_before: &std::collections::HashMap<String, crate::backend::dtal::VirtualReg>,
    vars_after_body: &std::collections::HashMap<String, crate::backend::dtal::VirtualReg>,
    _header_block: BlockId,
    _body_end_block: BlockId,
) {
    // For variables modified in the loop body, we need phi nodes
    // at the exit point to select between "never entered loop" and "after loop iterations"
    for (name, &before_reg) in vars_before {
        if let Some(&after_reg) = vars_after_body.get(name)
            && before_reg != after_reg
        {
            // Variable was modified in the loop
            // At exit, we use the value from the header's phi (for loop-carried state)
            // For simplicity, just update binding to the after value
            // (A full implementation would need proper loop phi handling)
            ctx.bind_var(name, after_reg);
        }
    }
}
