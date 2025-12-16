//! Statement lowering from TAST to TIR
//!
//! This module converts typed statements (`TStmt`) into TIR instructions
//! and control flow structures.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg};
use crate::backend::lower::context::LoweringContext;
use crate::backend::lower::expr::lower_expr;
use crate::backend::tir::{BoundsProof, ProofJustification, Terminator, TirInstr};
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
            // TODO: Full implementation in Step 5
            panic!("For loop lowering not yet implemented - see Step 5")
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

        TExpr::Index { base, index, ty } => {
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
    let value_reg = lower_expr(ctx, expr);

    // Note: We don't finish the block here because the function lowering
    // handles block termination. We just record that we have a return value.
    // The actual Return terminator is added by the function lowering logic.

    // For now, we'll store this in a temporary - the function lowering
    // will handle creating the actual Return terminator.
}
