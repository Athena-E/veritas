//! Expression lowering from TAST to TIR
//!
//! This module converts typed expressions (`TExpr`) into sequences of
//! TIR instructions, returning the register holding the result.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg};
use crate::backend::lower::context::LoweringContext;
use crate::backend::tir::builder::{and_constraints, negate_constraint, or_constraints};
use crate::backend::tir::{
    BinaryOp, BlockId, BoundsProof, PhiNode, ProofJustification, Terminator, TirInstr, UnaryOp,
};
use crate::common::ast::{BinOp as AstBinOp, Literal, UnaryOp as AstUnaryOp};
use crate::common::span::Spanned;
use crate::common::tast::{TExpr, TStmt};
use crate::common::types::IType;

/// Convert a typed expression to an IndexExpr for constraints.
/// Returns None if the expression cannot be represented in the constraint domain.
pub(super) fn expr_to_index_expr<'src>(expr: &Spanned<TExpr<'src>>) -> Option<IndexExpr> {
    match &expr.0 {
        TExpr::Literal {
            value: Literal::Int(n),
            ..
        } => Some(IndexExpr::Const(*n)),

        TExpr::Variable { name, ty }
            if matches!(
                ty,
                IType::Int | IType::SingletonInt(_) | IType::RefinedInt { .. }
            ) =>
        {
            Some(IndexExpr::Var((*name).to_string()))
        }

        TExpr::BinOp {
            op: AstBinOp::Add,
            lhs,
            rhs,
            ..
        } => Some(IndexExpr::Add(
            Box::new(expr_to_index_expr(lhs)?),
            Box::new(expr_to_index_expr(rhs)?),
        )),

        TExpr::BinOp {
            op: AstBinOp::Sub,
            lhs,
            rhs,
            ..
        } => Some(IndexExpr::Sub(
            Box::new(expr_to_index_expr(lhs)?),
            Box::new(expr_to_index_expr(rhs)?),
        )),

        TExpr::BinOp {
            op: AstBinOp::Mul,
            lhs,
            rhs,
            ..
        } => Some(IndexExpr::Mul(
            Box::new(expr_to_index_expr(lhs)?),
            Box::new(expr_to_index_expr(rhs)?),
        )),

        _ => None,
    }
}

/// Convert a boolean typed expression to a Constraint.
/// Returns Constraint::True if the expression cannot be represented.
fn expr_to_constraint<'src>(expr: &Spanned<TExpr<'src>>) -> Constraint {
    match &expr.0 {
        // Boolean literals
        TExpr::Literal {
            value: Literal::Bool(true),
            ..
        } => Constraint::True,
        TExpr::Literal {
            value: Literal::Bool(false),
            ..
        } => Constraint::False,

        // Comparison operations
        TExpr::BinOp {
            op,
            lhs,
            rhs,
            ty: IType::Bool,
        } => match op {
            AstBinOp::Lt | AstBinOp::Lte | AstBinOp::Gt | AstBinOp::Gte | AstBinOp::Eq
            | AstBinOp::NotEq => {
                if let (Some(l), Some(r)) = (expr_to_index_expr(lhs), expr_to_index_expr(rhs)) {
                    match op {
                        AstBinOp::Lt => Constraint::Lt(l, r),
                        AstBinOp::Lte => Constraint::Le(l, r),
                        AstBinOp::Gt => Constraint::Gt(l, r),
                        AstBinOp::Gte => Constraint::Ge(l, r),
                        AstBinOp::Eq => Constraint::Eq(l, r),
                        AstBinOp::NotEq => Constraint::Ne(l, r),
                        _ => unreachable!(),
                    }
                } else {
                    Constraint::True
                }
            }
            AstBinOp::And => {
                and_constraints(expr_to_constraint(lhs), expr_to_constraint(rhs))
            }
            AstBinOp::Or => {
                or_constraints(expr_to_constraint(lhs), expr_to_constraint(rhs))
            }
            _ => Constraint::True,
        },

        // Negation
        TExpr::UnaryOp {
            op: AstUnaryOp::Not,
            operand,
            ..
        } => negate_constraint(expr_to_constraint(operand)),

        _ => Constraint::True,
    }
}

/// Lower a typed expression to TIR instructions
///
/// Returns the virtual register holding the result value.
pub fn lower_expr<'src>(
    ctx: &mut LoweringContext<'src>,
    expr: &Spanned<TExpr<'src>>,
) -> VirtualReg {
    match &expr.0 {
        TExpr::Error { ty } => {
            // Error recovery: emit a placeholder value
            let dst = ctx.fresh_reg();
            ctx.emit(TirInstr::LoadImm {
                dst,
                value: 0,
                ty: ty.clone(),
            });
            dst
        }

        TExpr::Literal { value, ty } => lower_literal(ctx, value, ty),

        TExpr::Variable { name, ty: _ } => {
            // Look up the variable in the current scope
            ctx.lookup_var(name)
                .unwrap_or_else(|| panic!("Undefined variable during lowering: {}", name))
        }

        TExpr::BinOp { op, lhs, rhs, ty } => lower_binop(ctx, *op, lhs, rhs, ty),

        TExpr::UnaryOp { op, operand, ty } => lower_unaryop(ctx, *op, operand, ty),

        TExpr::Call {
            func_name,
            args,
            ty,
        } => lower_call(ctx, func_name, args, ty),

        TExpr::Index { base, index, ty } => lower_index(ctx, base, index, ty),

        TExpr::ArrayInit { value, length, ty } => lower_array_init(ctx, value, length, ty),

        TExpr::If {
            cond,
            then_block,
            else_block,
            ty,
        } => {
            // Delegate to if-expression lowering (implemented in Step 4)
            lower_if_expr(ctx, cond, then_block, else_block.as_ref(), ty)
        }
    }
}

/// Lower a literal value
fn lower_literal<'src>(
    ctx: &mut LoweringContext<'src>,
    value: &Literal,
    ty: &IType<'src>,
) -> VirtualReg {
    let dst = ctx.fresh_reg();

    match value {
        Literal::Int(n) => {
            ctx.emit(TirInstr::LoadImm {
                dst,
                value: *n,
                ty: ty.clone(),
            });
        }
        Literal::Bool(b) => {
            // Booleans are represented as 0/1 with Bool type
            ctx.emit(TirInstr::LoadImm {
                dst,
                value: if *b { 1 } else { 0 },
                ty: ty.clone(),
            });
        }
    }

    dst
}

/// Convert AST binary operator to TIR binary operator
fn convert_binop(op: AstBinOp) -> BinaryOp {
    match op {
        AstBinOp::Add => BinaryOp::Add,
        AstBinOp::Sub => BinaryOp::Sub,
        AstBinOp::Mul => BinaryOp::Mul,
        AstBinOp::Eq => BinaryOp::Eq,
        AstBinOp::NotEq => BinaryOp::Ne,
        AstBinOp::Lt => BinaryOp::Lt,
        AstBinOp::Lte => BinaryOp::Le,
        AstBinOp::Gt => BinaryOp::Gt,
        AstBinOp::Gte => BinaryOp::Ge,
        AstBinOp::And => BinaryOp::And,
        AstBinOp::Or => BinaryOp::Or,
    }
}

/// Lower a binary operation
fn lower_binop<'src>(
    ctx: &mut LoweringContext<'src>,
    op: AstBinOp,
    lhs: &Spanned<TExpr<'src>>,
    rhs: &Spanned<TExpr<'src>>,
    ty: &IType<'src>,
) -> VirtualReg {
    let lhs_reg = lower_expr(ctx, lhs);
    let rhs_reg = lower_expr(ctx, rhs);
    let dst = ctx.fresh_reg();

    ctx.emit(TirInstr::BinOp {
        dst,
        op: convert_binop(op),
        lhs: lhs_reg,
        rhs: rhs_reg,
        ty: ty.clone(),
    });

    dst
}

/// Convert AST unary operator to TIR unary operator
fn convert_unaryop(op: AstUnaryOp) -> UnaryOp {
    match op {
        AstUnaryOp::Not => UnaryOp::Not,
    }
}

/// Lower a unary operation
fn lower_unaryop<'src>(
    ctx: &mut LoweringContext<'src>,
    op: AstUnaryOp,
    operand: &Spanned<TExpr<'src>>,
    ty: &IType<'src>,
) -> VirtualReg {
    let operand_reg = lower_expr(ctx, operand);
    let dst = ctx.fresh_reg();

    ctx.emit(TirInstr::UnaryOp {
        dst,
        op: convert_unaryop(op),
        operand: operand_reg,
        ty: ty.clone(),
    });

    dst
}

/// Lower a function call
fn lower_call<'src>(
    ctx: &mut LoweringContext<'src>,
    func_name: &str,
    args: &[Spanned<TExpr<'src>>],
    ty: &IType<'src>,
) -> VirtualReg {
    // Lower all arguments
    let arg_regs: Vec<VirtualReg> = args.iter().map(|arg| lower_expr(ctx, arg)).collect();

    // For unit-returning functions, don't try to capture the return value
    if matches!(ty, IType::Unit) {
        ctx.emit(TirInstr::Call {
            dst: None,
            func: func_name.to_string(),
            args: arg_regs,
            result_ty: ty.clone(),
        });

        // Return a dummy unit value
        let dst = ctx.fresh_reg();
        ctx.emit(TirInstr::LoadImm {
            dst,
            value: 0,
            ty: IType::Unit,
        });
        dst
    } else {
        let dst = ctx.fresh_reg();
        ctx.emit(TirInstr::Call {
            dst: Some(dst),
            func: func_name.to_string(),
            args: arg_regs,
            result_ty: ty.clone(),
        });
        dst
    }
}

/// Lower an array index expression (array access)
fn lower_index<'src>(
    ctx: &mut LoweringContext<'src>,
    base: &Spanned<TExpr<'src>>,
    index: &Spanned<TExpr<'src>>,
    ty: &IType<'src>,
) -> VirtualReg {
    let base_reg = lower_expr(ctx, base);
    let index_reg = lower_expr(ctx, index);
    let dst = ctx.fresh_reg();

    // Create a bounds proof - in the frontend, bounds have already been verified
    // We trust the frontend's type checking here
    let bounds_proof = BoundsProof {
        constraint: Constraint::True, // Placeholder - frontend verified
        justification: ProofJustification::FromFrontend,
    };

    ctx.emit(TirInstr::ArrayLoad {
        dst,
        base: base_reg,
        index: index_reg,
        element_ty: ty.clone(),
        bounds_proof,
    });

    dst
}

/// Lower an array initialization expression
fn lower_array_init<'src>(
    ctx: &mut LoweringContext<'src>,
    value: &Spanned<TExpr<'src>>,
    length: &Spanned<TExpr<'src>>,
    ty: &IType<'src>,
) -> VirtualReg {
    // Get the array size (must be a constant for now)
    let size = match &length.0 {
        TExpr::Literal {
            value: Literal::Int(n),
            ..
        } => *n,
        _ => panic!("Array size must be a constant integer"),
    };

    // Get element type from the array type
    let element_ty = match ty {
        IType::Array { element_type, .. } => (**element_type).clone(),
        _ => panic!("Expected array type for ArrayInit"),
    };

    // Allocate the array
    let arr_reg = ctx.fresh_reg();
    ctx.emit(TirInstr::AllocArray {
        dst: arr_reg,
        element_ty: element_ty.clone(),
        size,
    });

    // Lower the initialization value
    let init_val_reg = lower_expr(ctx, value);

    // Initialize each element with a loop
    // For now, we unroll for small arrays or use a simple loop
    // This is a simplified version - just store to each index
    for i in 0..size {
        let idx_reg = ctx.fresh_reg();
        ctx.emit(TirInstr::LoadImm {
            dst: idx_reg,
            value: i,
            ty: IType::Int,
        });

        let bounds_proof = BoundsProof {
            constraint: Constraint::And(
                Box::new(Constraint::Ge(IndexExpr::Const(i), IndexExpr::Const(0))),
                Box::new(Constraint::Lt(IndexExpr::Const(i), IndexExpr::Const(size))),
            ),
            justification: ProofJustification::FromFrontend,
        };

        ctx.emit(TirInstr::ArrayStore {
            base: arr_reg,
            index: idx_reg,
            value: init_val_reg,
            bounds_proof,
        });
    }

    arr_reg
}

/// Lower an if expression to control flow with phi nodes
///
/// CFG structure:
/// ```text
///             ┌─────────┐
///             │  cond   │  (current block)
///             └────┬────┘
///            true/ \false
///               /   \
///     ┌────────▼┐   ┌▼────────┐
///     │  then   │   │  else   │
///     └────┬────┘   └────┬────┘
///          \           /
///           \         /
///          ┌─▼───────▼─┐
///          │   merge   │
///          │  φ(v1,v2) │
///          └───────────┘
/// ```
pub fn lower_if_expr<'src>(
    ctx: &mut LoweringContext<'src>,
    cond: &Spanned<TExpr<'src>>,
    then_stmts: &[Spanned<TStmt<'src>>],
    else_stmts: Option<&Vec<Spanned<TStmt<'src>>>>,
    ty: &IType<'src>,
) -> VirtualReg {
    // 1. Lower the condition expression in the current block
    let cond_reg = lower_expr(ctx, cond);
    let cond_block = ctx.current_block().expect("Should be in a block");

    // 2. Create blocks for then, else, and merge
    let then_block = ctx.new_block();
    let else_block = ctx.new_block();
    let merge_block = ctx.new_block();

    // 3. Derive constraints from the condition expression
    let true_constraint = expr_to_constraint(cond);
    let false_constraint = negate_constraint(true_constraint.clone());

    // 4. Finish the condition block with a branch
    ctx.finish_block(
        Terminator::Branch {
            cond: cond_reg,
            true_target: then_block,
            false_target: else_block,
            true_constraint,
            false_constraint,
        },
        vec![], // predecessors filled by builder
    );

    // 5. Lower the then branch
    ctx.start_block(then_block);

    // Snapshot variable state before then branch
    let vars_before_then = ctx.snapshot_var_map();

    // Lower all statements except possibly the last (which might be the result expr)
    let then_result = lower_block_with_result(ctx, then_stmts, ty);

    // Record then block's end for phi
    let then_end_block = ctx.current_block().expect("Should be in then block");
    let vars_after_then = ctx.snapshot_var_map();

    // Jump to merge
    ctx.finish_block(
        Terminator::Jump {
            target: merge_block,
        },
        vec![cond_block],
    );

    // 5. Lower the else branch
    ctx.start_block(else_block);

    // Restore variable state to before the if (for else branch)
    // The else branch should see variables as they were BEFORE the then branch,
    // not after. This ensures proper phi node creation at the merge point.
    ctx.restore_var_map(vars_before_then.clone());

    let else_result = if let Some(else_stmts) = else_stmts {
        lower_block_with_result(ctx, else_stmts, ty)
    } else {
        // No else block - produce a default value
        // For Unit type, this is fine; for other types this shouldn't happen
        // (type checker should have caught it)
        let dst = ctx.fresh_reg();
        ctx.emit(TirInstr::LoadImm {
            dst,
            value: 0,
            ty: ty.clone(),
        });
        dst
    };

    let else_end_block = ctx.current_block().expect("Should be in else block");
    let vars_after_else = ctx.snapshot_var_map();

    // Jump to merge
    ctx.finish_block(
        Terminator::Jump {
            target: merge_block,
        },
        vec![cond_block],
    );

    // 6. Create merge block with phi nodes
    ctx.start_block(merge_block);

    // Create phi node for the result value
    let result_reg = ctx.fresh_reg();
    let mut result_phi = PhiNode::new(result_reg, ty.clone());
    result_phi.add_incoming(then_end_block, then_result);
    result_phi.add_incoming(else_end_block, else_result);
    ctx.emit_phi(result_phi);

    // Create phi nodes for any variables modified in either branch
    // Compare vars_after_then and vars_after_else with vars_before_then
    create_phi_nodes_for_modified_vars(
        ctx,
        &vars_before_then,
        &vars_after_then,
        &vars_after_else,
        then_end_block,
        else_end_block,
    );

    result_reg
}

/// Lower a block of statements and return the result register
///
/// If the last statement is an expression statement, that's the result.
/// Otherwise, return a unit/default value.
fn lower_block_with_result<'src>(
    ctx: &mut LoweringContext<'src>,
    stmts: &[Spanned<TStmt<'src>>],
    ty: &IType<'src>,
) -> VirtualReg {
    use crate::backend::lower::stmt::lower_stmt;

    if stmts.is_empty() {
        // Empty block - produce default value
        let dst = ctx.fresh_reg();
        ctx.emit(TirInstr::LoadImm {
            dst,
            value: 0,
            ty: ty.clone(),
        });
        return dst;
    }

    // Lower all statements except the last
    for stmt in &stmts[..stmts.len() - 1] {
        lower_stmt(ctx, stmt);
    }

    // Check if last statement is an expression
    let last = &stmts[stmts.len() - 1];
    match &last.0 {
        TStmt::Expr(expr) => {
            // The expression's value is the block's result
            lower_expr(ctx, expr)
        }
        _ => {
            // Not an expression - lower it and return default
            lower_stmt(ctx, last);
            let dst = ctx.fresh_reg();
            ctx.emit(TirInstr::LoadImm {
                dst,
                value: 0,
                ty: ty.clone(),
            });
            dst
        }
    }
}

/// Create phi nodes for variables that were modified differently in two branches
fn create_phi_nodes_for_modified_vars<'src>(
    ctx: &mut LoweringContext<'src>,
    vars_before: &std::collections::HashMap<String, VirtualReg>,
    vars_after_then: &std::collections::HashMap<String, VirtualReg>,
    vars_after_else: &std::collections::HashMap<String, VirtualReg>,
    then_block: BlockId,
    else_block: BlockId,
) {
    // Find variables that were modified in either branch
    for (name, &before_reg) in vars_before {
        let then_reg = vars_after_then.get(name).copied().unwrap_or(before_reg);
        let else_reg = vars_after_else.get(name).copied().unwrap_or(before_reg);

        // If the variable has different values in then vs else (or vs before)
        if then_reg != else_reg {
            // Need a phi node
            let phi_dst = ctx.fresh_reg();
            // We don't have type info here - use Int as placeholder
            // In a production compiler, we'd track types in the var_map
            let mut phi = PhiNode::new(phi_dst, IType::Int);
            phi.add_incoming(then_block, then_reg);
            phi.add_incoming(else_block, else_reg);
            ctx.emit_phi(phi);

            // Update var_map to point to the phi result
            ctx.bind_var(name, phi_dst);
        } else if then_reg != before_reg {
            // Both branches modified it the same way - just update binding
            ctx.bind_var(name, then_reg);
        }
    }
}
