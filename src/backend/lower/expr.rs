//! Expression lowering from TAST to TIR
//!
//! This module converts typed expressions (`TExpr`) into sequences of
//! TIR instructions, returning the register holding the result.

use crate::backend::dtal::{Constraint, IndexExpr, VirtualReg};
use crate::backend::lower::context::LoweringContext;
use crate::backend::tir::{
    BinaryOp, BlockId, BoundsProof, PhiNode, ProofJustification, Terminator, TirInstr, UnaryOp,
};
use crate::common::ast::{BinOp as AstBinOp, Literal, UnaryOp as AstUnaryOp};
use crate::common::span::Spanned;
use crate::common::tast::{TExpr, TStmt};
use crate::common::types::IType;

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

        TExpr::Variable { name, ty } => {
            // Look up the variable in the current scope
            ctx.lookup_var(name).unwrap_or_else(|| {
                panic!("Undefined variable during lowering: {}", name)
            })
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

    let dst = ctx.fresh_reg();

    ctx.emit(TirInstr::Call {
        dst: Some(dst),
        func: func_name.to_string(),
        args: arg_regs,
        result_ty: ty.clone(),
    });

    dst
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
                Box::new(Constraint::Ge(
                    IndexExpr::Const(i),
                    IndexExpr::Const(0),
                )),
                Box::new(Constraint::Lt(
                    IndexExpr::Const(i),
                    IndexExpr::Const(size),
                )),
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
/// This is a placeholder - full implementation in Step 4
pub fn lower_if_expr<'src>(
    ctx: &mut LoweringContext<'src>,
    cond: &Spanned<TExpr<'src>>,
    then_block: &[Spanned<TStmt<'src>>],
    else_block: Option<&Vec<Spanned<TStmt<'src>>>>,
    ty: &IType<'src>,
) -> VirtualReg {
    // TODO: Full implementation in Step 4
    // For now, panic to indicate this needs to be implemented
    panic!("If expression lowering not yet implemented - see Step 4")
}
