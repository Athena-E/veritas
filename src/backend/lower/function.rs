//! Function lowering from TAST to TIR
//!
//! This module converts typed functions (`TFunction`) into TIR functions
//! with CFG in SSA form.

use crate::backend::dtal::VirtualReg;
use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::lower::context::LoweringContext;
use crate::backend::lower::expr::lower_expr;
use crate::backend::lower::stmt::lower_stmts;
use crate::backend::tir::{Terminator, TirFunction};
use crate::common::ast::{BinOp, Expr, Literal};
use crate::common::tast::TFunction;
use crate::common::types::{IProposition, IType};

/// Lower a typed function to TIR
pub fn lower_function<'src>(func: &TFunction<'src>) -> TirFunction<'src> {
    let mut ctx = LoweringContext::new();

    // Create the entry block
    let entry_block = ctx.new_block();
    ctx.start_block(entry_block);

    // Allocate registers for parameters and bind them
    let mut params: Vec<(VirtualReg, IType<'src>)> = Vec::new();
    for param in &func.parameters {
        let reg = ctx.fresh_reg();
        ctx.bind_var(&param.name, reg);
        params.push((reg, param.ty.clone()));
    }

    // Lower the function body statements
    lower_stmts(&mut ctx, &func.body.statements);

    // Lower the return expression (if any) and create the return terminator
    let return_value = func
        .body
        .return_expr
        .as_ref()
        .map(|return_expr| lower_expr(&mut ctx, return_expr));

    // Finish the entry block with a return terminator
    ctx.finish_block(
        Terminator::Return {
            value: return_value,
        },
        vec![], // Entry block has no predecessors
    );

    // Convert postcondition from IProposition to Constraint
    let postcondition = func
        .postcondition
        .as_ref()
        .and_then(|prop| proposition_to_constraint(prop));

    // Build and return the function
    ctx.build_function(
        func.name.clone(),
        params,
        func.return_type.clone(),
        None, // TODO: Convert precondition from IProposition to Constraint
        postcondition,
        entry_block,
    )
}

/// Convert an IProposition to a Constraint
fn proposition_to_constraint(prop: &IProposition) -> Option<Constraint> {
    expr_to_constraint(&prop.predicate.0)
}

/// Convert an Expr to a Constraint (for boolean expressions)
fn expr_to_constraint(expr: &Expr) -> Option<Constraint> {
    match expr {
        Expr::BinOp { op, lhs, rhs } => {
            match op {
                // Comparison operators -> Constraint
                BinOp::Eq => Some(Constraint::Eq(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::NotEq => Some(Constraint::Ne(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Lt => Some(Constraint::Lt(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Lte => Some(Constraint::Le(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Gt => Some(Constraint::Gt(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                BinOp::Gte => Some(Constraint::Ge(
                    expr_to_index(&lhs.0)?,
                    expr_to_index(&rhs.0)?,
                )),
                // Logical operators
                BinOp::And => Some(Constraint::And(
                    Box::new(expr_to_constraint(&lhs.0)?),
                    Box::new(expr_to_constraint(&rhs.0)?),
                )),
                BinOp::Or => Some(Constraint::Or(
                    Box::new(expr_to_constraint(&lhs.0)?),
                    Box::new(expr_to_constraint(&rhs.0)?),
                )),
                // Arithmetic operators can't be converted to constraints directly
                _ => None,
            }
        }
        Expr::Literal(Literal::Bool(true)) => Some(Constraint::True),
        Expr::Literal(Literal::Bool(false)) => Some(Constraint::False),
        _ => None,
    }
}

/// Convert an Expr to an IndexExpr (for arithmetic expressions)
fn expr_to_index(expr: &Expr) -> Option<IndexExpr> {
    match expr {
        Expr::Literal(Literal::Int(n)) => Some(IndexExpr::Const(*n)),
        Expr::Variable(name) => Some(IndexExpr::Var(name.to_string())),
        Expr::BinOp { op, lhs, rhs } => {
            let l = expr_to_index(&lhs.0)?;
            let r = expr_to_index(&rhs.0)?;
            match op {
                BinOp::Add => Some(IndexExpr::Add(Box::new(l), Box::new(r))),
                BinOp::Sub => Some(IndexExpr::Sub(Box::new(l), Box::new(r))),
                BinOp::Mul => Some(IndexExpr::Mul(Box::new(l), Box::new(r))),
                _ => None,
            }
        }
        _ => None,
    }
}
