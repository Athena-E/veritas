//! Function lowering from TAST to TIR
//!
//! This module converts typed functions (`TFunction`) into TIR functions
//! with CFG in SSA form.

use crate::backend::dtal::VirtualReg;
use crate::backend::dtal::constraints::Constraint;
use crate::backend::dtal::convert::expr_to_constraint;
use crate::backend::lower::context::LoweringContext;
use crate::backend::lower::expr::lower_expr;
use crate::backend::lower::stmt::lower_stmts;
use crate::backend::tir::{Terminator, TirFunction};
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
        .trailing_expr
        .as_ref()
        .map(|trailing_expr| lower_expr(&mut ctx, trailing_expr));

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
pub(super) fn proposition_to_constraint(prop: &IProposition) -> Option<Constraint> {
    expr_to_constraint(&prop.predicate.0)
}
