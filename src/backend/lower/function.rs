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
use crate::backend::tir::{Terminator, TirFunction, TirInstr};
use crate::common::ownership::OwnershipMode;
use crate::common::tast::{TExpr, TFunction};
use crate::common::types::{IProposition, IType};

/// Lower a typed function to TIR
pub fn lower_function<'src>(func: &TFunction<'src>) -> TirFunction<'src> {
    let mut ctx = LoweringContext::new();

    // Create the entry block
    let entry_block = ctx.new_block();
    ctx.start_block(entry_block);

    // Allocate registers for parameters and bind them
    let mut params: Vec<(VirtualReg, IType<'src>)> = Vec::new();
    let mut param_names: Vec<String> = Vec::new();
    for param in &func.parameters {
        let reg = ctx.fresh_reg();
        ctx.bind_var_typed(&param.name, reg, param.ty.clone());
        params.push((reg, param.ty.clone()));
        param_names.push(param.name.clone());
    }

    // Lower the function body statements
    lower_stmts(&mut ctx, &func.body.statements);

    // Lower the return expression (if any) and create the return terminator
    let return_value = func
        .body
        .trailing_expr
        .as_ref()
        .map(|trailing_expr| {
            let value_reg = lower_expr(&mut ctx, trailing_expr);
            if func.returns_owned && matches!(&trailing_expr.0, TExpr::Variable { .. }) {
                let moved_reg = ctx.fresh_reg();
                ctx.emit(TirInstr::MoveOwned {
                    dst: moved_reg,
                    src: value_reg,
                    ty: func.return_type.clone(),
                });
                moved_reg
            } else {
                value_reg
            }
        });

    // Finish the entry block with a return terminator
    ctx.finish_block(
        Terminator::Return {
            value: return_value,
            ownership: if func.returns_owned {
                OwnershipMode::Owned
            } else {
                OwnershipMode::Plain
            },
        },
        vec![], // Entry block has no predecessors
    );

    // Lower precondition, but skip quantified preconditions (forall/exists)
    // since the verifier can't yet reason about array contents
    let precondition = func
        .precondition
        .as_ref()
        .and_then(|prop| proposition_to_constraint(prop))
        .filter(|c| !contains_quantifier(c));

    let postcondition = func
        .postcondition
        .as_ref()
        .and_then(|prop| proposition_to_constraint(prop));

    // Build and return the function
    ctx.build_function(
        func.name.clone(),
        params,
        param_names,
        func.return_type.clone(),
        func.returns_owned,
        precondition,
        postcondition,
        entry_block,
    )
}

/// Convert an IProposition to a Constraint
pub(super) fn proposition_to_constraint(prop: &IProposition) -> Option<Constraint> {
    expr_to_constraint(&prop.predicate.0)
}

/// Check if a constraint contains quantifiers (forall/exists)
fn contains_quantifier(c: &Constraint) -> bool {
    match c {
        Constraint::Forall { .. } | Constraint::Exists { .. } => true,
        Constraint::And(l, r) | Constraint::Or(l, r) | Constraint::Implies(l, r) => {
            contains_quantifier(l) || contains_quantifier(r)
        }
        Constraint::Not(inner) => contains_quantifier(inner),
        _ => false,
    }
}
