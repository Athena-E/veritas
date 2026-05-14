use super::x86_to_dtal_reg;
use crate::backend::regalloc::allocator::AllocationResult;
use crate::backend::x86_64::regs::Location;
use crate::dtal::constraints::{Constraint, IndexExpr};
use crate::dtal::types::DtalType;
use std::collections::HashMap;
use std::sync::Arc;

/// Build a mapping from virtual register names to physical register names.
fn build_vreg_name_map(alloc: &AllocationResult) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for (vreg, loc) in alloc.allocation.iter() {
        let vname = format!("v{}", vreg.0);
        if let Location::Reg(x86) = loc {
            let preg = x86_to_dtal_reg(*x86);
            let pname = format!("{}", preg);
            map.insert(vname, pname);
        }
    }
    map
}

/// Remap virtual register names in an index expression.
fn remap_index_expr(expr: &IndexExpr, name_map: &HashMap<String, String>) -> IndexExpr {
    match expr {
        IndexExpr::Const(n) => IndexExpr::Const(*n),
        IndexExpr::Var(name) => {
            let new_name = name_map.get(name).cloned().unwrap_or_else(|| name.clone());
            IndexExpr::Var(new_name)
        }
        IndexExpr::Add(l, r) => IndexExpr::Add(
            Box::new(remap_index_expr(l, name_map)),
            Box::new(remap_index_expr(r, name_map)),
        ),
        IndexExpr::Sub(l, r) => IndexExpr::Sub(
            Box::new(remap_index_expr(l, name_map)),
            Box::new(remap_index_expr(r, name_map)),
        ),
        IndexExpr::Mul(l, r) => IndexExpr::Mul(
            Box::new(remap_index_expr(l, name_map)),
            Box::new(remap_index_expr(r, name_map)),
        ),
        IndexExpr::Div(l, r) => IndexExpr::Div(
            Box::new(remap_index_expr(l, name_map)),
            Box::new(remap_index_expr(r, name_map)),
        ),
        IndexExpr::Mod(l, r) => IndexExpr::Mod(
            Box::new(remap_index_expr(l, name_map)),
            Box::new(remap_index_expr(r, name_map)),
        ),
        IndexExpr::Select(name, idx) => {
            let new_name = name_map.get(name).cloned().unwrap_or_else(|| name.clone());
            IndexExpr::Select(new_name, Box::new(remap_index_expr(idx, name_map)))
        }
    }
}

/// Remap virtual register names in a constraint.
pub(super) fn remap_constraint(c: &Constraint, name_map: &HashMap<String, String>) -> Constraint {
    match c {
        Constraint::True => Constraint::True,
        Constraint::False => Constraint::False,
        Constraint::Eq(l, r) => {
            Constraint::Eq(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::Lt(l, r) => {
            Constraint::Lt(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::Le(l, r) => {
            Constraint::Le(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::Gt(l, r) => {
            Constraint::Gt(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::Ge(l, r) => {
            Constraint::Ge(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::Ne(l, r) => {
            Constraint::Ne(remap_index_expr(l, name_map), remap_index_expr(r, name_map))
        }
        Constraint::And(l, r) => Constraint::And(
            Box::new(remap_constraint(l, name_map)),
            Box::new(remap_constraint(r, name_map)),
        ),
        Constraint::Or(l, r) => Constraint::Or(
            Box::new(remap_constraint(l, name_map)),
            Box::new(remap_constraint(r, name_map)),
        ),
        Constraint::Not(c) => Constraint::Not(Box::new(remap_constraint(c, name_map))),
        Constraint::Implies(l, r) => Constraint::Implies(
            Box::new(remap_constraint(l, name_map)),
            Box::new(remap_constraint(r, name_map)),
        ),
        Constraint::Forall {
            var,
            lower,
            upper,
            body,
        } => Constraint::Forall {
            var: name_map.get(var).cloned().unwrap_or_else(|| var.clone()),
            lower: remap_index_expr(lower, name_map),
            upper: remap_index_expr(upper, name_map),
            body: Box::new(remap_constraint(body, name_map)),
        },
        Constraint::Exists {
            var,
            lower,
            upper,
            body,
        } => Constraint::Exists {
            var: name_map.get(var).cloned().unwrap_or_else(|| var.clone()),
            lower: remap_index_expr(lower, name_map),
            upper: remap_index_expr(upper, name_map),
            body: Box::new(remap_constraint(body, name_map)),
        },
    }
}

/// Remap virtual register names in a constraint assertion.
pub(super) fn remap_constraint_vars(
    constraint: &Constraint,
    alloc: &AllocationResult,
) -> Constraint {
    let name_map = build_vreg_name_map(alloc);
    remap_constraint(constraint, &name_map)
}

/// Remap virtual register names within type constraints.
pub(super) fn remap_constraint_vars_in_type(ty: &DtalType, alloc: &AllocationResult) -> DtalType {
    let name_map = build_vreg_name_map(alloc);
    remap_type(ty, &name_map)
}

fn remap_type(ty: &DtalType, name_map: &HashMap<String, String>) -> DtalType {
    match ty {
        DtalType::SingletonInt(idx) => DtalType::SingletonInt(remap_index_expr(idx, name_map)),
        DtalType::RefinedInt {
            base,
            var,
            constraint,
        } => DtalType::RefinedInt {
            base: Arc::new(remap_type(base, name_map)),
            var: name_map.get(var).cloned().unwrap_or_else(|| var.clone()),
            constraint: remap_constraint(constraint, name_map),
        },
        DtalType::ExistentialInt {
            witness_var,
            constraint,
        } => DtalType::ExistentialInt {
            witness_var: name_map
                .get(witness_var)
                .cloned()
                .unwrap_or_else(|| witness_var.clone()),
            constraint: remap_constraint(constraint, name_map),
        },
        DtalType::Array { element_type, size } => DtalType::Array {
            element_type: Arc::new(remap_type(element_type, name_map)),
            size: remap_index_expr(size, name_map),
        },
        _ => ty.clone(),
    }
}
