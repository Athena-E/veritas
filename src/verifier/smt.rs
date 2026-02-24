//! Z3-based constraint oracle for the DTAL verifier
//!
//! This module provides SMT-based constraint provability checking,
//! replacing the permissive syntactic fallback with a sound decision procedure.

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use z3::ast::{Bool, Int};
use z3::{FuncDecl, SatResult, Solver, Sort};

/// Z3-based constraint oracle for the verifier
pub struct ConstraintOracle;

impl ConstraintOracle {
    /// Translate an IndexExpr into a Z3 integer expression
    fn translate_index_expr(expr: &IndexExpr) -> Int {
        match expr {
            IndexExpr::Const(n) => Int::from_i64(*n),
            IndexExpr::Var(s) => Int::new_const(s.to_string()),
            IndexExpr::Add(l, r) => Self::translate_index_expr(l) + Self::translate_index_expr(r),
            IndexExpr::Sub(l, r) => Self::translate_index_expr(l) - Self::translate_index_expr(r),
            IndexExpr::Mul(l, r) => Self::translate_index_expr(l) * Self::translate_index_expr(r),
            IndexExpr::Select(name, idx) => {
                let func_name = format!("f_{}", name);
                let int_sort = Sort::int();
                let func = FuncDecl::new(func_name.as_str(), &[&int_sort], &int_sort);
                let index = Self::translate_index_expr(idx);
                func.apply(&[&index]).as_int().unwrap()
            }
        }
    }

    /// Translate a Constraint into a Z3 boolean expression
    fn translate_constraint(c: &Constraint) -> Bool {
        match c {
            Constraint::True => Bool::from_bool(true),
            Constraint::False => Bool::from_bool(false),
            Constraint::Eq(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                Int::eq(&left, &right)
            }
            Constraint::Ne(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                Int::eq(&left, &right).not()
            }
            Constraint::Lt(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                left.lt(&right)
            }
            Constraint::Le(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                left.le(&right)
            }
            Constraint::Gt(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                left.gt(&right)
            }
            Constraint::Ge(l, r) => {
                let left = Self::translate_index_expr(l);
                let right = Self::translate_index_expr(r);
                left.ge(&right)
            }
            Constraint::And(l, r) => {
                let left = Self::translate_constraint(l);
                let right = Self::translate_constraint(r);
                Bool::and(&[&left, &right])
            }
            Constraint::Or(l, r) => {
                let left = Self::translate_constraint(l);
                let right = Self::translate_constraint(r);
                Bool::or(&[&left, &right])
            }
            Constraint::Not(c) => Self::translate_constraint(c).not(),
            Constraint::Implies(l, r) => {
                let left = Self::translate_constraint(l);
                let right = Self::translate_constraint(r);
                left.implies(&right)
            }
            Constraint::Forall {
                var,
                lower,
                upper,
                body,
            } => {
                let bound = Int::new_const(var.to_string());
                let lo = Self::translate_index_expr(lower);
                let hi = Self::translate_index_expr(upper);
                let range_guard = Bool::and(&[&bound.ge(&lo), &bound.lt(&hi)]);
                let body_formula = Self::translate_constraint(body);
                z3::ast::forall_const(&[&bound], &[], &range_guard.implies(&body_formula))
            }
            Constraint::Exists {
                var,
                lower,
                upper,
                body,
            } => {
                let bound = Int::new_const(var.to_string());
                let lo = Self::translate_index_expr(lower);
                let hi = Self::translate_index_expr(upper);
                let range_guard = Bool::and(&[&bound.ge(&lo), &bound.lt(&hi)]);
                let body_formula = Self::translate_constraint(body);
                z3::ast::exists_const(
                    &[&bound],
                    &[],
                    &Bool::and(&[&range_guard, &body_formula]),
                )
            }
        }
    }

    /// Check if a goal constraint is provable from a set of context constraints.
    ///
    /// Uses the standard "negate and check unsatisfiability" pattern:
    /// if context /\ !goal is UNSAT, then context |= goal (the goal is provable).
    pub fn is_provable(goal: &Constraint, context: &[Constraint]) -> bool {
        let solver = Solver::new();

        // Assert all context constraints
        for ctx in context {
            let formula = Self::translate_constraint(ctx);
            solver.assert(&formula);
        }

        // Negate the goal
        let goal_formula = Self::translate_constraint(goal);
        let negated_goal = goal_formula.not();
        solver.assert(&negated_goal);

        match solver.check() {
            SatResult::Unsat => true,    // Goal is provable
            SatResult::Sat => false,     // Counterexample exists
            SatResult::Unknown => false, // Solver couldn't determine
        }
    }
}
