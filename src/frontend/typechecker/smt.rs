use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::types::{IProposition, IType, IValue};
use crate::frontend::typechecker::context::TypingContext;
use crate::frontend::typechecker::helpers::{rename_expr_var, substitute_expr_for_var};
use chumsky::prelude::SimpleSpan;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;
use z3::ast::{Array as Z3Array, Bool, Int};
use z3::{SatResult, Solver, Sort};

// Global counters for SMT query instrumentation
static FRONTEND_SMT_QUERIES: AtomicU64 = AtomicU64::new(0);
static FRONTEND_SMT_TIME_NS: AtomicU64 = AtomicU64::new(0);

/// Reset frontend SMT counters (call before compilation)
pub fn reset_frontend_smt_stats() {
    FRONTEND_SMT_QUERIES.store(0, Ordering::Relaxed);
    FRONTEND_SMT_TIME_NS.store(0, Ordering::Relaxed);
}

/// Get (query_count, total_time_ns) for frontend SMT queries
pub fn get_frontend_smt_stats() -> (u64, u64) {
    (
        FRONTEND_SMT_QUERIES.load(Ordering::Relaxed),
        FRONTEND_SMT_TIME_NS.load(Ordering::Relaxed),
    )
}

pub struct SmtOracle;

impl SmtOracle {
    pub fn new() -> Self {
        Self
    }

    /// Translate an integer-valued expression to Z3.
    /// Returns None if the expression contains forms not yet supported in SMT
    /// (function calls, if-expressions, etc.). Callers treat None as "unprovable".
    fn translate_expr(expr: &Expr) -> Option<Int> {
        match expr {
            Expr::Literal(Literal::Int(n)) => Some(Int::from_i64(*n)),

            Expr::Variable(name) => Some(Int::new_const(name.to_string())),

            Expr::BinOp { op, lhs, rhs } => {
                let left = Self::translate_expr(&lhs.0)?;
                let right = Self::translate_expr(&rhs.0)?;

                match op {
                    BinOp::Add => Some(left + right),
                    BinOp::Sub => Some(left - right),
                    BinOp::Mul => Some(left * right),
                    BinOp::Div => Some(left / right),
                    BinOp::Mod => Some(left % right),
                    // Comparison operators produce booleans, not integers
                    _ => None,
                }
            }

            Expr::UnaryOp { op, cond } => match op {
                UnaryOp::Neg => {
                    let operand = Self::translate_expr(&cond.0)?;
                    Some(-operand)
                }
                UnaryOp::Not => None,
            },

            Expr::Index { base, index } => {
                // Encode array indexing using Z3 array theory: arr[i] -> select(arr, i)
                if let Expr::Variable(name) = &base.0 {
                    let int_sort = Sort::int();
                    let arr = Z3Array::new_const(name.to_string(), &int_sort, &int_sort);
                    let idx = Self::translate_expr(&index.0)?;
                    Some(arr.select(&idx).as_int().unwrap())
                } else {
                    None
                }
            }

            Expr::Error
            | Expr::Literal(Literal::Bool(_))
            | Expr::Call { .. }
            | Expr::ArrayInit { .. }
            | Expr::If { .. }
            | Expr::Forall { .. }
            | Expr::Exists { .. } => None,
        }
    }

    /// Translate a boolean-valued expression to Z3.
    /// Returns None if the expression contains unsupported forms.
    fn translate_bool_expr(expr: &Expr) -> Option<Bool> {
        match expr {
            Expr::Literal(Literal::Bool(b)) => Some(Bool::from_bool(*b)),

            Expr::BinOp { op, lhs, rhs } => match op {
                BinOp::Eq => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(Int::eq(&left, &right))
                }
                BinOp::NotEq => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(Int::eq(&left, &right).not())
                }
                BinOp::Lt => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(left.lt(&right))
                }
                BinOp::Lte => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(left.le(&right))
                }
                BinOp::Gt => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(left.gt(&right))
                }
                BinOp::Gte => {
                    let left = Self::translate_expr(&lhs.0)?;
                    let right = Self::translate_expr(&rhs.0)?;
                    Some(left.ge(&right))
                }

                BinOp::And => {
                    let left = Self::translate_bool_expr(&lhs.0)?;
                    let right = Self::translate_bool_expr(&rhs.0)?;
                    Some(Bool::and(&[&left, &right]))
                }
                BinOp::Or => {
                    let left = Self::translate_bool_expr(&lhs.0)?;
                    let right = Self::translate_bool_expr(&rhs.0)?;
                    Some(Bool::or(&[&left, &right]))
                }
                BinOp::Implies => {
                    let left = Self::translate_bool_expr(&lhs.0)?;
                    let right = Self::translate_bool_expr(&rhs.0)?;
                    Some(left.implies(&right))
                }

                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => None,
            },

            Expr::UnaryOp { op, cond } => match op {
                UnaryOp::Not => {
                    let operand = Self::translate_bool_expr(&cond.0)?;
                    Some(operand.not())
                }
                UnaryOp::Neg => None,
            },

            Expr::Forall {
                var,
                start,
                end,
                body,
            } => {
                let bound = Int::new_const(var.to_string());
                let lo = Self::translate_expr(&start.0)?;
                let hi = Self::translate_expr(&end.0)?;
                let range_guard = Bool::and(&[&bound.ge(&lo), &bound.lt(&hi)]);
                let body_formula = Self::translate_bool_expr(&body.0)?;
                Some(z3::ast::forall_const(
                    &[&bound],
                    &[],
                    &range_guard.implies(&body_formula),
                ))
            }

            Expr::Exists {
                var,
                start,
                end,
                body,
            } => {
                let bound = Int::new_const(var.to_string());
                let lo = Self::translate_expr(&start.0)?;
                let hi = Self::translate_expr(&end.0)?;
                let range_guard = Bool::and(&[&bound.ge(&lo), &bound.lt(&hi)]);
                let body_formula = Self::translate_bool_expr(&body.0)?;
                Some(z3::ast::exists_const(
                    &[&bound],
                    &[],
                    &Bool::and(&[&range_guard, &body_formula]),
                ))
            }

            Expr::Error
            | Expr::Literal(Literal::Int(_))
            | Expr::Variable(_)
            | Expr::Call { .. }
            | Expr::Index { .. }
            | Expr::ArrayInit { .. }
            | Expr::If { .. } => None,
        }
    }

    /// Translate a proposition to Z3. Returns None if untranslatable.
    fn translate_proposition(prop: &IProposition) -> Option<Bool> {
        let predicate_expr = &prop.predicate.0;
        Self::translate_bool_expr(predicate_expr)
    }

    pub fn is_provable(&self, typing_ctx: &TypingContext, goal: &IProposition) -> bool {
        let start = Instant::now();

        let solver = Solver::new();

        // Add all context propositions as assumptions.
        // Skip any that can't be translated (conservative: less context
        // means fewer things provable, which is sound).
        for prop in typing_ctx.get_propositions() {
            if let Some(constraint) = Self::translate_proposition(prop) {
                solver.assert(&constraint);
            }
        }

        // Add refinements from variable types
        // For a variable `n: {v: int | v > 0}`, we add the constraint `n > 0`
        for (var_name, ty) in typing_ctx.get_all_variable_types() {
            if let Some(prop) = Self::extract_refinement_as_proposition(var_name, ty)
                && let Some(constraint) = Self::translate_proposition(&prop)
            {
                solver.assert(&constraint);
            }
        }

        // Negate the goal - if unsatisfiable, the goal is provable.
        // If the goal can't be translated, conservatively return false.
        let goal_formula = match Self::translate_proposition(goal) {
            Some(f) => f,
            None => {
                let elapsed = start.elapsed().as_nanos() as u64;
                FRONTEND_SMT_QUERIES.fetch_add(1, Ordering::Relaxed);
                FRONTEND_SMT_TIME_NS.fetch_add(elapsed, Ordering::Relaxed);
                return false;
            }
        };
        let negated_goal = goal_formula.not();
        solver.assert(&negated_goal);

        let result = match solver.check() {
            SatResult::Unsat => true,    // Goal is provable
            SatResult::Sat => false,     // Counterexample exists
            SatResult::Unknown => false, // Solver couldn't determine
        };

        let elapsed = start.elapsed().as_nanos() as u64;
        FRONTEND_SMT_QUERIES.fetch_add(1, Ordering::Relaxed);
        FRONTEND_SMT_TIME_NS.fetch_add(elapsed, Ordering::Relaxed);

        result
    }

    /// Extract a proposition from a refined type by substituting the variable name
    /// For `n: {v: int | v > 0}`, produces the proposition `n > 0`
    pub fn extract_refinement_as_proposition<'src>(
        var_name: &str,
        ty: &IType<'src>,
    ) -> Option<IProposition<'src>> {
        match ty {
            IType::RefinedInt { prop, .. } => {
                // Substitute the actual variable name for the bound variable
                let renamed_predicate = rename_expr_var(&prop.predicate.0, &prop.var, var_name);
                Some(IProposition {
                    var: var_name.to_string(),
                    predicate: Arc::new((renamed_predicate, prop.predicate.1)),
                })
            }
            IType::SingletonInt(value) => {
                // For singleton types, add equality constraint: var_name = value
                let dummy_span = SimpleSpan::new(0, 0);
                let var_leaked: &'src str = Box::leak(var_name.to_string().into_boxed_str());
                let value_expr = match value {
                    IValue::Int(n) => Expr::Literal(Literal::Int(*n)),
                    IValue::Symbolic(s) => {
                        let s_leaked: &'src str = Box::leak(s.clone().into_boxed_str());
                        Expr::Variable(s_leaked)
                    }
                    IValue::Bool(b) => Expr::Literal(Literal::Bool(*b)),
                };
                let eq_expr = Expr::BinOp {
                    op: BinOp::Eq,
                    lhs: Box::new((Expr::Variable(var_leaked), dummy_span)),
                    rhs: Box::new((value_expr, dummy_span)),
                };
                Some(IProposition {
                    var: var_name.to_string(),
                    predicate: Arc::new((eq_expr, dummy_span)),
                })
            }
            IType::Array { element_type, size } => {
                let size_int = match size {
                    IValue::Int(n) => Some(*n),
                    _ => None,
                };
                let size_int = size_int?;

                let dummy_span = SimpleSpan::new(0, 0);
                let var_leaked: &'src str = Box::leak(var_name.to_string().into_boxed_str());
                let idx_var: &'src str = Box::leak("__idx".to_string().into_boxed_str());

                // Build arr[__idx]
                let arr_index = Expr::Index {
                    base: Box::new((Expr::Variable(var_leaked), dummy_span)),
                    index: Box::new((Expr::Variable(idx_var), dummy_span)),
                };

                // Build the quantifier body depending on element type
                let body = match element_type.as_ref() {
                    // arr: [{v:int|v >= 0}; N] => forall __idx in 0..N { arr[__idx] >= 0 }
                    IType::RefinedInt { prop, .. } => {
                        substitute_expr_for_var(&prop.predicate.0, &prop.var, &arr_index)
                    }
                    // arr: [int(K); N] => forall __idx in 0..N { arr[__idx] == K }
                    IType::SingletonInt(IValue::Int(n)) => Expr::BinOp {
                        op: BinOp::Eq,
                        lhs: Box::new((arr_index, dummy_span)),
                        rhs: Box::new((Expr::Literal(Literal::Int(*n)), dummy_span)),
                    },
                    _ => return None,
                };

                let forall_expr = Expr::Forall {
                    var: idx_var,
                    start: Box::new((Expr::Literal(Literal::Int(0)), dummy_span)),
                    end: Box::new((Expr::Literal(Literal::Int(size_int)), dummy_span)),
                    body: Box::new((body, dummy_span)),
                };

                Some(IProposition {
                    var: var_name.to_string(),
                    predicate: Arc::new((forall_expr, dummy_span)),
                })
            }
            _ => None,
        }
    }
}

impl Default for SmtOracle {
    fn default() -> Self {
        Self::new()
    }
}

pub fn check_provable(typing_ctx: &TypingContext, goal: &IProposition) -> bool {
    let oracle = SmtOracle::new();
    oracle.is_provable(typing_ctx, goal)
}
