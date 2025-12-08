use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::types::IProposition;
use crate::frontend::typechecker::context::TypingContext;
use z3::ast::{Bool, Int};
use z3::{SatResult, Solver};

pub struct SmtOracle;

impl SmtOracle {
    pub fn new() -> Self {
        Self
    }

    fn translate_expr(expr: &Expr) -> Int {
        match expr {
            Expr::Literal(Literal::Int(n)) => Int::from_i64(*n),

            Expr::Variable(name) => Int::new_const(name.to_string()),

            Expr::BinOp { op, lhs, rhs } => {
                let left = Self::translate_expr(&lhs.0);
                let right = Self::translate_expr(&rhs.0);

                match op {
                    BinOp::Add => left + right,
                    BinOp::Sub => left - right,
                    BinOp::Mul => left * right,
                    _ => panic!("Comparison operator in integer expression context"),
                }
            }

            Expr::UnaryOp { .. } => {
                panic!("Unary operation in integer expression context")
            }

            Expr::Error => panic!("Error node in SMT translation"),
            Expr::Literal(Literal::Bool(_)) => panic!("Boolean literal in integer context"),
            Expr::Call { .. } => panic!("Function calls not yet supported in SMT"),
            Expr::Index { .. } => panic!("Array indexing not yet supported in SMT"),
            Expr::ArrayInit { .. } => panic!("Array initialization not yet supported in SMT"),
            Expr::If { .. } => panic!("If expressions not yet supported in SMT"),
        }
    }

    fn translate_bool_expr(expr: &Expr) -> Bool {
        match expr {
            Expr::Literal(Literal::Bool(b)) => Bool::from_bool(*b),

            Expr::BinOp { op, lhs, rhs } => match op {
                BinOp::Eq => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    Int::eq(&left, &right)
                }
                BinOp::NotEq => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    Int::eq(&left, &right).not()
                }
                BinOp::Lt => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    left.lt(&right)
                }
                BinOp::Lte => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    left.le(&right)
                }
                BinOp::Gt => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    left.gt(&right)
                }
                BinOp::Gte => {
                    let left = Self::translate_expr(&lhs.0);
                    let right = Self::translate_expr(&rhs.0);
                    left.ge(&right)
                }

                BinOp::And => {
                    let left = Self::translate_bool_expr(&lhs.0);
                    let right = Self::translate_bool_expr(&rhs.0);
                    Bool::and(&[&left, &right])
                }
                BinOp::Or => {
                    let left = Self::translate_bool_expr(&lhs.0);
                    let right = Self::translate_bool_expr(&rhs.0);
                    Bool::or(&[&left, &right])
                }

                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    panic!("Arithmetic operation in boolean expression context")
                }
            },

            Expr::UnaryOp {
                op: UnaryOp::Not,
                cond,
            } => {
                let operand = Self::translate_bool_expr(&cond.0);
                operand.not()
            }

            Expr::Error => panic!("Error node in SMT translation"),
            Expr::Literal(Literal::Int(_)) => panic!("Integer literal in boolean context"),
            Expr::Variable(_) => panic!("Variable in boolean context (need comparison)"),
            Expr::Call { .. } => panic!("Function calls not yet supported in SMT"),
            Expr::Index { .. } => panic!("Array indexing not yet supported in SMT"),
            Expr::ArrayInit { .. } => panic!("Array initialization not yet supported in SMT"),
            Expr::If { .. } => panic!("If expressions not yet supported in SMT"),
        }
    }

    fn translate_proposition(prop: &IProposition) -> Bool {
        let predicate_expr = &prop.predicate.0;
        Self::translate_bool_expr(predicate_expr)
    }

    pub fn is_provable(&self, typing_ctx: &TypingContext, goal: &IProposition) -> bool {
        let solver = Solver::new();

        // Add all context propositions as assumptions
        for prop in typing_ctx.get_propositions() {
            let constraint = Self::translate_proposition(prop);
            solver.assert(&constraint);
        }

        // Negate the goal - if unsatisfiable, the goal is provable
        let goal_formula = Self::translate_proposition(goal);
        let negated_goal = goal_formula.not();
        solver.assert(&negated_goal);

        match solver.check() {
            SatResult::Unsat => true,    // Goal is provable
            SatResult::Sat => false,     // Counterexample exists
            SatResult::Unknown => false, // Solver couldn't determine
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
