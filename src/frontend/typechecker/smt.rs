use crate::common::ast::{BinOp, Expr, Literal, UnaryOp};
use crate::common::types::{IProposition, IValue};
use crate::frontend::typechecker::context::TypingContext;
use std::collections::HashMap;
use z3::ast::{Ast, Bool, Int};
use z3::{Config, Context, SatResult, Solver};

pub struct SmtOracle<'ctx> {
    context: &'ctx Context,
    solver: Solver<'ctx>,
    var_cache: HashMap<String, Int<'ctx>>,
}

impl<'ctx> SmtOracle<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let solver = Solver::new(context);
        Self {
            context,
            solver,
            var_cache: HashMap::new(),
        }
    }

    fn translate_ivalue(&mut self, value: &IValue) -> Int<'ctx> {
        match value {
            IValue::Int(n) => Int::from_i64(self.context, *n),
            IValue::Symbolic(name) => {
                self.var_cache
                    .entry(name.clone())
                    .or_insert_with(|| Int::new_const(self.context, name.as_ref()))
                    .clone()
            }
            IValue::Bool(_) => panic!("Boolean IValue in integer context (type error)"),
        }
    }

    fn translate_expr(&mut self, expr: &Expr) -> Int<'ctx> {
        match expr {
            Expr::Literal(Literal::Int(n)) => Int::from_i64(self.context, *n),

            Expr::Variable(name) => self
                .var_cache
                .entry(name.to_string())
                .or_insert_with(|| Int::new_const(self.context, name.as_ref()))
                .clone(),

            Expr::BinOp { op, lhs, rhs } => {
                let left = self.translate_expr(&lhs.0);
                let right = self.translate_expr(&rhs.0);

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

    fn translate_bool_expr(&mut self, expr: &Expr) -> Bool<'ctx> {
        match expr {
            Expr::Literal(Literal::Bool(b)) => Bool::from_bool(self.context, *b),

            Expr::BinOp { op, lhs, rhs } => match op {
                BinOp::Eq => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    left._eq(&right)
                }
                BinOp::NotEq => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    !left._eq(&right)
                }
                BinOp::Lt => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    left.lt(&right)
                }
                BinOp::Lte => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    left.le(&right)
                }
                BinOp::Gt => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    left.gt(&right)
                }
                BinOp::Gte => {
                    let left = self.translate_expr(&lhs.0);
                    let right = self.translate_expr(&rhs.0);
                    left.ge(&right)
                }

                BinOp::And => {
                    let left = self.translate_bool_expr(&lhs.0);
                    let right = self.translate_bool_expr(&rhs.0);
                    Bool::and(self.context, &[&left, &right])
                }
                BinOp::Or => {
                    let left = self.translate_bool_expr(&lhs.0);
                    let right = self.translate_bool_expr(&rhs.0);
                    Bool::or(self.context, &[&left, &right])
                }

                BinOp::Add | BinOp::Sub | BinOp::Mul => {
                    panic!("Arithmetic operation in boolean expression context")
                }
            },

            Expr::UnaryOp {
                op: UnaryOp::Not,
                cond,
            } => {
                let operand = self.translate_bool_expr(&cond.0);
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

    fn translate_proposition(&mut self, prop: &IProposition) -> Bool<'ctx> {
        let predicate_expr = &prop.predicate.0;
        self.translate_bool_expr(predicate_expr)
    }

    pub fn is_provable(&mut self, typing_ctx: &TypingContext, goal: &IProposition) -> bool {
        self.solver.reset();
        self.var_cache.clear();

        for prop in typing_ctx.get_propositions() {
            let constraint = self.translate_proposition(prop);
            self.solver.assert(&constraint);
        }

        let goal_formula = self.translate_proposition(goal);
        let negated_goal = goal_formula.not();
        self.solver.assert(&negated_goal);

        match self.solver.check() {
            SatResult::Unsat => {
                true
            }
            SatResult::Sat => {
                false
            }
            SatResult::Unknown => {
                false
            }
        }
    }
}

pub fn check_provable(typing_ctx: &TypingContext, goal: &IProposition) -> bool {
    let mut oracle = SmtOracle::new(&z3_ctx);
    oracle.is_provable(typing_ctx, goal)
}
