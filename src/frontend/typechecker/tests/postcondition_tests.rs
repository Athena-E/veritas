// Tests for postcondition (ensures clause) verification

use crate::common::ast::{Expr, Function, FunctionBody, Literal, Program, Type};
use crate::frontend::typechecker::{TypeError, check_program};
use chumsky::prelude::SimpleSpan;

type Spanned<T> = (T, SimpleSpan);

fn spanned<T>(value: T) -> Spanned<T> {
    (value, SimpleSpan::new(0, 0))
}

/// Helper to create a program with a single function
fn make_program<'src>(func: Function<'src>) -> Program<'src> {
    Program {
        functions: vec![spanned(func)],
    }
}

/// Helper to create a simple return type
fn int_type() -> Spanned<Type<'static>> {
    spanned(Type::Int)
}

/// Test: fn five() -> int ensures result == 5 { 5 }
/// Should PASS - return value satisfies postcondition
#[test]
fn test_postcondition_singleton_satisfied() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Eq,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(5)))),
    });

    let func = Function {
        name: "five",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(5))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(
        result.is_ok(),
        "Expected postcondition to pass, got: {:?}",
        result.err()
    );
}

/// Test: fn wrong() -> int ensures result == 5 { 10 }
/// Should FAIL - return value violates postcondition
#[test]
fn test_postcondition_singleton_violated() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Eq,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(5)))),
    });

    let func = Function {
        name: "wrong",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(10))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(result.is_err(), "Expected postcondition violation");
    assert!(matches!(
        result.unwrap_err(),
        TypeError::PostconditionViolation { .. }
    ));
}

/// Test: fn positive() -> int ensures result > 0 { 42 }
/// Should PASS - 42 > 0 is provable
#[test]
fn test_postcondition_inequality_satisfied() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Gt,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(0)))),
    });

    let func = Function {
        name: "positive",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(42))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(
        result.is_ok(),
        "Expected postcondition to pass, got: {:?}",
        result.err()
    );
}

/// Test: fn non_positive() -> int ensures result > 0 { 0 }
/// Should FAIL - 0 > 0 is false
#[test]
fn test_postcondition_inequality_violated() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Gt,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(0)))),
    });

    let func = Function {
        name: "non_positive",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(0))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(result.is_err(), "Expected postcondition violation");
    assert!(matches!(
        result.unwrap_err(),
        TypeError::PostconditionViolation { .. }
    ));
}

/// Test: fn negative() -> int ensures result > 0 { -1 }
/// Should FAIL - -1 > 0 is false
#[test]
fn test_postcondition_negative_violated() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Gt,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(0)))),
    });

    let func = Function {
        name: "negative",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(-1))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(result.is_err(), "Expected postcondition violation");
}

/// Test: fn no_postcond() -> int { 42 }
/// Should PASS - no postcondition to violate
#[test]
fn test_no_postcondition() {
    let func = Function {
        name: "no_postcond",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(42))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(
        result.is_ok(),
        "Expected function without postcondition to pass"
    );
}

/// Test: fn gte_zero() -> int ensures result >= 0 { 0 }
/// Should PASS - 0 >= 0 is true
#[test]
fn test_postcondition_gte_satisfied() {
    let postcond = spanned(Expr::BinOp {
        op: crate::common::ast::BinOp::Gte,
        lhs: Box::new(spanned(Expr::Variable("result"))),
        rhs: Box::new(spanned(Expr::Literal(Literal::Int(0)))),
    });

    let func = Function {
        name: "gte_zero",
        parameters: vec![],
        return_type: int_type(),
        precondition: None,
        postcondition: Some(postcond),
        body: FunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(0))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(
        result.is_ok(),
        "Expected postcondition to pass, got: {:?}",
        result.err()
    );
}
