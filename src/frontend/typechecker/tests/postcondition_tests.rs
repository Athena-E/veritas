use crate::common::ast::{Expr, Function, FunctionBody, Literal, Program, Type};
use crate::frontend::typechecker::{TypeError, check_program};
use chumsky::prelude::SimpleSpan;

type Spanned<T> = (T, SimpleSpan);

fn spanned<T>(value: T) -> Spanned<T> {
    (value, SimpleSpan::new(0, 0))
}

fn make_program<'src>(func: Function<'src>) -> Program<'src> {
    Program {
        constants: vec![],
        functions: vec![spanned(func)],
    }
}

fn int_type() -> Spanned<Type<'static>> {
    spanned(Type::Int)
}

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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(5))))),
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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(10))))),
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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(42))))),
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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(0))))),
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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(-1))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(result.is_err(), "Expected postcondition violation");
}

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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(42))))),
        },
    };

    let program = make_program(func);
    let result = check_program(&program);
    assert!(
        result.is_ok(),
        "Expected function without postcondition to pass"
    );
}

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
            trailing_expr: Some(Box::new(spanned(Expr::Literal(Literal::Int(0))))),
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
