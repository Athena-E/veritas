use crate::common::ast::{Expr, Function, FunctionBody, Parameter, Program, Stmt, Type};
use crate::common::ownership::BorrowKind;
use crate::common::tast::TExpr;
use crate::frontend::typechecker::{TypeError, check_program};
use chumsky::prelude::SimpleSpan;

type Spanned<T> = (T, SimpleSpan);

fn spanned<T>(value: T) -> Spanned<T> {
    (value, SimpleSpan::new(0, 0))
}

fn int_type() -> Spanned<Type<'static>> {
    spanned(Type::Int)
}

fn ref_int_type() -> Spanned<Type<'static>> {
    spanned(Type::Ref(Box::new(int_type())))
}

fn ref_mut_int_type() -> Spanned<Type<'static>> {
    spanned(Type::RefMut(Box::new(int_type())))
}

fn int_array_type(size: i128) -> Spanned<Type<'static>> {
    spanned(Type::Array {
        element_type: Box::new(int_type()),
        size: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(size)))),
    })
}

fn make_program<'src>(functions: Vec<Function<'src>>) -> Program<'src> {
    Program {
        constants: vec![],
        functions: functions.into_iter().map(spanned).collect(),
    }
}

#[test]
fn shared_borrow_expression_typechecks() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "rx",
                    ty: ref_int_type(),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Shared,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    }),
                }),
            ],
            trailing_expr: None,
        },
    };

    let tast = check_program(&make_program(vec![func])).expect("shared borrow should typecheck");
    let main = &tast.functions[0];
    match &main.body.statements[1].0 {
        crate::common::tast::TStmt::Let { value, .. } => match &value.0 {
            TExpr::Borrow { kind, .. } => assert_eq!(*kind, BorrowKind::Shared),
            other => panic!("expected typed borrow, got {:?}", other),
        },
        other => panic!("expected let statement, got {:?}", other),
    }
}

#[test]
fn mutable_borrow_expression_typechecks() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: true,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "rx",
                    ty: ref_mut_int_type(),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Mutable,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    }),
                }),
            ],
            trailing_expr: None,
        },
    };

    check_program(&make_program(vec![func])).expect("mutable borrow should typecheck");
}

#[test]
fn borrow_of_temporary_is_rejected() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![spanned(Stmt::Let {
                is_mut: false,
                name: "rx",
                ty: ref_int_type(),
                value: spanned(Expr::Borrow {
                    kind: BorrowKind::Shared,
                    expr: Box::new(spanned(Expr::BinOp {
                        op: crate::common::ast::BinOp::Add,
                        lhs: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(
                            1,
                        )))),
                        rhs: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(
                            2,
                        )))),
                    })),
                }),
            })],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("borrowing a temporary should be rejected");
    assert!(matches!(err, TypeError::UnsupportedFeature { .. }));
}

#[test]
fn returning_references_is_rejected() {
    let func = Function {
        name: "bad",
        parameters: vec![],
        return_type: ref_int_type(),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![spanned(Stmt::Let {
                is_mut: false,
                name: "x",
                ty: int_type(),
                value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
            })],
            trailing_expr: Some(Box::new(spanned(Expr::Borrow {
                kind: BorrowKind::Shared,
                expr: Box::new(spanned(Expr::Variable("x"))),
            }))),
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("returning references should be rejected");
    assert!(matches!(err, TypeError::UnsupportedFeature { .. }));
}

#[test]
fn storing_references_inside_arrays_is_rejected() {
    let array_of_refs = spanned(Type::Array {
        element_type: Box::new(ref_int_type()),
        size: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(1)))),
    });

    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "arr",
                    ty: array_of_refs,
                    value: spanned(Expr::ArrayInit {
                        value: Box::new(spanned(Expr::Borrow {
                            kind: BorrowKind::Shared,
                            expr: Box::new(spanned(Expr::Variable("x"))),
                        })),
                        length: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(
                            1,
                        )))),
                    }),
                }),
            ],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("arrays of references should be rejected in the first slice");
    assert!(matches!(err, TypeError::UnsupportedFeature { .. }));
}

#[test]
fn shared_borrow_parameter_call_typechecks() {
    let inspect = Function {
        name: "inspect",
        parameters: vec![spanned(Parameter {
            name: "r",
            ty: ref_int_type(),
        })],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![],
            trailing_expr: None,
        },
    };

    let main = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Expr(spanned(Expr::Call {
                    func_name: "inspect",
                    args: spanned(vec![spanned(Expr::Borrow {
                        kind: BorrowKind::Shared,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    })]),
                }))),
            ],
            trailing_expr: None,
        },
    };

    check_program(&make_program(vec![inspect, main]))
        .expect("shared-borrow parameter call should typecheck");
}

#[test]
fn shared_borrow_blocks_owner_mutation() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: true,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "rx",
                    ty: ref_int_type(),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Shared,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    }),
                }),
                spanned(Stmt::Assignment {
                    lhs: spanned(Expr::Variable("x")),
                    rhs: spanned(Expr::Literal(crate::common::ast::Literal::Int(2))),
                }),
            ],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("mutating a shared-borrowed owner should be rejected");
    assert!(matches!(err, TypeError::BorrowConflict { .. }));
}

#[test]
fn mutable_borrow_blocks_shared_borrow() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: true,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "mx",
                    ty: ref_mut_int_type(),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Mutable,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    }),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "rx",
                    ty: ref_int_type(),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Shared,
                        expr: Box::new(spanned(Expr::Variable("x"))),
                    }),
                }),
            ],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("shared borrow during live mutable borrow should be rejected");
    assert!(matches!(err, TypeError::BorrowConflict { .. }));
}

#[test]
fn moving_borrowed_array_is_rejected() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "arr",
                    ty: int_array_type(1),
                    value: spanned(Expr::ArrayInit {
                        value: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(1)))),
                        length: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(1)))),
                    }),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "rarr",
                    ty: spanned(Type::Ref(Box::new(int_array_type(1)))),
                    value: spanned(Expr::Borrow {
                        kind: BorrowKind::Shared,
                        expr: Box::new(spanned(Expr::Variable("arr"))),
                    }),
                }),
                spanned(Stmt::Let {
                    is_mut: false,
                    name: "arr2",
                    ty: int_array_type(1),
                    value: spanned(Expr::Variable("arr")),
                }),
            ],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![func]))
        .expect_err("moving a borrowed array should be rejected");
    assert!(matches!(err, TypeError::BorrowConflict { .. }));
}

#[test]
fn shared_borrow_ends_at_if_block_exit() {
    let func = Function {
        name: "main",
        parameters: vec![],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![
                spanned(Stmt::Let {
                    is_mut: true,
                    name: "x",
                    ty: int_type(),
                    value: spanned(Expr::Literal(crate::common::ast::Literal::Int(1))),
                }),
                spanned(Stmt::Expr(spanned(Expr::If {
                    cond: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Bool(true)))),
                    then_block: crate::common::ast::Block {
                        statements: vec![spanned(Stmt::Let {
                            is_mut: false,
                            name: "rx",
                            ty: ref_int_type(),
                            value: spanned(Expr::Borrow {
                                kind: BorrowKind::Shared,
                                expr: Box::new(spanned(Expr::Variable("x"))),
                            }),
                        })],
                        trailing_expr: None,
                    },
                    else_block: None,
                }))),
                spanned(Stmt::Assignment {
                    lhs: spanned(Expr::Variable("x")),
                    rhs: spanned(Expr::Literal(crate::common::ast::Literal::Int(2))),
                }),
            ],
            trailing_expr: None,
        },
    };

    check_program(&make_program(vec![func]))
        .expect("shared borrow should end when the if block scope exits");
}

#[test]
fn shared_array_reference_indexing_typechecks() {
    let inspect = Function {
        name: "inspect",
        parameters: vec![spanned(Parameter {
            name: "r",
            ty: spanned(Type::Ref(Box::new(int_array_type(1)))),
        })],
        return_type: int_type(),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(Expr::Index {
                base: Box::new(spanned(Expr::Variable("r"))),
                index: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(0)))),
            }))),
        },
    };

    check_program(&make_program(vec![inspect]))
        .expect("indexing through a shared array reference should typecheck");
}

#[test]
fn mutable_array_reference_assignment_typechecks() {
    let touch = Function {
        name: "touch",
        parameters: vec![spanned(Parameter {
            name: "r",
            ty: spanned(Type::RefMut(Box::new(int_array_type(1)))),
        })],
        return_type: int_type(),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![spanned(Stmt::Assignment {
                lhs: spanned(Expr::Index {
                    base: Box::new(spanned(Expr::Variable("r"))),
                    index: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(0)))),
                }),
                rhs: spanned(Expr::Literal(crate::common::ast::Literal::Int(9))),
            })],
            trailing_expr: Some(Box::new(spanned(Expr::Index {
                base: Box::new(spanned(Expr::Variable("r"))),
                index: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(0)))),
            }))),
        },
    };

    check_program(&make_program(vec![touch]))
        .expect("assignment through a mutable array reference should typecheck");
}

#[test]
fn assignment_through_shared_array_reference_is_rejected() {
    let touch = Function {
        name: "touch",
        parameters: vec![spanned(Parameter {
            name: "r",
            ty: spanned(Type::Ref(Box::new(int_array_type(1)))),
        })],
        return_type: spanned(Type::Unit),
        precondition: None,
        postcondition: None,
        body: FunctionBody {
            statements: vec![spanned(Stmt::Assignment {
                lhs: spanned(Expr::Index {
                    base: Box::new(spanned(Expr::Variable("r"))),
                    index: Box::new(spanned(Expr::Literal(crate::common::ast::Literal::Int(0)))),
                }),
                rhs: spanned(Expr::Literal(crate::common::ast::Literal::Int(9))),
            })],
            trailing_expr: None,
        },
    };

    let err = check_program(&make_program(vec![touch]))
        .expect_err("assignment through a shared array reference should be rejected");
    assert!(matches!(err, TypeError::BorrowConflict { .. }));
}
