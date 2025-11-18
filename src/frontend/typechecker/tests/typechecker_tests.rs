// Integration tests for type checker

use crate::common::ast::*;
use crate::common::span::{Span, Spanned};
use crate::common::types::IType;
use crate::frontend::typechecker::{check_program, synth_expr, TypingContext};

#[test]
fn test_if_expression_basic() {
    // Test: if true { let x: int = 5; } else { let y: int = 10; }
    let cond = Spanned(Expr::Literal(Literal::Bool(true)), Span::default());

    let then_stmt = Spanned(
        Stmt::Let {
            is_mut: false,
            name: "x",
            ty: Spanned(Type::Int, Span::default()),
            value: Spanned(Expr::Literal(Literal::Int(5)), Span::default()),
        },
        Span::default(),
    );

    let else_stmt = Spanned(
        Stmt::Let {
            is_mut: false,
            name: "y",
            ty: Spanned(Type::Int, Span::default()),
            value: Spanned(Expr::Literal(Literal::Int(10)), Span::default()),
        },
        Span::default(),
    );

    let if_expr = Spanned(
        Expr::If {
            cond: Box::new(cond),
            then_block: vec![then_stmt],
            else_block: Some(vec![else_stmt]),
        },
        Span::default(),
    );

    let ctx = TypingContext::new();
    let result = synth_expr(&ctx, &if_expr);

    assert!(result.is_ok(), "If expression should type check");
    let (_, ty) = result.unwrap();
    assert!(matches!(ty, IType::Unit), "If expression should have unit type");
}

#[test]
fn test_if_expression_condition_not_bool() {
    // Test: if 5 { } else { }  -- should fail
    let cond = Spanned(Expr::Literal(Literal::Int(5)), Span::default());

    let if_expr = Spanned(
        Expr::If {
            cond: Box::new(cond),
            then_block: vec![],
            else_block: Some(vec![]),
        },
        Span::default(),
    );

    let ctx = TypingContext::new();
    let result = synth_expr(&ctx, &if_expr);

    assert!(result.is_err(), "If with non-bool condition should fail");
}

#[test]
fn test_if_expression_without_else() {
    // Test: if true { let x: int = 5; }
    let cond = Spanned(Expr::Literal(Literal::Bool(true)), Span::default());

    let then_stmt = Spanned(
        Stmt::Let {
            is_mut: false,
            name: "x",
            ty: Spanned(Type::Int, Span::default()),
            value: Spanned(Expr::Literal(Literal::Int(5)), Span::default()),
        },
        Span::default(),
    );

    let if_expr = Spanned(
        Expr::If {
            cond: Box::new(cond),
            then_block: vec![then_stmt],
            else_block: None,
        },
        Span::default(),
    );

    let ctx = TypingContext::new();
    let result = synth_expr(&ctx, &if_expr);

    assert!(result.is_ok(), "If expression without else should type check");
    let (_, ty) = result.unwrap();
    assert!(matches!(ty, IType::Unit), "If without else should have unit type");
}

#[test]
fn test_constant_folding() {
    // Test: 5 + 3
    let expr = Spanned(
        Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Spanned(Expr::Literal(Literal::Int(5)), Span::default())),
            rhs: Box::new(Spanned(Expr::Literal(Literal::Int(3)), Span::default())),
        },
        Span::default(),
    );

    let ctx = TypingContext::new();
    let result = synth_expr(&ctx, &expr);

    assert!(result.is_ok(), "Addition should type check");
    let (_, ty) = result.unwrap();

    // Should produce singleton type int(8)
    match ty {
        IType::SingletonInt(crate::common::types::IValue::Int(8)) => {
            // Success - constant folding worked
        }
        _ => panic!("Expected SingletonInt(8), got {:?}", ty),
    }
}
