//! Tests for TAST to TIR lowering

use crate::backend::lower::lower_function;
use crate::backend::tir::{Terminator, TirInstr};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TExpr, TFunction, TFunctionBody, TParameter, TStmt};
use crate::common::types::IType;
use crate::common::ast::Literal;

/// Helper to create a spanned value with a dummy span
fn spanned<T>(value: T) -> Spanned<T> {
    (value, Span::new(0, 0))
}

/// Test: fn id(x: int) -> int { x }
#[test]
fn test_lower_identity_function() {
    let func = TFunction {
        name: "id".to_string(),
        parameters: vec![TParameter {
            name: "x".to_string(),
            ty: IType::Int,
        }],
        return_type: IType::Int,
        body: TFunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(TExpr::Variable {
                name: "x".to_string(),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function properties
    assert_eq!(tir_func.name, "id");
    assert_eq!(tir_func.params.len(), 1);
    assert!(matches!(tir_func.return_type, IType::Int));

    // Verify we have at least one block
    assert!(!tir_func.blocks.is_empty());

    // The entry block should have a Return terminator
    let entry_block = tir_func.blocks.get(&tir_func.entry_block).unwrap();
    assert!(matches!(entry_block.terminator, Some(Terminator::Return { .. })));
}

/// Test: fn add(x: int, y: int) -> int { let z = x + y; z }
#[test]
fn test_lower_function_with_let() {
    use crate::common::ast::BinOp;

    let func = TFunction {
        name: "add".to_string(),
        parameters: vec![
            TParameter {
                name: "x".to_string(),
                ty: IType::Int,
            },
            TParameter {
                name: "y".to_string(),
                ty: IType::Int,
            },
        ],
        return_type: IType::Int,
        body: TFunctionBody {
            statements: vec![spanned(TStmt::Let {
                is_mut: false,
                name: "z".to_string(),
                declared_ty: IType::Int,
                value: spanned(TExpr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: IType::Int,
                    })),
                    rhs: Box::new(spanned(TExpr::Variable {
                        name: "y".to_string(),
                        ty: IType::Int,
                    })),
                    ty: IType::Int,
                }),
                checked_ty: IType::Int,
            })],
            return_expr: Some(Box::new(spanned(TExpr::Variable {
                name: "z".to_string(),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function properties
    assert_eq!(tir_func.name, "add");
    assert_eq!(tir_func.params.len(), 2);

    // The entry block should have a BinOp instruction
    let entry_block = tir_func.blocks.get(&tir_func.entry_block).unwrap();
    let has_binop = entry_block
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::BinOp { .. }));
    assert!(has_binop, "Expected a BinOp instruction in the entry block");
}

/// Test: fn const_five() -> int { 5 }
#[test]
fn test_lower_function_returning_literal() {
    let func = TFunction {
        name: "const_five".to_string(),
        parameters: vec![],
        return_type: IType::Int,
        body: TFunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(TExpr::Literal {
                value: Literal::Int(5),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function properties
    assert_eq!(tir_func.name, "const_five");
    assert_eq!(tir_func.params.len(), 0);

    // The entry block should have a LoadImm instruction
    let entry_block = tir_func.blocks.get(&tir_func.entry_block).unwrap();
    let has_loadimm = entry_block.instructions.iter().any(|instr| {
        matches!(instr, TirInstr::LoadImm { value: 5, .. })
    });
    assert!(has_loadimm, "Expected a LoadImm(5) instruction");
}

/// Test: fn abs(x: int) -> int { if x >= 0 { x } else { 0 - x } }
#[test]
fn test_lower_if_expression() {
    use crate::common::ast::BinOp;

    let func = TFunction {
        name: "abs".to_string(),
        parameters: vec![TParameter {
            name: "x".to_string(),
            ty: IType::Int,
        }],
        return_type: IType::Int,
        body: TFunctionBody {
            statements: vec![],
            return_expr: Some(Box::new(spanned(TExpr::If {
                cond: Box::new(spanned(TExpr::BinOp {
                    op: BinOp::Gte,
                    lhs: Box::new(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: IType::Int,
                    })),
                    rhs: Box::new(spanned(TExpr::Literal {
                        value: Literal::Int(0),
                        ty: IType::Int,
                    })),
                    ty: IType::Bool,
                })),
                then_block: vec![spanned(TStmt::Expr(spanned(TExpr::Variable {
                    name: "x".to_string(),
                    ty: IType::Int,
                })))],
                else_block: Some(vec![spanned(TStmt::Expr(spanned(TExpr::BinOp {
                    op: BinOp::Sub,
                    lhs: Box::new(spanned(TExpr::Literal {
                        value: Literal::Int(0),
                        ty: IType::Int,
                    })),
                    rhs: Box::new(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: IType::Int,
                    })),
                    ty: IType::Int,
                })))]),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function has multiple blocks (entry, then, else, merge)
    assert!(
        tir_func.blocks.len() >= 4,
        "Expected at least 4 blocks for if expression, got {}",
        tir_func.blocks.len()
    );

    // Verify there's a Branch terminator somewhere
    let has_branch = tir_func.blocks.values().any(|block| {
        matches!(block.terminator, Some(Terminator::Branch { .. }))
    });
    assert!(has_branch, "Expected a Branch terminator for if expression");

    // Verify there's a phi node in the merge block
    let has_phi = tir_func.blocks.values().any(|block| !block.phi_nodes.is_empty());
    assert!(has_phi, "Expected phi nodes for if expression merge");
}
