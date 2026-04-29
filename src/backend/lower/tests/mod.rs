//! Tests for TAST to TIR lowering

use crate::backend::lower::lower_function;
use crate::backend::tir::{Terminator, TirInstr};
use crate::common::ast::Literal;
use crate::common::ownership::{BorrowKind, OwnershipMode, ParameterKind};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TBlock, TExpr, TFunction, TFunctionBody, TParameter, TStmt};
use crate::common::types::IType;

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
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(TExpr::Variable {
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
    assert!(matches!(entry_block.terminator, Terminator::Return { .. }));
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
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: None,
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
                ownership: crate::common::ownership::OwnershipMode::Plain,
            })],
            trailing_expr: Some(Box::new(spanned(TExpr::Variable {
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
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(TExpr::Literal {
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
    let has_loadimm = entry_block
        .instructions
        .iter()
        .any(|instr| matches!(instr, TirInstr::LoadImm { value: 5, .. }));
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
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(TExpr::If {
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
                then_block: TBlock {
                    statements: vec![spanned(TStmt::Expr(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: IType::Int,
                    })))],
                    trailing_expr: None,
                },
                else_block: Some(TBlock {
                    statements: vec![spanned(TStmt::Expr(spanned(TExpr::BinOp {
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
                    })))],
                    trailing_expr: None,
                }),
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
    let has_branch = tir_func
        .blocks
        .values()
        .any(|block| matches!(block.terminator, Terminator::Branch { .. }));
    assert!(has_branch, "Expected a Branch terminator for if expression");

    // Verify there's a phi node in the merge block
    let has_phi = tir_func
        .blocks
        .values()
        .any(|block| !block.phi_nodes.is_empty());
    assert!(has_phi, "Expected phi nodes for if expression merge");
}

/// Test: fn five() -> int ensures result == 5 { 5 }
/// Verifies postcondition is lowered to TIR
#[test]
fn test_lower_function_with_postcondition() {
    use crate::common::ast::{BinOp, Expr};
    use crate::common::types::IProposition;
    use chumsky::prelude::SimpleSpan;
    use std::sync::Arc;

    // Create postcondition: result == 5
    let postcond_expr = Expr::BinOp {
        op: BinOp::Eq,
        lhs: Box::new((Expr::Variable("result"), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(5)), SimpleSpan::new(0, 0))),
    };
    let postcondition = IProposition {
        var: "result".to_string(),
        predicate: Arc::new((postcond_expr, SimpleSpan::new(0, 0))),
    };

    let func = TFunction {
        name: "five".to_string(),
        parameters: vec![],
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: Some(postcondition),
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(TExpr::Literal {
                value: Literal::Int(5),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function properties
    assert_eq!(tir_func.name, "five");
    assert!(
        tir_func.postcondition.is_some(),
        "Expected postcondition to be lowered to TIR"
    );
}

/// Test: fn no_postcond() -> int { 42 }
/// Verifies functions without postconditions work correctly
#[test]
fn test_lower_function_without_postcondition() {
    let func = TFunction {
        name: "no_postcond".to_string(),
        parameters: vec![],
        parameter_kinds: vec![],
        return_type: IType::Int,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: Some(Box::new(spanned(TExpr::Literal {
                value: Literal::Int(42),
                ty: IType::Int,
            }))),
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);

    // Verify function properties
    assert_eq!(tir_func.name, "no_postcond");
    assert!(
        tir_func.postcondition.is_none(),
        "Expected no postcondition"
    );
}

#[test]
fn test_lower_shared_borrow_binding_emits_borrow_and_scope_end() {
    let array_ty = IType::Array {
        element_type: std::sync::Arc::new(IType::Int),
        size: crate::common::types::IValue::Int(1),
    };
    let func = TFunction {
        name: "borrow_local".to_string(),
        parameters: vec![TParameter {
            name: "x".to_string(),
            ty: array_ty.clone(),
        }],
        parameter_kinds: vec![ParameterKind::PlainValue],
        return_type: IType::Unit,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![spanned(TStmt::Let {
                is_mut: false,
                name: "rx".to_string(),
                declared_ty: IType::Ref(std::sync::Arc::new(array_ty.clone())),
                value: spanned(TExpr::Borrow {
                    kind: BorrowKind::Shared,
                    expr: Box::new(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: array_ty.clone(),
                    })),
                    ty: IType::Ref(std::sync::Arc::new(array_ty.clone())),
                }),
                checked_ty: IType::Ref(std::sync::Arc::new(array_ty)),
                ownership: OwnershipMode::Plain,
            })],
            trailing_expr: None,
        },
        span: Span::new(0, 0),
    };

    let tir_func = lower_function(&func);
    let entry_block = tir_func.blocks.get(&tir_func.entry_block).unwrap();
    assert!(
        entry_block
            .instructions
            .iter()
            .any(|instr| matches!(instr, TirInstr::BorrowShared { .. }))
    );
    assert!(
        entry_block
            .instructions
            .iter()
            .any(|instr| matches!(instr, TirInstr::BorrowEnd { .. }))
    );
}

#[test]
fn test_lower_direct_scalar_borrow_call_materializes_hidden_cell_and_ends_it() {
    let callee = TFunction {
        name: "inspect".to_string(),
        parameters: vec![TParameter {
            name: "r".to_string(),
            ty: IType::Ref(std::sync::Arc::new(IType::Int)),
        }],
        parameter_kinds: vec![ParameterKind::SharedBorrow],
        return_type: IType::Unit,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![],
            trailing_expr: None,
        },
        span: Span::new(0, 0),
    };

    let caller = TFunction {
        name: "caller".to_string(),
        parameters: vec![TParameter {
            name: "x".to_string(),
            ty: IType::Int,
        }],
        parameter_kinds: vec![ParameterKind::PlainValue],
        return_type: IType::Unit,
        returns_owned: false,
        precondition: None,
        postcondition: None,
        body: TFunctionBody {
            statements: vec![spanned(TStmt::Expr(spanned(TExpr::Call {
                func_name: "inspect".to_string(),
                args: vec![spanned(TExpr::Borrow {
                    kind: BorrowKind::Shared,
                    expr: Box::new(spanned(TExpr::Variable {
                        name: "x".to_string(),
                        ty: IType::Int,
                    })),
                    ty: IType::Ref(std::sync::Arc::new(IType::Int)),
                })],
                arg_kinds: vec![ParameterKind::SharedBorrow],
                ownership: OwnershipMode::Plain,
                ty: IType::Unit,
            })))],
            trailing_expr: None,
        },
        span: Span::new(0, 0),
    };

    let _ = lower_function(&callee);
    let tir_func = lower_function(&caller);
    let entry_block = tir_func.blocks.get(&tir_func.entry_block).unwrap();
    let borrow_shared_count = entry_block
        .instructions
        .iter()
        .filter(|instr| matches!(instr, TirInstr::BorrowShared { .. }))
        .count();
    let borrow_end_count = entry_block
        .instructions
        .iter()
        .filter(|instr| matches!(instr, TirInstr::BorrowEnd { .. }))
        .count();
    assert_eq!(borrow_shared_count, 1);
    assert_eq!(borrow_end_count, 1);
    assert!(
        entry_block
            .instructions
            .iter()
            .any(|instr| matches!(instr, TirInstr::AllocArray { size: 1, .. }))
    );
    assert!(entry_block.instructions.iter().any(|instr| matches!(
        instr,
        TirInstr::Call {
            arg_kinds,
            func,
            ..
        } if func == "inspect" && arg_kinds == &vec![ParameterKind::SharedBorrow]
    )));
}
