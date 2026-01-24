//! Integration tests for the backend compilation pipeline
//!
//! Tests the full pipeline: TAST -> TIR -> DTAL -> Text

use crate::backend::codegen::codegen_program;
use crate::backend::emit::emit_program;
use crate::backend::lower::lower_program;
use crate::common::ast::{BinOp, Literal};
use crate::common::span::{Span, Spanned};
use crate::common::tast::{TExpr, TFunction, TFunctionBody, TParameter, TProgram, TStmt};
use crate::common::types::IType;

/// Helper to create a spanned value with a dummy span
fn spanned<T>(value: T) -> Spanned<T> {
    (value, Span::new(0, 0))
}

/// Test the full pipeline for an identity function
/// Source: fn id(x: int) -> int { x }
#[test]
fn test_pipeline_identity_function() {
    let program = TProgram {
        functions: vec![TFunction {
            name: "id".to_string(),
            parameters: vec![TParameter {
                name: "x".to_string(),
                ty: IType::Int,
            }],
            return_type: IType::Int,
            postcondition: None,
            body: TFunctionBody {
                statements: vec![],
                return_expr: Some(Box::new(spanned(TExpr::Variable {
                    name: "x".to_string(),
                    ty: IType::Int,
                }))),
            },
            span: Span::new(0, 0),
        }],
    };

    // Lower to TIR
    let tir = lower_program(&program);
    assert_eq!(tir.functions.len(), 1);

    // Generate DTAL
    let dtal = codegen_program(&tir);
    assert_eq!(dtal.functions.len(), 1);
    assert_eq!(dtal.functions[0].name, "id");

    // Emit text
    let output = emit_program(&dtal);
    assert!(output.contains(".function id"));
    assert!(output.contains("ret"));

    // Print for debugging
    println!("=== Identity Function DTAL ===\n{}", output);
}

/// Test the full pipeline for an addition function
/// Source: fn add(x: int, y: int) -> int { let z = x + y; z }
#[test]
fn test_pipeline_add_function() {
    let program = TProgram {
        functions: vec![TFunction {
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
                })],
                return_expr: Some(Box::new(spanned(TExpr::Variable {
                    name: "z".to_string(),
                    ty: IType::Int,
                }))),
            },
            span: Span::new(0, 0),
        }],
    };

    // Lower to TIR
    let tir = lower_program(&program);

    // Generate DTAL
    let dtal = codegen_program(&tir);

    // Emit text
    let output = emit_program(&dtal);
    assert!(output.contains(".function add"));
    assert!(output.contains("add")); // Should contain add instruction
    assert!(output.contains("ret"));

    println!("=== Add Function DTAL ===\n{}", output);
}

/// Test the full pipeline for a function returning a constant
/// Source: fn const_five() -> int { 5 }
#[test]
fn test_pipeline_constant_function() {
    let program = TProgram {
        functions: vec![TFunction {
            name: "const_five".to_string(),
            parameters: vec![],
            return_type: IType::Int,
            postcondition: None,
            body: TFunctionBody {
                statements: vec![],
                return_expr: Some(Box::new(spanned(TExpr::Literal {
                    value: Literal::Int(5),
                    ty: IType::Int,
                }))),
            },
            span: Span::new(0, 0),
        }],
    };

    let tir = lower_program(&program);
    let dtal = codegen_program(&tir);
    let output = emit_program(&dtal);

    assert!(output.contains(".function const_five"));
    assert!(output.contains("mov")); // Should contain mov for loading constant
    assert!(output.contains("5")); // Should contain the value 5
    assert!(output.contains("ret"));

    println!("=== Const Five Function DTAL ===\n{}", output);
}

/// Test the full pipeline for a function with conditionals
/// Source: fn abs(x: int) -> int { if x >= 0 { x } else { 0 - x } }
#[test]
fn test_pipeline_conditional_function() {
    let program = TProgram {
        functions: vec![TFunction {
            name: "abs".to_string(),
            parameters: vec![TParameter {
                name: "x".to_string(),
                ty: IType::Int,
            }],
            return_type: IType::Int,
            postcondition: None,
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
        }],
    };

    let tir = lower_program(&program);

    // Verify TIR has multiple blocks (entry, then, else, merge)
    assert!(tir.functions[0].blocks.len() >= 4);

    let dtal = codegen_program(&tir);
    let output = emit_program(&dtal);

    // Should have multiple blocks with branches
    assert!(output.contains(".function abs"));
    assert!(output.contains("cmp")); // Comparison
    assert!(output.contains("bne") || output.contains("beq")); // Branch
    assert!(output.contains("jmp")); // Jump
    assert!(output.contains("ret"));

    println!("=== Abs Function DTAL ===\n{}", output);
}

/// Test a multi-function program
#[test]
fn test_pipeline_multi_function_program() {
    let program = TProgram {
        functions: vec![
            TFunction {
                name: "helper".to_string(),
                parameters: vec![TParameter {
                    name: "n".to_string(),
                    ty: IType::Int,
                }],
                return_type: IType::Int,
                postcondition: None,
                body: TFunctionBody {
                    statements: vec![],
                    return_expr: Some(Box::new(spanned(TExpr::BinOp {
                        op: BinOp::Mul,
                        lhs: Box::new(spanned(TExpr::Variable {
                            name: "n".to_string(),
                            ty: IType::Int,
                        })),
                        rhs: Box::new(spanned(TExpr::Literal {
                            value: Literal::Int(2),
                            ty: IType::Int,
                        })),
                        ty: IType::Int,
                    }))),
                },
                span: Span::new(0, 0),
            },
            TFunction {
                name: "main".to_string(),
                parameters: vec![],
                return_type: IType::Int,
                postcondition: None,
                body: TFunctionBody {
                    statements: vec![spanned(TStmt::Let {
                        is_mut: false,
                        name: "result".to_string(),
                        declared_ty: IType::Int,
                        value: spanned(TExpr::Call {
                            func_name: "helper".to_string(),
                            args: vec![spanned(TExpr::Literal {
                                value: Literal::Int(21),
                                ty: IType::Int,
                            })],
                            ty: IType::Int,
                        }),
                        checked_ty: IType::Int,
                    })],
                    return_expr: Some(Box::new(spanned(TExpr::Variable {
                        name: "result".to_string(),
                        ty: IType::Int,
                    }))),
                },
                span: Span::new(0, 0),
            },
        ],
    };

    let tir = lower_program(&program);
    assert_eq!(tir.functions.len(), 2);

    let dtal = codegen_program(&tir);
    assert_eq!(dtal.functions.len(), 2);

    let output = emit_program(&dtal);
    assert!(output.contains(".function helper"));
    assert!(output.contains(".function main"));
    assert!(output.contains("call helper"));
    assert!(output.contains("mul")); // helper multiplies by 2

    println!("=== Multi-function Program DTAL ===\n{}", output);
}

/// Test the full pipeline for a function with a postcondition
/// Source: fn five() -> int ensures result == 5 { 5 }
#[test]
fn test_pipeline_function_with_postcondition() {
    use crate::common::ast::Expr;
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

    let program = TProgram {
        functions: vec![TFunction {
            name: "five".to_string(),
            parameters: vec![],
            return_type: IType::Int,
            postcondition: Some(postcondition),
            body: TFunctionBody {
                statements: vec![],
                return_expr: Some(Box::new(spanned(TExpr::Literal {
                    value: Literal::Int(5),
                    ty: IType::Int,
                }))),
            },
            span: Span::new(0, 0),
        }],
    };

    // Lower to TIR
    let tir = lower_program(&program);
    assert_eq!(tir.functions.len(), 1);
    assert!(
        tir.functions[0].postcondition.is_some(),
        "TIR should have postcondition"
    );

    // Generate DTAL
    let dtal = codegen_program(&tir);
    assert_eq!(dtal.functions.len(), 1);
    assert!(
        dtal.functions[0].postcondition.is_some(),
        "DTAL should have postcondition"
    );

    // Emit text
    let output = emit_program(&dtal);
    assert!(output.contains(".function five"));
    assert!(
        output.contains(".postcondition"),
        "DTAL output should contain .postcondition directive"
    );
    assert!(output.contains("ret"));

    println!("=== Function with Postcondition DTAL ===\n{}", output);
}

/// Test the pipeline for a function with inequality postcondition
/// Source: fn positive() -> int ensures result > 0 { 42 }
#[test]
fn test_pipeline_function_with_inequality_postcondition() {
    use crate::common::ast::Expr;
    use crate::common::types::IProposition;
    use chumsky::prelude::SimpleSpan;
    use std::sync::Arc;

    // Create postcondition: result > 0
    let postcond_expr = Expr::BinOp {
        op: BinOp::Gt,
        lhs: Box::new((Expr::Variable("result"), SimpleSpan::new(0, 0))),
        rhs: Box::new((Expr::Literal(Literal::Int(0)), SimpleSpan::new(0, 0))),
    };
    let postcondition = IProposition {
        var: "result".to_string(),
        predicate: Arc::new((postcond_expr, SimpleSpan::new(0, 0))),
    };

    let program = TProgram {
        functions: vec![TFunction {
            name: "positive".to_string(),
            parameters: vec![],
            return_type: IType::Int,
            postcondition: Some(postcondition),
            body: TFunctionBody {
                statements: vec![],
                return_expr: Some(Box::new(spanned(TExpr::Literal {
                    value: Literal::Int(42),
                    ty: IType::Int,
                }))),
            },
            span: Span::new(0, 0),
        }],
    };

    let tir = lower_program(&program);
    let dtal = codegen_program(&tir);
    let output = emit_program(&dtal);

    assert!(output.contains(".function positive"));
    assert!(
        output.contains(".postcondition"),
        "DTAL output should contain .postcondition directive"
    );
    assert!(
        output.contains("result > 0"),
        "Postcondition should show result > 0"
    );

    println!(
        "=== Function with Inequality Postcondition DTAL ===\n{}",
        output
    );
}
