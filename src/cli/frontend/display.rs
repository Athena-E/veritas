use crate::common::ast::{BinOp, Expr, Literal, Program, Stmt, Type, UnaryOp};
use crate::common::tast::{TProgram, TStmt, TExpr};
use crate::common::types::IType;

/// Display the program structure in a human-readable format
pub fn display_program(program: &Program) {
    println!("\nProgram with {} function(s):", program.functions.len());

    for (func, _span) in &program.functions {
        println!("\n  Function: '{}'", func.name);

        // Format parameters
        if func.parameters.is_empty() {
            println!("    Parameters: ()");
        } else {
            let params: Vec<String> = func
                .parameters
                .iter()
                .map(|(param, _)| format!("{}: {}", param.name, format_type(&param.ty.0)))
                .collect();
            println!("    Parameters: ({})", params.join(", "));
        }

        // Display return type
        println!("    Return type: {}", format_type(&func.return_type.0));

        let stmt_count = func.body.statements.len();
        let has_return_expr = func.body.return_expr.is_some();
        println!("    Body: {} statement(s){}\n",
            stmt_count,
            if has_return_expr { " + implicit return" } else { "" }
        );

        for (i, (stmt, _)) in func.body.statements.iter().enumerate() {
            match stmt {
                Stmt::Let {
                    is_mut,
                    name,
                    ty,
                    value,
                } => {
                    let mutability = if *is_mut { "mut " } else { "" };
                    println!(
                        "    [{}] let {}{}: {} = {};",
                        i + 1,
                        mutability,
                        name,
                        format_type(&ty.0),
                        format_expr(&value.0)
                    );
                }
                Stmt::Assignment { lhs, rhs } => {
                    println!(
                        "    [{}] {} = {};",
                        i + 1,
                        format_expr(&lhs.0),
                        format_expr(&rhs.0)
                    );
                }
                Stmt::Return { expr } => {
                    println!(
                        "    [{}] return {};",
                        i + 1,
                        format_expr(&expr.0)
                    );
                }
                Stmt::Expr(expr) => {
                    println!(
                        "    [{}] {};",
                        i + 1,
                        format_expr(&expr.0)
                    );
                }
                Stmt::For { var, start, end, invariant, body } => {
                    let inv_str = if let Some(inv) = invariant {
                        format!(" invariant {}", format_expr(&inv.0))
                    } else {
                        String::new()
                    };
                    println!(
                        "    [{}] for {} in {}..{}{} {{ {} statement(s) }}",
                        i + 1,
                        var,
                        format_expr(&start.0),
                        format_expr(&end.0),
                        inv_str,
                        body.len()
                    );
                }
            }
        }

        // Display implicit return if present
        if let Some(return_expr) = &func.body.return_expr {
            println!("    [return] {}", format_expr(&return_expr.0));
        }
    }
}

/// Display the typed program structure in a human-readable format
pub fn display_typed_program(program: &TProgram) {
    println!("\nTyped Program with {} function(s):", program.functions.len());

    for func in &program.functions {
        println!("\n  Function: '{}'", func.name);

        // Format parameters with types
        if func.parameters.is_empty() {
            println!("    Parameters: ()");
        } else {
            let params: Vec<String> = func
                .parameters
                .iter()
                .map(|param| format!("{}: {}", param.name, param.ty))
                .collect();
            println!("    Parameters: ({})", params.join(", "));
        }

        // Display return type
        println!("    Return type: {}", func.return_type);

        let stmt_count = func.body.statements.len();
        let has_return_expr = func.body.return_expr.is_some();
        println!("    Body: {} statement(s){}\n",
            stmt_count,
            if has_return_expr { " + implicit return" } else { "" }
        );

        for (i, (stmt, _)) in func.body.statements.iter().enumerate() {
            match stmt {
                TStmt::Let {
                    is_mut,
                    name,
                    declared_ty,
                    value,
                    checked_ty,
                } => {
                    let mutability = if *is_mut { "mut " } else { "" };
                    println!(
                        "    [{}] let {}{}: {} = {}; // checked: {}",
                        i + 1,
                        mutability,
                        name,
                        declared_ty,
                        format_texpr(&value.0),
                        checked_ty
                    );
                }
                TStmt::Assignment { lhs, rhs } => {
                    println!(
                        "    [{}] {} = {}; // {} := {}",
                        i + 1,
                        format_texpr(&lhs.0),
                        format_texpr(&rhs.0),
                        lhs.0.get_type(),
                        rhs.0.get_type()
                    );
                }
                TStmt::Return { expr } => {
                    println!(
                        "    [{}] return {}; // : {}",
                        i + 1,
                        format_texpr(&expr.0),
                        expr.0.get_type()
                    );
                }
                TStmt::Expr(expr) => {
                    println!(
                        "    [{}] {}; // : {}",
                        i + 1,
                        format_texpr(&expr.0),
                        expr.0.get_type()
                    );
                }
                TStmt::For { var, var_ty, start, end, invariant, body } => {
                    let inv_str = if let Some(inv) = invariant {
                        format!(" invariant {}", format_texpr(&inv.0))
                    } else {
                        String::new()
                    };
                    println!(
                        "    [{}] for {}: {} in {}..{}{} {{ {} statement(s) }}",
                        i + 1,
                        var,
                        var_ty,
                        format_texpr(&start.0),
                        format_texpr(&end.0),
                        inv_str,
                        body.len()
                    );
                }
            }
        }

        // Display implicit return if present
        if let Some(return_expr) = &func.body.return_expr {
            println!("    [return] {} // : {}",
                format_texpr(&return_expr.0),
                return_expr.0.get_type()
            );
        }
    }
}

/// Format a typed expression as a string
fn format_texpr(expr: &TExpr) -> String {
    match expr {
        TExpr::Error { .. } => "<error>".to_string(),
        TExpr::Literal { value, .. } => match value {
            Literal::Int(n) => n.to_string(),
            Literal::Bool(b) => b.to_string(),
        },
        TExpr::Variable { name, .. } => name.to_string(),
        TExpr::BinOp { op, lhs, rhs, .. } => {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Eq => "==",
                BinOp::NotEq => "!=",
                BinOp::Lt => "<",
                BinOp::Lte => "<=",
                BinOp::Gt => ">",
                BinOp::Gte => ">=",
                BinOp::And => "&&",
                BinOp::Or => "||",
            };
            format!(
                "({} {} {})",
                format_texpr(&lhs.0),
                op_str,
                format_texpr(&rhs.0)
            )
        }
        TExpr::UnaryOp { op, operand, .. } => {
            let u_op_str = match op {
                UnaryOp::Not => "!",
            };
            format!("({} {})", u_op_str, format_texpr(&operand.0))
        }
        TExpr::Call { func_name, args, .. } => {
            let arg_strs: Vec<String> = args.iter().map(|(e, _)| format_texpr(e)).collect();
            format!("{}({})", func_name, arg_strs.join(", "))
        }
        TExpr::Index { base, index, .. } => {
            format!("{}[{}]", format_texpr(&base.0), format_texpr(&index.0))
        }
        TExpr::ArrayInit { value, length, .. } => {
            format!("[{}; {}]", format_texpr(&value.0), format_texpr(&length.0))
        }
        TExpr::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            let else_str = if else_block.is_some() {
                " else { ... }"
            } else {
                ""
            };
            format!(
                "if {} {{ {} stmt(s) }}{}",
                format_texpr(&cond.0),
                then_block.len(),
                else_str
            )
        }
    }
}

/// Format a type as a string
pub fn format_type(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Array {
            element_type,
            size,
        } => {
            format!(
                "[{}; {}]",
                format_type(&element_type.0),
                format_expr(&size.0)
            )
        }
        Type::Ref(inner) => format!("&{}", format_type(&inner.0)),
        Type::RefMut(inner) => format!("&mut {}", format_type(&inner.0)),
        Type::SingletonInt(expr) => format!("int({})", format_expr(&expr.0)),
        Type::RefinedInt { var, predicate } => {
            format!("{{{}: int | {}}}", var, format_expr(&predicate.0))
        },
        Type::Unit => "()".to_string(),
    }
}

/// Format an expression as a string
pub fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Error => "<error>".to_string(),
        Expr::Literal(Literal::Int(n)) => n.to_string(),
        Expr::Literal(Literal::Bool(b)) => b.to_string(),
        Expr::Variable(name) => name.to_string(),
        Expr::BinOp { op, lhs, rhs } => {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Eq => "==",
                BinOp::NotEq => "!=",
                BinOp::Lt => "<",
                BinOp::Lte => "<=",
                BinOp::Gt => ">",
                BinOp::Gte => ">=",
                BinOp::And => "&&",
                BinOp::Or => "||",
            };
            format!(
                "({} {} {})",
                format_expr(&lhs.0),
                op_str,
                format_expr(&rhs.0)
            )
        }
        Expr::UnaryOp { op, cond } => {
            let u_op_str = match op {
                UnaryOp::Not => "!",
            };
            format!("({} {})", u_op_str, format_expr(&cond.0))
        }
        Expr::Call { func_name, args } => {
            let arg_strs: Vec<String> = args.0.iter().map(|(e, _)| format_expr(e)).collect();
            format!("{}({})", func_name, arg_strs.join(", "))
        }
        Expr::Index { base, index } => {
            format!("{}[{}]", format_expr(&base.0), format_expr(&index.0))
        }
        Expr::ArrayInit { value, length } => {
            format!("[{}; {}]", format_expr(&value.0), format_expr(&length.0))
        }
        Expr::If {
            cond,
            then_block,
            else_block,
        } => {
            let else_str = if else_block.is_some() {
                " else { ... }"
            } else {
                ""
            };
            format!(
                "if {} {{ {} stmt(s) }}{}",
                format_expr(&cond.0),
                then_block.len(),
                else_str
            )
        }
    }
}
