use crate::common::ast::{BinOp, Expr, Literal, Program, Stmt, Type, UnaryOp};

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
                Stmt::For { var, start, end, body } => {
                    println!(
                        "    [{}] for {} in {}..{} {{ {} statement(s) }}",
                        i + 1,
                        var,
                        format_expr(&start.0),
                        format_expr(&end.0),
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
