// Pretty error reporting with source highlighting using ariadne

use crate::frontend::typechecker::TypeError;
use ariadne::{Color, Label, Report, ReportKind, Source};

/// Report a type error with source highlighting
pub fn report_type_error(filename: &str, source: &str, error: &TypeError) {
    let report = build_report(error);
    report
        .eprint(Source::from(source))
        .expect("Failed to print error report");

    // Print filename for context
    eprintln!(
        "  --> {}:{}",
        filename,
        get_line_col(source, get_span_start(error))
    );
}

fn get_span_start(error: &TypeError) -> usize {
    match error {
        TypeError::TypeMismatch { span, .. } => span.start,
        TypeError::UndefinedVariable { span, .. } => span.start,
        TypeError::UndefinedFunction { span, .. } => span.start,
        TypeError::NotMutable { span, .. } => span.start,
        TypeError::AssignToImmutable { span, .. } => span.start,
        TypeError::ReturnTypeMismatch { span, .. } => span.start,
        TypeError::MissingReturn { span, .. } => span.start,
        TypeError::NotAnArray { span, .. } => span.start,
        TypeError::InvalidArrayAccess { span, .. } => span.start,
        TypeError::WrongNumberOfArguments { span, .. } => span.start,
        TypeError::ArityMismatch { span, .. } => span.start,
        TypeError::InvalidAssignment { span, .. } => span.start,
        TypeError::MasterTypeMismatch { span, .. } => span.start,
        TypeError::Unprovable { span, .. } => span.start,
        TypeError::PreconditionViolation { span, .. } => span.start,
        TypeError::PostconditionViolation { span, .. } => span.start,
        TypeError::InvalidArraySize { span, .. } => span.start,
        TypeError::InvalidOperation { span, .. } => span.start,
        TypeError::UnsupportedFeature { span, .. } => span.start,
        TypeError::NotAConstant { span, .. } => span.start,
    }
}

fn get_line_col(source: &str, offset: usize) -> String {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in source.chars().enumerate() {
        if i == offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    format!("{}:{}", line, col)
}

/// Build an ariadne Report from a TypeError
fn build_report(error: &TypeError) -> Report<'static, std::ops::Range<usize>> {
    match error {
        TypeError::TypeMismatch {
            expected,
            found,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E001")
            .with_message("Type mismatch")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!("expected `{}`, found `{}`", expected, found))
                    .with_color(Color::Red),
            )
            .with_help(format!(
                "The expression has type `{}` but `{}` was expected",
                found, expected
            ))
            .finish(),

        TypeError::UndefinedVariable { name, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E002")
                .with_message(format!("Cannot find variable `{}` in this scope", name))
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                )
                .with_help("Make sure the variable is declared before use")
                .finish()
        }

        TypeError::UndefinedFunction { name, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E003")
                .with_message(format!("Cannot find function `{}` in this scope", name))
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message("not found in this scope")
                        .with_color(Color::Red),
                )
                .with_help("Make sure the function is defined in the program")
                .finish()
        }

        TypeError::NotMutable { name, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E004")
                .with_message(format!("Cannot assign to immutable variable `{}`", name))
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message("cannot assign twice to immutable variable")
                        .with_color(Color::Red),
                )
                .with_help(format!(
                    "Consider making `{}` mutable: `let mut {}`",
                    name, name
                ))
                .finish()
        }

        TypeError::AssignToImmutable { variable, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E004")
                .with_message(format!(
                    "Cannot assign to immutable variable `{}`",
                    variable
                ))
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message("cannot assign twice to immutable variable")
                        .with_color(Color::Red),
                )
                .with_help(format!(
                    "Consider making `{}` mutable: `let mut {}`",
                    variable, variable
                ))
                .finish()
        }

        TypeError::ReturnTypeMismatch {
            expected,
            found,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E005")
            .with_message("Mismatched return type")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!("expected `{}`, found `{}`", expected, found))
                    .with_color(Color::Red),
            )
            .with_help(format!("The function expects to return `{}`", expected))
            .finish(),

        TypeError::MissingReturn { expected, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E006")
                .with_message("Missing return value")
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("expected to return `{}`", expected))
                        .with_color(Color::Red),
                )
                .with_help("Add a return expression or trailing expression to the function body")
                .finish()
        }

        TypeError::NotAnArray { found, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E007")
                .with_message("Type cannot be indexed")
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("expected array, found `{}`", found))
                        .with_color(Color::Red),
                )
                .with_help("Only array types can be indexed with `[]`")
                .finish()
        }

        TypeError::InvalidArrayAccess {
            array_type,
            index_expr,
            reason,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E008")
            .with_message("Invalid array access")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(reason.clone())
                    .with_color(Color::Red),
            )
            .with_note(format!(
                "Array type: `{}`\nIndex expression: `{}`",
                array_type, index_expr
            ))
            .finish(),

        TypeError::WrongNumberOfArguments {
            expected,
            found,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E009")
            .with_message("Wrong number of arguments")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!(
                        "expected {} argument(s), found {}",
                        expected, found
                    ))
                    .with_color(Color::Red),
            )
            .finish(),

        TypeError::ArityMismatch {
            function,
            expected,
            found,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E009")
            .with_message(format!(
                "Function `{}` expects {} argument(s)",
                function, expected
            ))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!("supplied {} argument(s)", found))
                    .with_color(Color::Red),
            )
            .finish(),

        TypeError::InvalidAssignment {
            variable,
            reason,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E010")
            .with_message(format!("Invalid assignment to `{}`", variable))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(reason.clone())
                    .with_color(Color::Red),
            )
            .finish(),

        TypeError::MasterTypeMismatch {
            variable,
            master_type,
            assigned_type,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E011")
            .with_message(format!(
                "Assignment violates type constraint for `{}`",
                variable
            ))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!(
                        "cannot assign `{}` to variable of type `{}`",
                        assigned_type, master_type
                    ))
                    .with_color(Color::Red),
            )
            .with_note(format!(
                "Variable `{}` was declared with type `{}`",
                variable, master_type
            ))
            .finish(),

        TypeError::Unprovable {
            proposition,
            context,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E012")
            .with_message("Could not verify refinement")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(context.clone())
                    .with_color(Color::Red),
            )
            .with_note(format!("Required to prove: {}", proposition))
            .with_help("Consider adding assertions or preconditions to help the solver")
            .finish(),

        TypeError::PreconditionViolation {
            function,
            precondition,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E013")
            .with_message(format!("Precondition not satisfied for `{}`", function))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message("precondition may not hold")
                    .with_color(Color::Red),
            )
            .with_note(format!("Required: {}", precondition))
            .finish(),

        TypeError::PostconditionViolation {
            function,
            postcondition,
            return_type,
            span,
        } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E018")
            .with_message(format!("Postcondition not satisfied for `{}`", function))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message("postcondition may not hold")
                    .with_color(Color::Red),
            )
            .with_note(format!(
                "Return type: {}\nRequired: ensures {}",
                return_type, postcondition
            ))
            .finish(),

        TypeError::InvalidArraySize { size, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E014")
                .with_message("Invalid array size")
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("`{}` is not a valid array size", size))
                        .with_color(Color::Red),
                )
                .with_help("Array size must be a positive integer")
                .finish()
        }

        TypeError::InvalidOperation {
            operation,
            operand_types,
            span,
        } => {
            let types_str = operand_types
                .iter()
                .map(|t| format!("`{}`", t))
                .collect::<Vec<_>>()
                .join(", ");
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E015")
                .with_message(format!("Invalid operation `{}`", operation))
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("cannot apply to {}", types_str))
                        .with_color(Color::Red),
                )
                .finish()
        }

        TypeError::UnsupportedFeature { feature, span } => {
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code("E016")
                .with_message("Unsupported feature")
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("{} is not yet supported", feature))
                        .with_color(Color::Yellow),
                )
                .finish()
        }

        TypeError::NotAConstant { span } => Report::build(ReportKind::Error, span.start..span.end)
            .with_code("E017")
            .with_message("Expected compile-time constant")
            .with_label(
                Label::new(span.start..span.end)
                    .with_message("this expression is not a constant")
                    .with_color(Color::Red),
            )
            .with_help("Array sizes must be known at compile time")
            .finish(),
    }
}
