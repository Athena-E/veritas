//! Veritas Compiler Pipeline
//!
//! This module provides the end-to-end compilation pipeline from source code
//! to DTAL (Dependently Typed Assembly Language) output.
//!
//! # Pipeline Stages
//!
//! ```text
//! Source Code (&str)
//!     │
//!     ▼ lexer
//! Tokens (Vec<Spanned<Token>>)
//!     │
//!     ▼ parser
//! AST (Program)
//!     │
//!     ▼ typechecker
//! Typed AST (TProgram)
//!     │
//!     ▼ lower
//! TIR (TirProgram) - SSA form
//!     │
//!     ▼ codegen
//! DTAL (DtalProgram)
//!     │
//!     ▼ emit
//! Output (String)
//! ```

use crate::backend::{codegen_program, emit_program, lower_program};
use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::{TypeError, check_program, report_type_error};
use chumsky::prelude::*;
use std::fmt;

/// Compilation error types
#[derive(Debug)]
pub enum CompileError<'src> {
    /// Lexer errors (tokenization failed)
    LexError(String),
    /// Parser errors (syntax errors)
    ParseError(String),
    /// Type errors (semantic errors)
    TypeError(TypeError<'src>),
}

impl<'src> fmt::Display for CompileError<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::LexError(msg) => write!(f, "Lexer error: {}", msg),
            CompileError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            CompileError::TypeError(e) => write!(f, "Type error: {:?}", e),
        }
    }
}

/// Result of a successful compilation
#[derive(Debug, Clone)]
pub struct CompileOutput {
    /// The generated DTAL assembly as text
    pub dtal: String,
}

/// Verbose compilation output with all intermediate stages
pub struct VerboseOutput<'src> {
    /// Tokens produced by lexer (token, span as string)
    pub tokens: Vec<(String, String)>,
    /// The typed AST
    pub tast: crate::common::tast::TProgram<'src>,
    /// The TIR (SSA form)
    pub tir: crate::backend::TirProgram<'src>,
    /// The generated DTAL assembly as text
    pub dtal: String,
}

/// Compile source code to DTAL assembly
///
/// This is the main entry point for the compiler pipeline.
///
/// # Arguments
///
/// * `source` - The source code to compile
///
/// # Returns
///
/// * `Ok(CompileOutput)` - Successful compilation with DTAL output
/// * `Err(CompileError)` - Compilation failed at some stage
///
/// # Example
///
/// ```
/// use veritas::pipeline::compile;
///
/// let source = "fn add(x: int, y: int) -> int { x + y }";
/// let output = compile(source).unwrap();
/// assert!(output.dtal.contains(".function add"));
/// ```
pub fn compile(source: &str) -> Result<CompileOutput, CompileError<'_>> {
    // Stage 1: Lexical analysis
    let tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

    // Stage 2: Parsing
    let eoi = (source.len()..source.len()).into();
    let token_stream = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let ast = program_parser()
        .parse(token_stream)
        .into_result()
        .map_err(|errors| {
            CompileError::ParseError(
                errors
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?;

    // Stage 3: Type checking
    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    // Stage 4: Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Stage 5: Generate DTAL
    let dtal_program = codegen_program(&tir);

    // Stage 6: Emit text
    let dtal = emit_program(&dtal_program);

    Ok(CompileOutput { dtal })
}

/// Compile source code with verbose output, returning all intermediate stages
///
/// This function is useful for debugging and understanding the compilation process.
/// It returns all intermediate representations including the typed AST and TIR.
pub fn compile_verbose(source: &str) -> Result<VerboseOutput<'_>, CompileError<'_>> {
    // Stage 1: Lexical analysis
    let raw_tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

    // Capture tokens as strings for display
    let token_strings: Vec<(String, String)> = raw_tokens
        .iter()
        .map(|(tok, span)| (format!("{:?}", tok), format!("{:?}", span)))
        .collect();

    // Stage 2: Parsing
    let eoi = (source.len()..source.len()).into();
    let token_stream = raw_tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let ast = program_parser()
        .parse(token_stream)
        .into_result()
        .map_err(|errors| {
            CompileError::ParseError(
                errors
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?;

    // Stage 3: Type checking
    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    // Stage 4: Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Stage 5: Generate DTAL
    let dtal_program = codegen_program(&tir);

    // Stage 6: Emit text
    let dtal = emit_program(&dtal_program);

    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal,
    })
}

/// Compile source code and report errors with source context
///
/// This is a convenience function that prints pretty error messages
/// when compilation fails.
///
/// # Arguments
///
/// * `filename` - The filename (for error reporting)
/// * `source` - The source code to compile
///
/// # Returns
///
/// * `Ok(String)` - The generated DTAL assembly
/// * `Err(())` - Compilation failed (errors printed to stderr)
#[allow(clippy::result_unit_err)]
pub fn compile_and_report(filename: &str, source: &str) -> Result<String, ()> {
    match compile(source) {
        Ok(output) => Ok(output.dtal),
        Err(CompileError::LexError(msg)) => {
            eprintln!("Lexer errors in {}:\n{}", filename, msg);
            Err(())
        }
        Err(CompileError::ParseError(msg)) => {
            eprintln!("Parse errors in {}:\n{}", filename, msg);
            Err(())
        }
        Err(CompileError::TypeError(e)) => {
            report_type_error(filename, source, &e);
            Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_identity_function() {
        let source = r#"
            fn id(x: int) -> int {
                x
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function id"));
        assert!(output.dtal.contains("ret"));

        println!("=== Identity Function ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_add_function() {
        let source = r#"
            fn add(x: int, y: int) -> int {
                x + y
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function add"));
        assert!(output.dtal.contains("add ")); // add instruction
        assert!(output.dtal.contains("ret"));

        println!("=== Add Function ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_constant() {
        let source = r#"
            fn const_five() -> int {
                5
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function const_five"));
        assert!(output.dtal.contains("mov")); // mov immediate
        assert!(output.dtal.contains("5")); // the value 5
        assert!(output.dtal.contains("ret"));

        println!("=== Const Five ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_with_let() {
        let source = r#"
            fn double(n: int) -> int {
                let result: int = n + n;
                result
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function double"));
        assert!(output.dtal.contains("add ")); // add instruction

        println!("=== Double Function ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_conditional() {
        let source = r#"
            fn abs(x: int) -> int {
                let mut result: int = 0;
                if x >= 0 {
                    result = x;
                } else {
                    result = 0 - x;
                }
                result
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function abs"));
        assert!(output.dtal.contains("cmp")); // comparison
        assert!(output.dtal.contains("jmp")); // jump
        assert!(output.dtal.contains("ret"));

        println!("=== Abs Function ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_multi_function() {
        let source = r#"
            fn helper(n: int) -> int {
                n * 2
            }

            fn main() -> int {
                let result: int = helper(21);
                result
            }
        "#;

        let result = compile(source);
        assert!(result.is_ok(), "Compilation failed: {:?}", result.err());

        let output = result.unwrap();
        assert!(output.dtal.contains(".function helper"));
        assert!(output.dtal.contains(".function main"));
        assert!(output.dtal.contains("call helper"));
        assert!(output.dtal.contains("mul")); // multiply

        println!("=== Multi-function Program ===\n{}", output.dtal);
    }

    #[test]
    fn test_compile_type_error() {
        let source = r#"
            fn bad() -> int {
                true
            }
        "#;

        let result = compile(source);
        assert!(result.is_err());
        match result {
            Err(CompileError::TypeError(_)) => (),
            other => panic!("Expected TypeError, got {:?}", other),
        }
    }

    #[test]
    fn test_compile_parse_error() {
        let source = r#"
            fn bad( -> int {
                42
            }
        "#;

        let result = compile(source);
        assert!(result.is_err());
        match result {
            Err(CompileError::ParseError(_)) => (),
            other => panic!("Expected ParseError, got {:?}", other),
        }
    }
}
