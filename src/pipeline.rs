//! Veritas Compiler Pipeline
//!
//! This module provides the end-to-end compilation pipeline from source code
//! to DTAL (Dependently Typed Assembly Language) output.
//!
//! Pipeline Stages:
//!
//! Source Code (&str)
//!     │
//!     v lexer
//! Tokens (Vec<Spanned<Token>>)
//!     │
//!     v parser
//! AST (Program)
//!     │
//!     v typechecker
//! Typed AST (TProgram)
//!     │
//!     v lower
//! TIR (TirProgram) - SSA form
//!     │
//!     v codegen
//! DTAL (DtalProgram)
//!     │
//!     v emit
//! Output (String)
//!

use crate::backend::optimise::{OptConfig, optimize_program};
use crate::backend::{codegen_program, emit_program, lower_program};
use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::helpers::reset_fresh_var_counter;
use crate::frontend::typechecker::{TypeError, check_program, report_type_error};
use chumsky::prelude::*;
use std::fmt;

fn reset_pipeline_state() {
    reset_fresh_var_counter();
}

/// Compilation error types
#[derive(Debug)]
pub enum CompileError<'src> {
    LexError(String),
    ParseError(String),
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
    pub dtal: String,
}

/// Verbose compilation output with all intermediate stages
pub struct VerboseOutput<'src> {
    pub tokens: Vec<(String, String)>,
    pub tast: crate::common::tast::TProgram<'src>,
    pub tir: crate::backend::TirProgram<'src>,
    pub dtal_program: crate::backend::dtal::instr::DtalProgram,
    pub dtal: String,
}

/// Compile source code to DTAL assembly
///
/// Main entry point for the compiler pipeline.
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
    reset_pipeline_state();

    // Lexical analysis
    let tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

    // Parsing
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

    // Type checking
    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    // Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Generate DTAL
    let dtal_program = codegen_program(&tir);

    // Emit text
    let dtal = emit_program(&dtal_program);

    Ok(CompileOutput { dtal })
}

/// Compile source code to DTAL assembly with optimisation
///
/// This is like `compile` but allows specifying optimisation options.
///
/// # Arguments
///
/// * `source` - The source code to compile
/// * `opt_config` - Configuration for optimisation passes
///
/// # Returns
///
/// * `Ok(CompileOutput)` - Successful compilation with DTAL output
/// * `Err(CompileError)` - Compilation failed at some stage
pub fn compile_optimized<'src>(
    source: &'src str,
    opt_config: &OptConfig,
) -> Result<CompileOutput, CompileError<'src>> {
    reset_pipeline_state();

    // Lexical analysis
    let tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

    // Parsing
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

    // Type checking
    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    // Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Generate DTAL
    let mut dtal_program = codegen_program(&tir);

    // Optimise (if enabled)
    if opt_config.any_enabled() {
        optimize_program(&mut dtal_program, opt_config);
    }

    // Emit text
    let dtal = emit_program(&dtal_program);

    Ok(CompileOutput { dtal })
}

/// Compile source code with verbose output, returning all intermediate stages
pub fn compile_verbose(source: &str) -> Result<VerboseOutput<'_>, CompileError<'_>> {
    reset_pipeline_state();

    // Lexical analysis
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

    // Parsing
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

    // Type checking
    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    // Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Generate DTAL
    let dtal_program = codegen_program(&tir);

    // Emit text
    let dtal = emit_program(&dtal_program);

    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal_program,
        dtal,
    })
}

/// Compile source code for bare-metal target (no Linux intrinsics)
pub fn compile_verbose_bare_metal(source: &str) -> Result<VerboseOutput<'_>, CompileError<'_>> {
    use crate::frontend::typechecker::check_program_bare_metal;

    reset_pipeline_state();

    let raw_tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;
    let token_strings: Vec<(String, String)> = raw_tokens
        .iter()
        .map(|(tok, span)| (format!("{:?}", tok), format!("{:?}", span)))
        .collect();
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
    let tast = check_program_bare_metal(&ast).map_err(CompileError::TypeError)?;
    let tir = lower_program(&tast);
    let dtal_program = crate::backend::codegen::codegen_program_with_target(&tir, true);
    let dtal = emit_program(&dtal_program);
    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal_program,
        dtal,
    })
}

/// Compile source code with verbose output and optimisation
pub fn compile_verbose_optimized<'src>(
    source: &'src str,
    opt_config: &OptConfig,
) -> Result<VerboseOutput<'src>, CompileError<'src>> {
    compile_verbose_configured(source, Some(opt_config), false)
}

/// Compile source code with explicit target and optimisation options.
pub fn compile_verbose_configured<'src>(
    source: &'src str,
    opt_config: Option<&OptConfig>,
    bare_metal: bool,
) -> Result<VerboseOutput<'src>, CompileError<'src>> {
    reset_pipeline_state();

    // Lexical analysis
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

    // Parsing
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

    // Type checking
    let tast = if bare_metal {
        crate::frontend::typechecker::check_program_bare_metal(&ast)
    } else {
        check_program(&ast)
    }
    .map_err(CompileError::TypeError)?;

    // Lower to TIR (SSA form)
    let tir = lower_program(&tast);

    // Generate DTAL
    let mut dtal_program = if bare_metal {
        crate::backend::codegen::codegen_program_with_target(&tir, true)
    } else {
        codegen_program(&tir)
    };

    // Optimise (if enabled)
    if let Some(opt_config) = opt_config
        && opt_config.any_enabled()
    {
        optimize_program(&mut dtal_program, opt_config);
    }

    // Emit text
    let dtal = emit_program(&dtal_program);

    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal_program,
        dtal,
    })
}

/// Compile source code and report errors with source context
///
/// Prints pretty error messages when compilation fails.
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
