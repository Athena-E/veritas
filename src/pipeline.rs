//! End-to-end Veritas compilation pipeline.
//!
//! The pipeline turns source text into tokens, AST, typed AST, TIR, DTAL, and
//! finally emitted DTAL text. Verbose entry points expose the intermediate
//! stages for tests and debugging.
//!
//! # Stages
//!
//! ```text
//! source
//!   -> lexer tokens
//!   -> parsed AST
//!   -> typed AST
//!   -> TIR
//!   -> DTAL program
//!   -> emitted DTAL text
//! ```
//!
//! # Design Notes
//!
//! Each public entry point resets frontend freshness state before compiling so
//! repeated calls are deterministic. Optimisation is optional and runs after
//! DTAL generation, which keeps the default `compile` path close to the source
//! lowering behavior.
//!
//! # Errors
//!
//! [`CompileError`] reports the first failed stage: lexing, parsing, or type
//! checking. Later verifier errors are surfaced by consumers that explicitly
//! verify emitted DTAL through [`crate::verifier`].
//!
//! # Related Modules
//!
//! [`crate::frontend`] owns lexing, parsing, and type checking; [`crate::middle`]
//! owns TIR lowering; [`crate::backend`] owns DTAL generation and target-facing
//! stages.

use crate::backend::optimise::{OptConfig, optimize_program};
use crate::backend::{codegen_program, emit_program};
use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::helpers::reset_fresh_var_counter;
use crate::frontend::typechecker::{TypeError, check_program, report_type_error};
use crate::middle::lower_program;
use chumsky::prelude::*;
use std::fmt;

fn reset_pipeline_state() {
    reset_fresh_var_counter();
}
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
#[derive(Debug, Clone)]

pub struct CompileOutput {
    pub dtal: String,
}

pub struct VerboseOutput<'src> {
    pub tokens: Vec<(String, String)>,
    pub tast: crate::common::tast::TProgram<'src>,
    pub tir: crate::middle::TirProgram<'src>,
    pub dtal_program: crate::dtal::instr::DtalProgram,
    pub dtal: String,
}

pub fn compile(source: &str) -> Result<CompileOutput, CompileError<'_>> {
    reset_pipeline_state();

    let tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

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

    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    let tir = lower_program(&tast);

    let dtal_program = codegen_program(&tir);

    let dtal = emit_program(&dtal_program);

    Ok(CompileOutput { dtal })
}

pub fn compile_optimized<'src>(
    source: &'src str,
    opt_config: &OptConfig,
) -> Result<CompileOutput, CompileError<'src>> {
    reset_pipeline_state();

    let tokens = lexer().parse(source).into_result().map_err(|errors| {
        CompileError::LexError(
            errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    })?;

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

    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    let tir = lower_program(&tast);

    let mut dtal_program = codegen_program(&tir);

    if opt_config.any_enabled() {
        optimize_program(&mut dtal_program, opt_config);
    }

    let dtal = emit_program(&dtal_program);

    Ok(CompileOutput { dtal })
}

pub fn compile_verbose(source: &str) -> Result<VerboseOutput<'_>, CompileError<'_>> {
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

    let tast = check_program(&ast).map_err(CompileError::TypeError)?;

    let tir = lower_program(&tast);

    let dtal_program = codegen_program(&tir);

    let dtal = emit_program(&dtal_program);

    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal_program,
        dtal,
    })
}

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

pub fn compile_verbose_optimized<'src>(
    source: &'src str,
    opt_config: &OptConfig,
) -> Result<VerboseOutput<'src>, CompileError<'src>> {
    compile_verbose_configured(source, Some(opt_config), false)
}

pub fn compile_verbose_configured<'src>(
    source: &'src str,
    opt_config: Option<&OptConfig>,
    bare_metal: bool,
) -> Result<VerboseOutput<'src>, CompileError<'src>> {
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

    let tast = if bare_metal {
        crate::frontend::typechecker::check_program_bare_metal(&ast)
    } else {
        check_program(&ast)
    }
    .map_err(CompileError::TypeError)?;

    let tir = lower_program(&tast);

    let mut dtal_program = if bare_metal {
        crate::backend::codegen::codegen_program_with_target(&tir, true)
    } else {
        codegen_program(&tir)
    };

    if let Some(opt_config) = opt_config
        && opt_config.any_enabled()
    {
        optimize_program(&mut dtal_program, opt_config);
    }

    let dtal = emit_program(&dtal_program);

    Ok(VerboseOutput {
        tokens: token_strings,
        tast,
        tir,
        dtal_program,
        dtal,
    })
}

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
        assert!(output.dtal.contains("add "));
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
        assert!(output.dtal.contains("mov"));
        assert!(output.dtal.contains("5"));
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
        assert!(output.dtal.contains("add "));

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
        assert!(output.dtal.contains("cmp"));
        assert!(output.dtal.contains("jmp"));
        assert!(output.dtal.contains("ret"));

        println!("=== Abs Function ===\n{}", output.dtal);
    }
    #[test]

    fn test_compile_branch_joined_singletons_as_array_index() {
        let source = r#"
            fn choose(flag: bool) -> int {
                let arr: [int; 2] = [7; 2];
                let mut i: int = 0;
                if flag {
                    i = 0;
                } else {
                    i = 1;
                }
                arr[i]
            }
        "#;

        let result = compile(source);
        assert!(
            result.is_ok(),
            "Compilation should preserve singleton alternatives across join: {:?}",
            result.err()
        );
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
        assert!(output.dtal.contains("mul"));

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
