use chumsky::prelude::*;
use crate::common::ast::Program;
use crate::common::tast::TProgram;
use crate::frontend::lexer::lexer;
use crate::frontend::parser::program_parser;
use crate::frontend::typechecker::check_program;

/// Run the full compilation pipeline: lex, parse, and type check
pub fn run_pipeline<'src>(src: &'src str, verbose: bool) -> Result<TProgram<'src>, String> {
    // Lexer
    println!("\n[1] Lexing...");
    let (tokens, lex_errors) = lexer().parse(src).into_output_errors();

    // Handle lexer errors
    if !lex_errors.is_empty() {
        println!("Lexer errors:");
        for error in &lex_errors {
            eprintln!("  Error at {:?}: {:?}", error.span(), error);
        }
    }

    let tokens = match tokens {
        Some(t) => t,
        None => {
            return Err("Lexing failed. Aborting.".to_string());
        }
    };

    println!("Lexed {} tokens", tokens.len());

    // Print tokens if verbose
    if verbose {
        println!("\nTokens:");
        for (i, (token, span)) in tokens.iter().enumerate() {
            println!("  {:3}: {:?} @ {:?}", i, token, span);
        }
    }

    // Parser
    println!("\n[2] Parsing...");
    let (program, parse_errors) = program_parser()
        .parse(tokens.as_slice().map((src.len()..src.len()).into(), |(t, s)| (t, s)))
        .into_output_errors();

    // Handle parse errors
    if !parse_errors.is_empty() {
        println!("Parse errors:");
        for error in &parse_errors {
            eprintln!("  Error at {:?}: {}", error.span(), error);
        }
    }

    let program = match program {
        Some(p) => p,
        None => {
            return Err("Parsing failed. Aborting.".to_string());
        }
    };

    println!("Parsed successfully!");

    // Type checker
    println!("\n[3] Type checking...");
    let typed_program = check_program(&program)
        .map_err(|e| format!("Type error: {:?}", e))?;

    println!("Type checking passed!");

    Ok(typed_program)
}
