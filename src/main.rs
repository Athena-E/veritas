use std::env;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use veritas::backend::elf::generate_elf;
use veritas::backend::x86_64::{lower_program as lower_to_x86, Encoder};
use veritas::pipeline::{CompileError, compile_verbose};
use veritas::verifier::verify_dtal;

fn main() {
    // Parse command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source_file> [OPTIONS]", args[0]);
        eprintln!();
        eprintln!("Options:");
        eprintln!("  --verbose, -v    Show compilation stages");
        eprintln!("  --tokens         Show lexer tokens");
        eprintln!("  --ast            Show typed AST");
        eprintln!("  --tir            Show TIR (SSA form)");
        eprintln!("  --verify         Verify DTAL output");
        eprintln!("  --verify-only    Verify DTAL and exit (no output)");
        std::process::exit(1);
    }

    let file_path = &args[1];
    let verbose = args.iter().any(|a| a == "--verbose" || a == "-v");
    let show_tokens = args.iter().any(|a| a == "--tokens");
    let show_ast = args.iter().any(|a| a == "--ast");
    let show_tir = args.iter().any(|a| a == "--tir");
    let verify = args.iter().any(|a| a == "--verify" || a == "--verify-only");
    let verify_only = args.iter().any(|a| a == "--verify-only");

    println!("\n{}", file_path);
    println!("{}", "=".repeat(60));

    // Read the source file
    let src = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            std::process::exit(1);
        }
    };

    // Print the source code
    println!("\nSource code:");
    println!("{}", "-".repeat(60));
    println!("{}", src);
    println!("{}", "-".repeat(60));

    // Run the full compilation pipeline
    if verbose {
        println!("\n[1] Lexing...");
    }

    match compile_verbose(&src) {
        Ok(output) => {
            if verbose {
                println!("Lexed {} tokens", output.tokens.len());
                println!("\n[2] Parsing...");
                println!("Parsed successfully!");
                println!("\n[3] Type checking...");
                println!("Type checking passed!");
                println!(
                    "\n[4] Lowering to TIR ({} function(s))...",
                    output.tir.functions.len()
                );
                println!("\n[5] Generating DTAL...");
                println!("\n[6] Emitting output...");
            } else {
                println!("\nCompilation successful!");
            }

            // Verify DTAL if requested
            if verify {
                if verbose {
                    println!("\n[7] Verifying DTAL...");
                }
                match verify_dtal(&output.dtal_program) {
                    Ok(()) => {
                        println!("\nVerification passed!");
                    }
                    Err(e) => {
                        eprintln!("\nVerification FAILED: {}", e);
                        std::process::exit(1);
                    }
                }

                if verify_only {
                    return;
                }
            }

            // Show tokens if requested
            if show_tokens {
                println!("\n{}", "=".repeat(60));
                println!("Tokens ({}):", output.tokens.len());
                println!("{}", "=".repeat(60));
                for (i, (token, span)) in output.tokens.iter().enumerate() {
                    println!("  {:3}: {} @ {}", i, token, span);
                }
            }

            // Show typed AST if requested
            if show_ast {
                println!("\n{}", "=".repeat(60));
                println!("Typed AST:");
                println!("{}", "=".repeat(60));
                for func in &output.tast.functions {
                    println!("\nFunction: {}", func.name);
                    println!("  Parameters:");
                    for param in &func.parameters {
                        println!("    {}: {:?}", param.name, param.ty);
                    }
                    println!("  Return type: {:?}", func.return_type);
                    println!("  Body: {:#?}", func.body);
                }
            }

            // Show TIR if requested
            if show_tir {
                println!("\n{}", "=".repeat(60));
                println!("TIR (SSA form):");
                println!("{}", "=".repeat(60));
                for func in &output.tir.functions {
                    println!("\nFunction: {}", func.name);
                    println!("  Entry block: {:?}", func.entry_block);
                    println!("  Blocks:");
                    for (id, block) in &func.blocks {
                        println!("    Block {:?}:", id);
                        if !block.phi_nodes.is_empty() {
                            println!("      Phi nodes:");
                            for phi in &block.phi_nodes {
                                println!("        {:?}", phi);
                            }
                        }
                        println!("      Instructions:");
                        for instr in &block.instructions {
                            println!("        {:?}", instr);
                        }
                        println!("      Terminator: {:?}", block.terminator);
                    }
                }
            }

            println!("\n{}", "=".repeat(60));
            println!("DTAL Output:");
            println!("{}", "=".repeat(60));
            println!("{}", output.dtal);
        }
        Err(e) => {
            println!();
            match e {
                CompileError::LexError(msg) => {
                    eprintln!("Lexer error:\n{}", msg);
                }
                CompileError::ParseError(msg) => {
                    eprintln!("Parse error:\n{}", msg);
                }
                CompileError::TypeError(type_error) => {
                    veritas::frontend::typechecker::report_type_error(file_path, &src, &type_error);
                }
            }
            std::process::exit(1);
        }
    }
}
