use std::env;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::time::Instant;
use veritas::backend::elf::generate_elf;
use veritas::backend::optimise::OptConfig;
use veritas::backend::x86_64::{Encoder, lower_program as lower_to_x86};
use veritas::frontend::typechecker::smt::{get_frontend_smt_stats, reset_frontend_smt_stats};
use veritas::pipeline::{CompileError, compile_verbose, compile_verbose_optimized};
use veritas::verifier::smt::{get_verifier_smt_stats, reset_verifier_smt_stats};
use veritas::verifier::verify_dtal;

fn print_bench_json(
    file: &str,
    compile_time: &std::time::Duration,
    verify_time: Option<&std::time::Duration>,
    binary_size: Option<usize>,
) {
    let (fe_queries, fe_time_ns) = get_frontend_smt_stats();
    let (ver_queries, ver_time_ns) = get_verifier_smt_stats();
    let verify_ms = verify_time.map(|d| d.as_secs_f64() * 1000.0);
    println!(
        r#"{{"file":"{}","compile_ms":{:.3},"verify_ms":{},"binary_bytes":{},"frontend_smt_queries":{},"frontend_smt_ms":{:.3},"verifier_smt_queries":{},"verifier_smt_ms":{:.3}}}"#,
        file,
        compile_time.as_secs_f64() * 1000.0,
        verify_ms.map_or("null".to_string(), |v| format!("{:.3}", v)),
        binary_size.map_or("null".to_string(), |v| v.to_string()),
        fe_queries,
        fe_time_ns as f64 / 1_000_000.0,
        ver_queries,
        ver_time_ns as f64 / 1_000_000.0,
    );
}

fn main() {
    // Parse command line arguments
    let args: Vec<String> = env::args().collect();

    let show_help = args.iter().any(|a| a == "--help" || a == "-h");

    if args.len() < 2 || show_help {
        eprintln!("Usage: {} <source_file> [OPTIONS]", args[0]);
        eprintln!();
        eprintln!("Options:");
        eprintln!("  -h, --help       Show this help message");
        eprintln!("  -v, --verbose    Show compilation stages");
        eprintln!("  --tokens         Show lexer tokens");
        eprintln!("  --ast            Show typed AST");
        eprintln!("  --tir            Show TIR (SSA form)");
        eprintln!("  --verify         Verify DTAL output");
        eprintln!("  --verify-only    Verify DTAL and exit (no output)");
        eprintln!("  --verify-dtal    Verify a standalone .dtal file");
        eprintln!("  -o <file>        Output native executable (ELF)");
        eprintln!("  --native         Generate native x86-64 code (to stdout)");
        eprintln!();
        eprintln!("Optimization:");
        eprintln!("  -O, --optimize   Enable all optimizations");
        eprintln!("  --copy-prop      Enable copy propagation only");
        eprintln!("  --dce            Enable dead code elimination only");
        eprintln!();
        eprintln!("Environment variables:");
        eprintln!(
            "  VERITAS_LS=1           Use linear scan register allocator (default: graph coloring)"
        );
        eprintln!("  VERITAS_DEBUG_ALLOC=1  Dump register allocation details to stderr");
        std::process::exit(if show_help { 0 } else { 1 });
    }

    let file_path = &args[1];

    // Handle --verify-dtal mode: standalone DTAL verification
    if args.iter().any(|a| a == "--verify-dtal") {
        let src = match fs::read_to_string(file_path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading file '{}': {}", file_path, e);
                std::process::exit(1);
            }
        };

        match veritas::verifier::verify_dtal_text(&src) {
            Ok(()) => {
                println!("Verification passed!");
            }
            Err(e) => {
                eprintln!("Verification FAILED: {}", e);
                std::process::exit(1);
            }
        }
        return;
    }

    let verbose = args.iter().any(|a| a == "--verbose" || a == "-v");
    let quiet = args.iter().any(|a| a == "--quiet" || a == "-q");
    let bench = args.iter().any(|a| a == "--bench");
    let show_tokens = args.iter().any(|a| a == "--tokens");
    let show_ast = args.iter().any(|a| a == "--ast");
    let show_tir = args.iter().any(|a| a == "--tir");
    let verify = args.iter().any(|a| a == "--verify" || a == "--verify-only");
    let verify_only = args.iter().any(|a| a == "--verify-only");
    let native = args.iter().any(|a| a == "--native");
    let output_file = args
        .iter()
        .position(|a| a == "-o")
        .and_then(|i| args.get(i + 1));

    // Optimization flags
    let optimize_all = args.iter().any(|a| a == "-O" || a == "--optimize");
    let copy_prop = args.iter().any(|a| a == "--copy-prop");
    let dce = args.iter().any(|a| a == "--dce");

    // Build optimization config
    let opt_config = if optimize_all {
        OptConfig::all()
    } else {
        OptConfig {
            copy_propagation: copy_prop,
            dead_code_elimination: dce,
            max_iterations: Some(10),
        }
    };

    if !quiet && !bench {
        println!("\n{}", file_path);
        println!("{}", "=".repeat(60));
    }

    // Read the source file
    let src = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            std::process::exit(1);
        }
    };

    if !quiet && !bench {
        // Print the source code
        println!("\nSource code:");
        println!("{}", "-".repeat(60));
        println!("{}", src);
        println!("{}", "-".repeat(60));
    }

    // Reset SMT stats before compilation
    reset_frontend_smt_stats();
    reset_verifier_smt_stats();

    // Run the full compilation pipeline
    if verbose {
        println!("\n[1] Lexing...");
    }

    let compile_start = Instant::now();

    // Use optimized compilation if any optimizations are enabled
    let compile_result = if opt_config.any_enabled() {
        compile_verbose_optimized(&src, &opt_config)
    } else {
        compile_verbose(&src)
    };

    let compile_elapsed = compile_start.elapsed();

    match compile_result {
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
                if opt_config.any_enabled() {
                    println!("\n[5.5] Optimizing...");
                    if opt_config.copy_propagation {
                        println!("  - Copy propagation enabled");
                    }
                    if opt_config.dead_code_elimination {
                        println!("  - Dead code elimination enabled");
                    }
                }
                println!("\n[6] Emitting output...");
            } else {
                let opt_status = if opt_config.any_enabled() {
                    " (optimized)"
                } else {
                    ""
                };
                if !quiet && !bench {
                    println!("\nCompilation successful!{}", opt_status);
                }
            }

            // Verify DTAL if requested
            let mut verify_elapsed = std::time::Duration::ZERO;
            if verify {
                if verbose {
                    println!("\n[7] Verifying DTAL...");
                }
                let verify_start = Instant::now();
                match verify_dtal(&output.dtal_program) {
                    Ok(()) => {
                        verify_elapsed = verify_start.elapsed();
                        if !quiet && !bench {
                            println!("\nVerification passed!");
                        }
                    }
                    Err(e) => {
                        eprintln!("\nVerification FAILED: {}", e);
                        std::process::exit(1);
                    }
                }

                if verify_only {
                    if bench {
                        print_bench_json(file_path, &compile_elapsed, Some(&verify_elapsed), None);
                    }
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

            // Show DTAL output unless generating native code
            if !native && output_file.is_none() {
                println!("\n{}", "=".repeat(60));
                println!("DTAL Output:");
                println!("{}", "=".repeat(60));
                println!("{}", output.dtal);
            }

            // Generate native code if requested
            if native || output_file.is_some() {
                if verbose {
                    println!("\n[8] Lowering to x86-64...");
                }

                // Lower DTAL to x86-64
                let x86_program = lower_to_x86(&output.dtal_program);

                if verbose {
                    println!("Generated {} function(s)", x86_program.functions.len());
                    println!("\n[9] Encoding x86-64 instructions...");
                }

                // Encode to machine code
                let mut encoder = Encoder::new();
                let encoded = encoder.encode_program(&x86_program);

                if verbose {
                    println!("Encoded {} bytes of machine code", encoded.code.len());
                }

                if let Some(out_path) = output_file {
                    if verbose {
                        println!("\n[10] Generating ELF executable...");
                    }

                    // Determine entry point (use "main" or first function)
                    let entry = if encoded.symbols.contains_key("main") {
                        "main"
                    } else {
                        x86_program
                            .functions
                            .first()
                            .map(|f| f.name.as_str())
                            .unwrap_or("main")
                    };

                    // Generate ELF
                    let elf = generate_elf(&encoded, entry);

                    // Write executable
                    if let Err(e) = fs::write(out_path, &elf) {
                        eprintln!("Error writing output file '{}': {}", out_path, e);
                        std::process::exit(1);
                    }

                    // Make executable
                    if let Ok(metadata) = fs::metadata(out_path) {
                        let mut perms = metadata.permissions();
                        perms.set_mode(0o755);
                        let _ = fs::set_permissions(out_path, perms);
                    }

                    if !quiet && !bench {
                        println!("\nGenerated executable: {} ({} bytes)", out_path, elf.len());
                    }
                    if bench {
                        print_bench_json(
                            file_path,
                            &compile_elapsed,
                            if verify { Some(&verify_elapsed) } else { None },
                            Some(elf.len()),
                        );
                    }
                } else if native {
                    // Print x86-64 assembly
                    println!("\n{}", "=".repeat(60));
                    println!("x86-64 Assembly:");
                    println!("{}", "=".repeat(60));
                    for func in &x86_program.functions {
                        println!("\n{}:", func.name);
                        for instr in &func.instructions {
                            println!("    {}", instr);
                        }
                    }

                    println!("\n{}", "=".repeat(60));
                    println!("Machine Code ({} bytes):", encoded.code.len());
                    println!("{}", "=".repeat(60));
                    for (i, chunk) in encoded.code.chunks(16).enumerate() {
                        print!("{:08x}  ", i * 16);
                        for byte in chunk {
                            print!("{:02x} ", byte);
                        }
                        println!();
                    }
                }
            }
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
