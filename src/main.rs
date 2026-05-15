use std::env;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::time::Instant;
use veritas::backend::elf::generate_elf;
use veritas::backend::optimise::OptConfig;
use veritas::backend::x86_64::{Encoder, lower_program as lower_to_x86};
use veritas::frontend::typechecker::smt::{get_frontend_smt_stats, reset_frontend_smt_stats};
use veritas::pipeline::{CompileError, compile_verbose, compile_verbose_configured};
use veritas::verifier::smt::{get_verifier_smt_stats, reset_verifier_smt_stats};
use veritas::verifier::verify_dtal;

struct TamperCase {
    id: &'static str,
    name: &'static str,
    source: &'static str,
    mutate: fn(&str) -> String,
    expected_error: &'static str,
}

fn replace_once(haystack: &str, needle: &str, replacement: &str) -> String {
    assert!(
        haystack.contains(needle),
        "tamper needle not found: {:?}",
        needle
    );
    haystack.replacen(needle, replacement, 1)
}

fn compile_to_dtal(source: &str) -> String {
    compile_verbose(source)
        .unwrap_or_else(|err| panic!("expected source to compile successfully: {}", err))
        .dtal
}

fn dtal_tamper_cases() -> [TamperCase; 18] {
    [
        TamperCase {
            id: "T01",
            name: "return_signature_mismatch",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| replace_once(dtal, ".returns int", ".returns bool"),
            expected_error: "Return type mismatch",
        },
        TamperCase {
            id: "T02",
            name: "strengthened_precondition_breaks_call_site",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    ".precondition (v0 >= 0 && v0 < 4)",
                    ".precondition (v0 >= 0 && v0 < 2)",
                );
                replace_once(
                    &dtal,
                    ".assume (v0 >= 0 && v0 < 4)",
                    ".assume (v0 >= 0 && v0 < 2)",
                )
            },
            expected_error: "Precondition not provable",
        },
        TamperCase {
            id: "T03",
            name: "shared_borrow_out_of_bounds_index",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T04",
            name: "move_owned_while_shared_borrow_live",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]",
                    "    move_owned v6, v1    : [int(7); 1]\n    borrow_end v3    : &[int(7); 1]",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            id: "T05",
            name: "i64_add_overflow_from_tampered_constants",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    "mov v0, 42    : int(42)",
                    "mov v0, 9223372036854775807    : int(9223372036854775807)",
                );
                let dtal = replace_once(
                    dtal.as_str(),
                    "mov v1, 10    : int(10)",
                    "mov v1, 1    : int(1)",
                );
                replace_once(
                    dtal.as_str(),
                    "add v2, v0, v1    : int(52)",
                    "add v2, v0, v1    : i64",
                )
            },
            expected_error: "Arithmetic overflow",
        },
        TamperCase {
            id: "T06",
            name: "shared_borrow_negative_index",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| replace_once(dtal, "mov v4, 0    : int", "mov v4, -1    : int(-1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T07",
            name: "use_after_drop_owned",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]\n    push v5",
                    "    borrow_end v3    : &[int(7); 1]\n    drop_owned v1    : [int(7); 1]\n    move_owned v6, v1    : [int(7); 1]\n    push v5",
                )
            },
            expected_error: "used after ownership was consumed",
        },
        TamperCase {
            id: "T08",
            name: "plain_mov_duplicates_owned_value",
            source: include_str!("../eval/feature_suite/programs/34_shared_scalar_deref.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    borrow_end v3    : &[int(7); 1]\n    push v5",
                    "    borrow_end v3    : &[int(7); 1]\n    mov v6, v1    : [int(7); 1]\n    push v5",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            id: "T09",
            name: "alias_shared_while_mutable_borrow_live",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "borrow_mut r0, v0    : int",
                    "borrow_mut r0, v0    : int\n    alias_borrow v9, v0    : &[int; 1]",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            id: "T10",
            name: "double_mutable_borrow",
            source: include_str!(
                "../eval/feature_suite/programs/37_mutable_scalar_borrow_call.veri"
            ),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "borrow_mut r0, v1    : int",
                    "borrow_mut r0, v1    : int\n    borrow_mut r1, v1    : int",
                )
            },
            expected_error: "Ownership violation",
        },
        TamperCase {
            id: "T11",
            name: "mutable_store_out_of_bounds_index",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| replace_once(dtal, "mov v1, 0    : int(0)", "mov v1, 1    : int(1)"),
            expected_error: "Bounds check failed",
        },
        TamperCase {
            id: "T12",
            name: "entry_param_type_weakened_from_mutable_to_shared",
            source: include_str!("../eval/feature_suite/programs/33_mutable_borrow.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    ".params {v0: &mut [int; 1]}",
                    ".params {v0: &[int; 1]}",
                )
            },
            expected_error: "Type mismatch",
        },
        TamperCase {
            id: "T13",
            name: "tampered_entry_state_type",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| replace_once(dtal, ".entry {v0: int}", ".entry {v0: bool}"),
            expected_error: "Type mismatch",
        },
        TamperCase {
            id: "T14",
            name: "impossible_assertion_inserted",
            source: include_str!("../eval/feature_suite/programs/17_preconditions.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    .assume (v0 >= 0 && v0 < 4)",
                    "    .assume (v0 >= 0 && v0 < 4)\n    .assert (v0 < 0)",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            id: "T15",
            name: "branch_target_swap_breaks_edge_assumption",
            source: include_str!("../eval/feature_suite/programs/02_conditionals.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    "    bgt .max_of_bb1\n    jmp .max_of_bb2",
                    "    bgt .max_of_bb2\n    jmp .max_of_bb1",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            id: "T16",
            name: "false_singleton_annotation_after_mov",
            source: include_str!("../eval/feature_suite/programs/01_simple.veri"),
            mutate: |dtal| replace_once(dtal, "mov v0, 42    : int(42)", "mov v0, 42    : int(41)"),
            expected_error: "Singleton type mismatch",
        },
        TamperCase {
            id: "T17",
            name: "division_nonzero_evidence_removed",
            source: include_str!("../eval/feature_suite/programs/21_safe_division.veri"),
            mutate: |dtal| {
                let dtal = replace_once(
                    dtal,
                    ".params {v0: int, v1: {v: int | v != 0 }}",
                    ".params {v0: int, v1: int}",
                );
                replace_once(
                    &dtal,
                    ".entry {v0: int, v1: {v: int | v != 0 }}",
                    ".entry {v0: int, v1: int}",
                )
            },
            expected_error: "Cannot prove constraint",
        },
        TamperCase {
            id: "T18",
            name: "postcondition_corrupted_to_unprovable_fact",
            source: include_str!("../eval/feature_suite/programs/38_sortedness.veri"),
            mutate: |dtal| {
                replace_once(
                    dtal,
                    ".postcondition (forall i in 0..2 { v7[i] <= v7[(i + 1)] })",
                    ".postcondition v7[0] > v7[1]",
                )
            },
            expected_error: "Postcondition not provable",
        },
    ]
}

fn option_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

fn generate_dtal_tampering(out_dir: &str) {
    let out_dir = PathBuf::from(out_dir);
    fs::create_dir_all(&out_dir)
        .unwrap_or_else(|err| panic!("failed to create {}: {}", out_dir.display(), err));

    for case in dtal_tamper_cases() {
        let dtal = compile_to_dtal(case.source);
        let tampered = (case.mutate)(&dtal);
        let file_name = format!("{}_{}.dtal", case.id, case.name);
        let path = out_dir.join(file_name);
        fs::write(&path, tampered)
            .unwrap_or_else(|err| panic!("failed to write {}: {}", path.display(), err));
        println!("{}\t{}\t{}", case.id, path.display(), case.expected_error);
    }
}

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
    let args: Vec<String> = env::args().collect();
    let generate_tampering = args.iter().any(|a| a == "--generate-dtal-tampering");

    let show_help = args.iter().any(|a| a == "--help" || a == "-h");

    if generate_tampering {
        let out_dir =
            option_value(&args, "--tampering-out-dir").unwrap_or("eval/dtal_tampering/generated");
        generate_dtal_tampering(out_dir);
        return;
    }

    if args.len() < 2 || show_help {
        eprintln!("Veritas Compiler");
        eprintln!();
        eprintln!("Usage: {} <source_file> [OPTIONS]", args[0]);
        eprintln!();
        eprintln!("Output:");
        eprintln!("  -o <file>            Compile to native ELF executable");
        eprintln!("  --native             Print x86-64 assembly to stdout");
        eprintln!("  --target-bare-metal  Generate Multiboot ELF for bare-metal/QEMU:");
        eprintln!("                       qemu-system-x86_64 -kernel <binary> -serial stdio");
        eprintln!();
        eprintln!("Verification:");
        eprintln!("  --verify             Verify DTAL before code generation");
        eprintln!("  --verify-only        Verify DTAL and exit (no codegen)");
        eprintln!("  --verify-dtal        Verify a standalone .dtal file");
        eprintln!();
        eprintln!("Optimisation:");
        eprintln!("  -O, --optimize       Enable all optimisations");
        eprintln!();
        eprintln!("Debug:");
        eprintln!("  -v, --verbose        Show compilation stages");
        eprintln!("  --tokens             Show lexer tokens");
        eprintln!("  --ast                Show typed AST");
        eprintln!("  --tir                Show TIR (SSA form)");
        eprintln!("  -q, --quiet          Suppress non-error output");
        eprintln!("  --bench              Output benchmark JSON");
        eprintln!("  -h, --help           Show this help message");
        eprintln!();
        eprintln!("Runtime intrinsics:");
        eprintln!("  print_int(n: int)      Print integer + newline to stdout");
        eprintln!("  print_char(c: int)     Print single byte to stdout");
        eprintln!("  read_int() -> int      Read decimal integer from stdin");
        eprintln!();
        eprintln!("Development:");
        eprintln!("  --legacy-pipeline  Use the old trusted-lowering pipeline instead of");
        eprintln!("                     the default verify-after-regalloc pipeline");
        eprintln!("  --generate-dtal-tampering");
        eprintln!("                     Generate the tampered DTAL corpus");
        eprintln!("  --tampering-out-dir <dir>");
        eprintln!("                     Output directory for generated tampered DTAL");
        eprintln!("  --const-fold       Constant folding and immediate folding");
        eprintln!("  --peephole         Peephole simplifications");
        eprintln!("  --copy-prop        Copy propagation only");
        eprintln!("  --dce              Dead code elimination only");
        eprintln!("  --licm             Loop-invariant code motion");
        eprintln!("  --load-fusion      Fuse load+add patterns");
        eprintln!();
        eprintln!("Development Environment:");
        eprintln!("  VERITAS_LS=1           Use linear scan allocator (default: graph colouring)");
        eprintln!("  VERITAS_DEBUG_ALLOC=1  Dump register allocation details");
        std::process::exit(if show_help { 0 } else { 1 });
    }

    let file_path = &args[1];

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
    let physical = !args.iter().any(|a| a == "--legacy-pipeline");
    let bare_metal = args.iter().any(|a| a == "--target-bare-metal");
    let output_file = args
        .iter()
        .position(|a| a == "-o")
        .and_then(|i| args.get(i + 1));

    let optimize_all = args.iter().any(|a| a == "-O" || a == "--optimize");
    let const_fold = args.iter().any(|a| a == "--const-fold");
    let peephole = args.iter().any(|a| a == "--peephole");
    let copy_prop = args.iter().any(|a| a == "--copy-prop");
    let dce = args.iter().any(|a| a == "--dce");
    let licm = args.iter().any(|a| a == "--licm");
    let load_fusion = args.iter().any(|a| a == "--load-fusion");

    let opt_config = if optimize_all {
        OptConfig::all()
    } else {
        OptConfig {
            constant_folding: const_fold,
            peephole,
            copy_propagation: copy_prop,
            dead_code_elimination: dce,
            licm,
            load_fusion,
            max_iterations: Some(10),
        }
    };

    if !quiet && !bench {
        println!("\n{}", file_path);
        println!("{}", "=".repeat(60));
    }

    let src = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", file_path, e);
            std::process::exit(1);
        }
    };

    if !quiet && !bench {
        println!("\nSource code:");
        println!("{}", "-".repeat(60));
        println!("{}", src);
        println!("{}", "-".repeat(60));
    }

    reset_frontend_smt_stats();
    reset_verifier_smt_stats();

    if verbose {
        println!("\n[1] Lexing...");
    }

    let compile_start = Instant::now();

    let compile_result = if opt_config.any_enabled() || bare_metal {
        compile_verbose_configured(
            &src,
            opt_config.any_enabled().then_some(&opt_config),
            bare_metal,
        )
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

            reset_verifier_smt_stats();

            let mut verify_elapsed = std::time::Duration::ZERO;
            if verify && !physical {
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

            if show_tokens {
                println!("\n{}", "=".repeat(60));
                println!("Tokens ({}):", output.tokens.len());
                println!("{}", "=".repeat(60));
                for (i, (token, span)) in output.tokens.iter().enumerate() {
                    println!("  {:3}: {} @ {}", i, token, span);
                }
            }

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

            if !native && output_file.is_none() {
                println!("\n{}", "=".repeat(60));
                println!("DTAL Output:");
                println!("{}", "=".repeat(60));
                println!("{}", output.dtal);
            }

            if native || output_file.is_some() {
                let encoded = if physical {
                    if verbose {
                        println!("\n[8] Physical allocation (regalloc → physical DTAL)...");
                    }
                    let physical_dtal =
                        veritas::backend::physalloc::physically_allocate(&output.dtal_program);

                    if verify {
                        if verbose {
                            println!("\n[8b] Verifying physically-allocated DTAL...");
                        }
                        let verify_start = Instant::now();
                        match verify_dtal(&physical_dtal) {
                            Ok(()) => {
                                verify_elapsed = verify_start.elapsed();
                                if !quiet && !bench {
                                    println!("\nPhysical DTAL verification passed!");
                                }
                            }
                            Err(e) => {
                                eprintln!("\nPhysical DTAL verification FAILED: {}", e);
                                std::process::exit(1);
                            }
                        }

                        if verify_only {
                            if bench {
                                print_bench_json(
                                    file_path,
                                    &compile_elapsed,
                                    Some(&verify_elapsed),
                                    None,
                                );
                            }
                            return;
                        }
                    }

                    if verbose {
                        println!("\n[9] Direct encoding physical DTAL...");
                    }
                    veritas::backend::direct_encode::encode_physical_dtal(&physical_dtal)
                } else {
                    if verbose {
                        println!("\n[8] Lowering to x86-64...");
                    }
                    let x86_program = lower_to_x86(&output.dtal_program);

                    if verbose {
                        println!("Generated {} function(s)", x86_program.functions.len());
                        println!("\n[9] Encoding x86-64 instructions...");
                    }
                    let mut encoder = Encoder::new();
                    encoder.encode_program(&x86_program)
                };

                if verbose {
                    println!("Encoded {} bytes of machine code", encoded.code.len());
                }

                if let Some(out_path) = output_file {
                    if verbose {
                        println!("\n[10] Generating ELF executable...");
                    }

                    let entry = if encoded.symbols.contains_key("main") {
                        "main"
                    } else {
                        output
                            .dtal_program
                            .functions
                            .iter()
                            .find(|f| !f.blocks.is_empty())
                            .map(|f| f.name.as_str())
                            .unwrap_or("main")
                    };

                    let elf = if bare_metal {
                        veritas::backend::elf::generate_baremetal_elf(&encoded, entry)
                    } else {
                        generate_elf(&encoded, entry)
                    };

                    if let Err(e) = fs::write(out_path, &elf) {
                        eprintln!("Error writing output file '{}': {}", out_path, e);
                        std::process::exit(1);
                    }

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
                    if physical {
                        println!("\n{}", "=".repeat(60));
                        println!("Physical DTAL:");
                        println!("{}", "=".repeat(60));
                        let physical_dtal =
                            veritas::backend::physalloc::physically_allocate(&output.dtal_program);
                        let text = veritas::backend::emit::emit_program(&physical_dtal);
                        println!("{}", text);
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
