mod cli;
mod common;
mod frontend;

use cli::frontend::{Config, read_source_file, display_typed_program, run_pipeline};

fn main() {
    // Parse configuration
    let config = Config::from_args();

    println!("Parsing: {}", config.file_path);
    println!("{}", "=".repeat(60));

    // Read the source file
    let src = match read_source_file(&config.file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // Print the source code
    println!("\nSource code:");
    println!("{}", "-".repeat(60));
    println!("{}", src);
    println!("{}", "-".repeat(60));

    // Run the compilation pipeline
    let program = match run_pipeline(&src, config.verbose) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("\n{}", e);
            return;
        }
    };

    // Display results
    println!("\n[4] Program structure:");
    println!("{}", "=".repeat(60));

    display_typed_program(&program);

    // Show detailed AST if requested
    if config.show_ast {
        println!("\n[5] Detailed Typed AST:");
        println!("{}", "=".repeat(60));
        println!("{:#?}", program);
    }

    println!("\n{}", "=".repeat(60));
    println!("Successfully type-checked {} function(s)", program.functions.len());
}
