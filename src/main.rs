mod cli;
mod common;
mod frontend;

use cli::frontend::{Config, read_source_file, display_program, run_pipeline};

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
    println!("\n[3] Program structure:");
    println!("{}", "=".repeat(60));

    display_program(&program);

    // Show detailed AST if requested
    if config.show_ast {
        println!("\n[4] Detailed AST:");
        println!("{}", "=".repeat(60));
        println!("{:#?}", program);
    }

    println!("\n{}", "=".repeat(60));
    println!("Successfully parsed {} function(s)", program.functions.len());
}
