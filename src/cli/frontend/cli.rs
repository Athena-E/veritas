use std::{env, fs};

/// Configuration for the CLI application
pub struct Config {
    pub file_path: String,
    pub verbose: bool,
    pub show_ast: bool,
}

impl Config {
    /// Parse command line arguments and environment variables
    pub fn from_args() -> Self {
        let args: Vec<String> = env::args().collect();
        let file_path = if args.len() > 1 {
            args[1].clone()
        } else {
            "src/examples/01_simple.veri".to_string()
        };

        let verbose = env::var("VERBOSE").is_ok();
        let show_ast = env::var("AST").is_ok();

        Config {
            file_path,
            verbose,
            show_ast,
        }
    }
}

/// Read the source file from the given path
pub fn read_source_file(file_path: &str) -> Result<String, String> {
    fs::read_to_string(file_path).map_err(|e| {
        format!(
            "Error reading file '{}': {}\n\n\
            Usage: cargo run [example_file.veri]\n\n\
            Available examples:\n\
            \x20 - src/examples/01_simple.veri\n\
            \x20 - src/examples/02_conditionals.veri\n\
            \x20 - src/examples/03_arrays.veri\n\
            \x20 - src/examples/04_references.veri\n\
            \x20 - src/examples/05_comparisons.veri\n\
            \x20 - src/examples/06_logical.veri\n\
            \x20 - src/examples/07_function_calls.veri\n\
            \x20 - src/examples/08_mutable.veri\n\
            \x20 - src/examples/09_complex_expressions.veri\n\
            \x20 - src/examples/10_advanced_types.veri\n\
            \x20 - src/examples/11_unary_not.veri\n\
            \x20 - src/examples/12_function_parameters.veri",
            file_path, e
        )
    })
}
