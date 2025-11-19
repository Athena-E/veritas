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
            Usage: cargo run [example_file.veri]",
            file_path, e
        )
    })
}
