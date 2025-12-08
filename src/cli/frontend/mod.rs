pub mod cli;
pub mod display;
pub mod pipeline;

pub use cli::{Config, read_source_file};
pub use display::display_typed_program;
pub use pipeline::{run_pipeline, PipelineError};
