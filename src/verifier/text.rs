use super::{VerifyError, verify_dtal};

/// Error returned when verifying DTAL text.
///
/// This type keeps syntax errors separate from verifier errors so tools can
/// report parser diagnostics without losing the semantic error structure.
#[derive(Debug)]
pub enum VerifyTextError {
    /// Parsing failed before verification.
    ParseErrors(Vec<crate::dtal::parser::DtalParseError>),
    /// Parsed DTAL failed verification.
    VerifyError(Box<VerifyError>),
}

impl std::fmt::Display for VerifyTextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerifyTextError::ParseErrors(errors) => {
                writeln!(f, "DTAL parse errors:")?;
                for e in errors {
                    writeln!(f, "  {}", e)?;
                }
                Ok(())
            }
            VerifyTextError::VerifyError(e) => write!(f, "Verification error: {}", e),
        }
    }
}

impl std::error::Error for VerifyTextError {}

/// Parse DTAL text and verify the resulting program.
///
/// # Errors
///
/// Returns [`VerifyTextError::ParseErrors`] when DTAL parsing fails, or
/// [`VerifyTextError::VerifyError`] when parsed DTAL violates verifier rules.
pub fn verify_dtal_text(input: &str) -> Result<(), VerifyTextError> {
    let program = crate::dtal::parser::parse_dtal(input).map_err(VerifyTextError::ParseErrors)?;
    verify_dtal(&program).map_err(|e| VerifyTextError::VerifyError(Box::new(e)))
}
