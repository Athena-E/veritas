use super::{VerifyError, verify_dtal};

#[derive(Debug)]
pub enum VerifyTextError {
    ParseErrors(Vec<crate::dtal::parser::DtalParseError>),
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

pub fn verify_dtal_text(input: &str) -> Result<(), VerifyTextError> {
    let program = crate::dtal::parser::parse_dtal(input).map_err(VerifyTextError::ParseErrors)?;
    verify_dtal(&program).map_err(|e| VerifyTextError::VerifyError(Box::new(e)))
}
