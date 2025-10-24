use chumsky::prelude::*;

// Type definitions
pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);
