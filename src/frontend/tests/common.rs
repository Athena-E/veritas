use crate::common::types::Spanned;
use crate::common::ast::Token;
use crate::frontend::lexer::lexer;
use chumsky::prelude::*;

pub fn parse_tokens(src: &str) -> Vec<Spanned<Token>> {
    lexer().parse(src).into_result().unwrap()
}
