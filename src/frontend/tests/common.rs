use crate::common::span::Spanned;
use crate::common::ast::Token;
use crate::frontend::lexer::lexer;
use chumsky::prelude::*;

pub fn parse_tokens(src: &'_ str) -> Vec<Spanned<Token<'_>>> {
    lexer().parse(src).into_result().unwrap()
}
