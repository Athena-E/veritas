use chumsky::prelude::*;
use crate::common::span::{Span, Spanned};
use crate::common::ast::Token;

// Lexer
pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers
    let num = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Num);

    // A parser for operators
    let op = choice((
        just("=="),
        just("!="),
        just("<="),
        just(">="),
        just("&&"),
        just("||"),
        just("->"),
        just(".."),
        just("<"),
        just(">"),
        just("+"),
        just("-"),
        just("*"),
        just("="),
        just("&"),
        just("|"),
        just("!"),
    ))
    .map(Token::Op);

    // A parser for control characters
    let ctrl = one_of("(){}[];,:").map(Token::Ctrl);

    // A parser for identifiers and keywords
    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "for" => Token::For,
        "in" => Token::In,
        "int" => Token::Int,
        "bool" => Token::Bool,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(ident),
    });

    let token = num.or(op).or(ctrl).or(ident);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
