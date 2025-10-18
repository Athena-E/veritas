use chumsky::{input::ValueInput, prelude::*};
use crate::common::types::{Span, Spanned};
use crate::common::ast::{Token, Function, Program, Parameter};
use super::expr::expr_parser;
use super::stmt::stmt_parser;
use super::types::type_parser;

// Function parser
pub fn function_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Function<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let expr = expr_parser();
    let stmt = stmt_parser(expr);
    let ty = type_parser();

    // Parse a single parameter: name: type
    let parameter = select! { Token::Ident(name) => name }
        .then_ignore(just(Token::Ctrl(':')))
        .then(ty)
        .map_with(|(name, ty), e| {
            (
                Parameter { name, ty },
                e.span(),
            )
        });

    // Parse parameter list: (param1, param2, ...)
    let parameters = parameter
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    just(Token::Fn)
        .ignore_then(select! { Token::Ident(name) => name })
        .then(parameters)
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map_with(|((name, parameters), statements), e| {
            (
                Function {
                    name,
                    parameters,
                    statements,
                },
                e.span(),
            )
        })
        .labelled("function")
        .boxed()
}

// Program parser
pub fn program_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Program<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    function_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|functions| Program { functions })
        .then_ignore(end())
        .boxed()
}
