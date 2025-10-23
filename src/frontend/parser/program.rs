use chumsky::{input::ValueInput, prelude::*};
use crate::common::types::{Span, Spanned};
use crate::common::ast::{Token, Function, Program, Parameter, Type};
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
        .then(ty.clone())
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

    // Parse optional return type: -> Type
    let return_type = just(Token::Op("->"))
        .ignore_then(ty.clone())
        .or_not();

    just(Token::Fn)
        .ignore_then(select! { Token::Ident(name) => name })
        .then(parameters)
        .then(return_type)
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map_with(|(((name, parameters), return_type), statements), e| {
            // Default to Unit type if no return type is specified
            let return_type = return_type.unwrap_or_else(|| {
                let span = e.span();
                (Type::Unit, span)
            });
            (
                Function {
                    name,
                    parameters,
                    return_type,
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
