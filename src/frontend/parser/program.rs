use chumsky::{input::ValueInput, prelude::*};
use crate::common::span::{Span, Spanned};
use crate::common::ast::{Token, Function, Program, Parameter, Type, FunctionBody};
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
    let stmt = stmt_parser(expr.clone());
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

    // Parse optional precondition: requires expr
    let precondition = just(Token::Requires)
        .ignore_then(expr.clone())
        .or_not();

    // Parse function body: { stmts* expr? }
    // We need to parse statements, then check if there's a final expression without semicolon
    let body = just(Token::Ctrl('{'))
        .ignore_then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .then(expr.or_not())
        )
        .then_ignore(just(Token::Ctrl('}')))
        .map(|(statements, return_expr)| FunctionBody {
            statements,
            return_expr: return_expr.map(Box::new),
        });

    just(Token::Fn)
        .ignore_then(select! { Token::Ident(name) => name })
        .then(parameters)
        .then(return_type)
        .then(precondition)
        .then(body)
        .map_with(|((((name, parameters), return_type), precondition), body), e| {
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
                    precondition,
                    body,
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
