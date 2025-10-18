use chumsky::{input::ValueInput, prelude::*};
use crate::common::types::{Span, Spanned};
use crate::common::ast::{Token, Stmt, Expr};
use super::types::type_parser;

// Statement parser
pub fn stmt_parser<'tokens, 'src: 'tokens, I>(
    expr: impl Parser<'tokens, I, Spanned<Expr<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
        + Clone
        + 'tokens,
) -> impl Parser<'tokens, I, Spanned<Stmt<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let ty = type_parser();

    // Let statement
    let let_stmt = just(Token::Let)
        .ignore_then(just(Token::Mut).or_not())
        .then(select! { Token::Ident(name) => name })
        .then_ignore(just(Token::Ctrl(':')))
        .then(ty)
        .then_ignore(just(Token::Op("=")))
        .then(expr.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .map_with(|(((is_mut, name), ty), value), e| {
            (
                Stmt::Let {
                    is_mut: is_mut.is_some(),
                    name,
                    ty,
                    value,
                },
                e.span(),
            )
        });

    // Assignment statement
    let assign_stmt = expr
        .clone()
        .then_ignore(just(Token::Op("=")))
        .then(expr.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .map_with(|(lhs, rhs), e| (Stmt::Assignment { lhs, rhs }, e.span()));

    choice((let_stmt, assign_stmt))
        .labelled("statement")
        .boxed()
}
