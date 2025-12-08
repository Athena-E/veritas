use chumsky::{input::ValueInput, prelude::*};
use crate::common::span::{Span, Spanned};
use crate::common::ast::{Token, Stmt, Expr};
use super::types::type_parser;

// Note: Expr is used for constructing if-statements

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

    // Use recursive to handle for loops with nested statements
    recursive(|stmt| {
        // Let statement
        let let_stmt = just(Token::Let)
            .ignore_then(just(Token::Mut).or_not())
            .then(select! { Token::Ident(name) => name })
            .then_ignore(just(Token::Ctrl(':')))
            .then(ty.clone())
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

        // Return statement
        let return_stmt = just(Token::Return)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Ctrl(';')))
            .map_with(|expr, e| {
                (
                    Stmt::Return {
                        expr: Box::new(expr),
                    },
                    e.span(),
                )
            });

        // For loop: for var in start..end [invariant expr] { body }
        let invariant_clause = just(Token::Invariant)
            .ignore_then(expr.clone())
            .or_not();

        let for_stmt = just(Token::For)
            .ignore_then(select! { Token::Ident(name) => name })
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .then_ignore(just(Token::Op("..")))
            .then(expr.clone())
            .then(invariant_clause)
            .then(
                stmt.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            )
            .map_with(|((((var, start), end), invariant), body), e| {
                (
                    Stmt::For {
                        var,
                        start: Box::new(start),
                        end: Box::new(end),
                        invariant,
                        body,
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

        // If statement (if expression used as statement, no semicolon needed)
        let if_stmt = just(Token::If)
            .ignore_then(expr.clone())
            .then(
                stmt.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            )
            .then(
                just(Token::Else)
                    .ignore_then(
                        stmt.clone()
                            .repeated()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    )
                    .or_not()
            )
            .map_with(|((cond, then_block), else_block), e| {
                (
                    Stmt::Expr((
                        Expr::If {
                            cond: Box::new(cond),
                            then_block,
                            else_block,
                        },
                        e.span(),
                    )),
                    e.span(),
                )
            });

        // Expression statement
        let expr_stmt = expr
            .clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map_with(|expr, e| (Stmt::Expr(expr), e.span()));

        choice((let_stmt, return_stmt, for_stmt, if_stmt, assign_stmt, expr_stmt))
            .labelled("statement")
    })
    .boxed()
}
