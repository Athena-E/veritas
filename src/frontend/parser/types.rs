use chumsky::{input::ValueInput, prelude::*};
use crate::common::span::{Span, Spanned};
use crate::common::ast::{Token, Type};
use super::expr::expr_parser_for_types;

// Type parser
pub fn type_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Type<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
       + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|ty| {
        let expr = expr_parser_for_types();

        let basic_type = select! {
            Token::Int => Type::Int,
            Token::Bool => Type::Bool,
        }
        .map_with(|t, e| (t, e.span()));

        // Array type with size expressions
        let array_type = ty
            .clone()
            .then_ignore(just(Token::Ctrl(';')))
            .then(expr.clone())
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .map_with(|(element_type, size), e| {
                (
                    Type::Array {
                        element_type: Box::new(element_type),
                        size: Box::new(size),
                    },
                    e.span(),
                )
            });

        // Singleton int type
        let singleton_type = just(Token::Int)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map_with(|expr, e| (Type::SingletonInt(Box::new(expr)), e.span()));

        // Refined int type
        let refined_type = select! { Token::Ident(name) => name }
            .then_ignore(just(Token::Ctrl(':')))
            .then_ignore(just(Token::Int))
            .then_ignore(just(Token::Op("|")))
            .then(expr.clone())
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .map_with(|(var, predicate), e| {
                (
                    Type::RefinedInt {
                        var,
                        predicate: Box::new(predicate),
                    },
                    e.span(),
                )
            });

        // Reference types
        let ref_type = just(Token::Op("&"))
            .ignore_then(
                just(Token::Mut)
                    .ignore_then(ty.clone())
                    .map(|t| Type::RefMut(Box::new(t)))
                    .or(ty.clone().map(|t| Type::Ref(Box::new(t)))),
            )
            .map_with(|t, e| (t, e.span()));

        choice((array_type, singleton_type, refined_type, ref_type, basic_type))
            .labelled("type")
    })
    .boxed()
}
