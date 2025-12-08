use super::types::type_parser;
use crate::common::ast::{BinOp, Expr, Literal, Stmt, Token, UnaryOp};
use crate::common::span::{Span, Spanned};
use chumsky::{input::ValueInput, prelude::*};

// Expression parser for use in type contexts
pub fn expr_parser_for_types<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|expr| {
        // Literals
        let lit = select! {
            Token::Num(n) => Literal::Int(n),
            Token::True => Literal::Bool(true),
            Token::False => Literal::Bool(false),
        }
        .map(Expr::Literal)
        .labelled("literal");

        // Variables
        let var = select! { Token::Ident(name) => name }
            .map(Expr::Variable)
            .labelled("identifier");

        // Parenthesized expression
        let paren = expr
            .clone()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .map(|(e, _)| e);

        // Array initialization: [value; length]
        let array_init = expr
            .clone()
            .then_ignore(just(Token::Ctrl(';')))
            .then(expr.clone())
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .map(|(value, length)| Expr::ArrayInit {
                value: Box::new(value),
                length: Box::new(length),
            });

        // Function call
        let call = select! { Token::Ident(name) => name }
            .then(
                expr.clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .map_with(|args, e| (args, e.span()))
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map(|(func_name, args)| Expr::Call { func_name, args });

        let atom = call
            .or(array_init)
            .or(paren)
            .or(lit)
            .or(var)
            .map_with(|expr, e| (expr, e.span()))
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl('('),
                Token::Ctrl(')'),
                [
                    (Token::Ctrl('['), Token::Ctrl(']')),
                    (Token::Ctrl('{'), Token::Ctrl('}')),
                ],
                |span| (Expr::Error, span),
            )))
            .boxed();

        // Array indexing
        let indexed = atom.foldl_with(
            expr.clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .repeated(),
            |base, index, e| {
                (
                    Expr::Index {
                        base: Box::new(base),
                        index: Box::new(index),
                    },
                    e.span(),
                )
            },
        );

        // Unary operators
        let op_not = just(Token::Op("!")).to(UnaryOp::Not);
        let unary = op_not.repeated().foldr_with(indexed, |op, operand, e| {
            (
                Expr::UnaryOp {
                    op,
                    cond: Box::new(operand),
                },
                e.span(),
            )
        });

        // Binary operators with precedence
        // Multiplication
        let op_mul = just(Token::Op("*")).to(BinOp::Mul);
        let product =
            unary
                .clone()
                .foldl_with(op_mul.then(unary).repeated(), |lhs, (op, rhs), e| {
                    (
                        Expr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                });

        // Addition and subtraction
        let op_add = just(Token::Op("+")).to(BinOp::Add);
        let op_sub = just(Token::Op("-")).to(BinOp::Sub);
        let sum = product.clone().foldl_with(
            choice((op_add, op_sub)).then(product).repeated(),
            |lhs, (op, rhs), e| {
                (
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    e.span(),
                )
            },
        );

        // Comparisons
        let op_lt = just(Token::Op("<")).to(BinOp::Lt);
        let op_lte = just(Token::Op("<=")).to(BinOp::Lte);
        let op_gt = just(Token::Op(">")).to(BinOp::Gt);
        let op_gte = just(Token::Op(">=")).to(BinOp::Gte);
        let op_eq = just(Token::Op("==")).to(BinOp::Eq);
        let op_neq = just(Token::Op("!=")).to(BinOp::NotEq);
        let comparison = sum.clone().foldl_with(
            choice((op_lte, op_gte, op_eq, op_neq, op_lt, op_gt))
                .then(sum)
                .repeated(),
            |lhs, (op, rhs), e| {
                (
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    e.span(),
                )
            },
        );

        // Logical operators
        let op_and = just(Token::Op("&&")).to(BinOp::And);
        let op_or = just(Token::Op("||")).to(BinOp::Or);
        let logical = comparison.clone().foldl_with(
            choice((op_and, op_or)).then(comparison).repeated(),
            |lhs, (op, rhs), e| {
                (
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    e.span(),
                )
            },
        );

        logical.labelled("expression").as_context()
    })
    .boxed()
}

// Full expression parser
pub fn expr_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Spanned<Expr<'src>>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    recursive(|_expr| {
        let inline_expr = recursive(|inline_expr| {
            // Literals
            let lit = select! {
                Token::Num(n) => Literal::Int(n),
                Token::True => Literal::Bool(true),
                Token::False => Literal::Bool(false),
            }
            .map(Expr::Literal)
            .labelled("literal");

            // Variables
            let var = select! { Token::Ident(name) => name }
                .map(Expr::Variable)
                .labelled("identifier");

            // Parenthesized expression
            let paren = inline_expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map(|(e, _)| e);

            // Array initialization: [value; length]
            let array_init = inline_expr
                .clone()
                .then_ignore(just(Token::Ctrl(';')))
                .then(inline_expr.clone())
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map(|(value, length)| Expr::ArrayInit {
                    value: Box::new(value),
                    length: Box::new(length),
                });

            // Function call
            let call = select! { Token::Ident(name) => name }
                .then(
                    inline_expr
                        .clone()
                        .separated_by(just(Token::Ctrl(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .map_with(|args, e| (args, e.span()))
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|(func_name, args)| Expr::Call { func_name, args });

            let atom = call
                .or(array_init)
                .or(paren)
                .or(lit)
                .or(var)
                .map_with(|expr, e| (expr, e.span()))
                .recover_with(via_parser(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                )))
                .boxed();

            // Array indexing
            let indexed = atom.foldl_with(
                inline_expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                    .repeated(),
                |base, index, e| {
                    (
                        Expr::Index {
                            base: Box::new(base),
                            index: Box::new(index),
                        },
                        e.span(),
                    )
                },
            );

            // Unary operators
            let op_not = just(Token::Op("!")).to(UnaryOp::Not);
            let unary = op_not.repeated().foldr_with(indexed, |op, operand, e| {
                (
                    Expr::UnaryOp {
                        op,
                        cond: Box::new(operand),
                    },
                    e.span(),
                )
            });

            // Binary operators with precedence
            // Multiplication
            let op_mul = just(Token::Op("*")).to(BinOp::Mul);
            let product =
                unary
                    .clone()
                    .foldl_with(op_mul.then(unary).repeated(), |lhs, (op, rhs), e| {
                        (
                            Expr::BinOp {
                                op,
                                lhs: Box::new(lhs),
                                rhs: Box::new(rhs),
                            },
                            e.span(),
                        )
                    });

            // Addition and subtraction
            let op_add = just(Token::Op("+")).to(BinOp::Add);
            let op_sub = just(Token::Op("-")).to(BinOp::Sub);
            let sum = product.clone().foldl_with(
                choice((op_add, op_sub)).then(product).repeated(),
                |lhs, (op, rhs), e| {
                    (
                        Expr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                },
            );

            // Comparisons
            let op_lt = just(Token::Op("<")).to(BinOp::Lt);
            let op_lte = just(Token::Op("<=")).to(BinOp::Lte);
            let op_gt = just(Token::Op(">")).to(BinOp::Gt);
            let op_gte = just(Token::Op(">=")).to(BinOp::Gte);
            let op_eq = just(Token::Op("==")).to(BinOp::Eq);
            let op_neq = just(Token::Op("!=")).to(BinOp::NotEq);
            let comparison = sum.clone().foldl_with(
                choice((op_lte, op_gte, op_eq, op_neq, op_lt, op_gt))
                    .then(sum)
                    .repeated(),
                |lhs, (op, rhs), e| {
                    (
                        Expr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                },
            );

            // Logical operators
            let op_and = just(Token::Op("&&")).to(BinOp::And);
            let op_or = just(Token::Op("||")).to(BinOp::Or);
            let logical = comparison.clone().foldl_with(
                choice((op_and, op_or)).then(comparison).repeated(),
                |lhs, (op, rhs), e| {
                    (
                        Expr::BinOp {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        e.span(),
                    )
                },
            );

            logical.labelled("expression").as_context()
        });

        // Statements
        let ty = type_parser();

        let let_stmt = just(Token::Let)
            .ignore_then(just(Token::Mut).or_not())
            .then(select! { Token::Ident(name) => name })
            .then_ignore(just(Token::Ctrl(':')))
            .then(ty.clone())
            .then_ignore(just(Token::Op("=")))
            .then(inline_expr.clone())
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

        let assign_stmt = inline_expr
            .clone()
            .then_ignore(just(Token::Op("=")))
            .then(inline_expr.clone())
            .then_ignore(just(Token::Ctrl(';')))
            .map_with(|(lhs, rhs), e| (Stmt::Assignment { lhs, rhs }, e.span()));

        let stmt = choice((let_stmt, assign_stmt));

        // If expression
        let if_expr = just(Token::If)
            .ignore_then(inline_expr.clone())
            .then(
                stmt.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    .recover_with(via_parser(nested_delimiters(
                        Token::Ctrl('{'),
                        Token::Ctrl('}'),
                        [
                            (Token::Ctrl('('), Token::Ctrl(')')),
                            (Token::Ctrl('['), Token::Ctrl(']')),
                        ],
                        |span| {
                            vec![(
                                Stmt::Assignment {
                                    lhs: (Expr::Error, span),
                                    rhs: (Expr::Error, span),
                                },
                                span,
                            )]
                        },
                    ))),
            )
            .then(
                just(Token::Else)
                    .ignore_then(
                        stmt.repeated()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                            .recover_with(via_parser(nested_delimiters(
                                Token::Ctrl('{'),
                                Token::Ctrl('}'),
                                [
                                    (Token::Ctrl('('), Token::Ctrl(')')),
                                    (Token::Ctrl('['), Token::Ctrl(']')),
                                ],
                                |span| {
                                    vec![(
                                        Stmt::Assignment {
                                            lhs: (Expr::Error, span),
                                            rhs: (Expr::Error, span),
                                        },
                                        span,
                                    )]
                                },
                            ))),
                    )
                    .or_not(),
            )
            .map_with(|((cond, then_block), else_block), e| {
                (
                    Expr::If {
                        cond: Box::new(cond),
                        then_block,
                        else_block,
                    },
                    e.span(),
                )
            });

        if_expr.or(inline_expr).labelled("expression")
    })
    .boxed()
}
