use super::span::Spanned;
use std::fmt;

// Token definition
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Num(i64),
    Ident(&'src str),
    Op(&'src str),
    Ctrl(char),
    // Keywords
    Let,
    Mut,
    Fn,
    If,
    Else,
    Return,
    For,
    In,
    Requires,
    Ensures,
    Invariant,
    // Type keywords
    Int,
    Bool,
    True,
    False,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Num(n) => write!(f, "{n}"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Op(s) => write!(f, "{s}"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Fn => write!(f, "fn"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Requires => write!(f, "requires"),
            Token::Ensures => write!(f, "ensures"),
            Token::Invariant => write!(f, "invariant"),
            Token::Int => write!(f, "int"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

// Binary operators
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

// Unary operators
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Not,
}

// Literals
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

// Type expressions
#[derive(Clone, Debug)]
pub enum Type<'src> {
    Unit,
    Int,
    Bool,
    Array {
        element_type: Box<Spanned<Self>>,
        size: Box<Spanned<Expr<'src>>>,
    },
    Ref(Box<Spanned<Self>>),
    RefMut(Box<Spanned<Self>>),
    SingletonInt(Box<Spanned<Expr<'src>>>),
    RefinedInt {
        var: &'src str,
        predicate: Box<Spanned<Expr<'src>>>,
    },
}

// Expression nodes
#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Error,
    Literal(Literal),
    Variable(&'src str),
    BinOp {
        op: BinOp,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
    UnaryOp {
        op: UnaryOp,
        cond: Box<Spanned<Self>>,
    },
    Call {
        func_name: &'src str,
        args: Spanned<Vec<Spanned<Self>>>,
    },
    Index {
        base: Box<Spanned<Self>>,
        index: Box<Spanned<Self>>,
    },
    ArrayInit {
        value: Box<Spanned<Self>>,
        length: Box<Spanned<Self>>,
    },
    If {
        cond: Box<Spanned<Self>>,
        then_block: Vec<Spanned<Stmt<'src>>>,
        else_block: Option<Vec<Spanned<Stmt<'src>>>>,
    },
}

// Statement nodes
#[derive(Clone, Debug)]
pub enum Stmt<'src> {
    Let {
        is_mut: bool,
        name: &'src str,
        ty: Spanned<Type<'src>>,
        value: Spanned<Expr<'src>>,
    },
    Assignment {
        lhs: Spanned<Expr<'src>>,
        rhs: Spanned<Expr<'src>>,
    },
    Return {
        expr: Box<Spanned<Expr<'src>>>,
    },
    Expr(Spanned<Expr<'src>>),
    For {
        var: &'src str,
        start: Box<Spanned<Expr<'src>>>,
        end: Box<Spanned<Expr<'src>>>,
        invariant: Option<Spanned<Expr<'src>>>,
        body: Vec<Spanned<Stmt<'src>>>,
    },
}

// Function parameter
#[derive(Debug)]
pub struct Parameter<'src> {
    pub name: &'src str,
    pub ty: Spanned<Type<'src>>,
}

// Function body
#[derive(Debug)]
pub struct FunctionBody<'src> {
    pub statements: Vec<Spanned<Stmt<'src>>>,
    pub return_expr: Option<Box<Spanned<Expr<'src>>>>,
}

// Function definition
#[derive(Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub parameters: Vec<Spanned<Parameter<'src>>>,
    pub return_type: Spanned<Type<'src>>,
    pub precondition: Option<Spanned<Expr<'src>>>,
    pub postcondition: Option<Spanned<Expr<'src>>>,
    pub body: FunctionBody<'src>,
}

// Program
#[derive(Debug)]
pub struct Program<'src> {
    pub functions: Vec<Spanned<Function<'src>>>,
}
