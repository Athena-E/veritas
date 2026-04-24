use super::span::Spanned;
use std::fmt;

// Token definition
#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Num(i128),
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
    While,
    In,
    Requires,
    Ensures,
    Invariant,
    Forall,
    Exists,
    Const,
    Region,
    // Type keywords
    Int,
    I64,
    U64,
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
            Token::While => write!(f, "while"),
            Token::In => write!(f, "in"),
            Token::Requires => write!(f, "requires"),
            Token::Ensures => write!(f, "ensures"),
            Token::Invariant => write!(f, "invariant"),
            Token::Forall => write!(f, "forall"),
            Token::Exists => write!(f, "exists"),
            Token::Const => write!(f, "const"),
            Token::Region => write!(f, "region"),
            Token::Int => write!(f, "int"),
            Token::I64 => write!(f, "i64"),
            Token::U64 => write!(f, "u64"),
            Token::Bool => write!(f, "bool"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::BitAnd => write!(f, "&"),
            BinOp::BitOr => write!(f, "|"),
            BinOp::BitXor => write!(f, "^"),
            BinOp::Shl => write!(f, "<<"),
            BinOp::Shr => write!(f, ">>"),
            BinOp::Eq => write!(f, "=="),
            BinOp::NotEq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Gte => write!(f, ">="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
            BinOp::Implies => write!(f, "==>"),
        }
    }
}

// Binary operators
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Implies,
}

// Unary operators
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

// Literals
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i128),
    Bool(bool),
}

// Type expressions
#[derive(Clone, Debug)]
pub enum Type<'src> {
    Unit,
    Int,
    I64,
    U64,
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
    RefinedI64 {
        var: &'src str,
        predicate: Box<Spanned<Expr<'src>>>,
    },
    RefinedU64 {
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
        then_block: Block<'src>,
        else_block: Option<Block<'src>>,
    },
    Forall {
        var: &'src str,
        start: Box<Spanned<Self>>,
        end: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    },
    Exists {
        var: &'src str,
        start: Box<Spanned<Self>>,
        end: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    },
}

// A block: statements followed by an optional trailing expression (the block's value)
// { stmts*; trailing_expr? }
// `{ 1 }` has trailing_expr = Some(1), `{ 1; }` has trailing_expr = None
#[derive(Clone, Debug)]
pub struct Block<'src> {
    pub statements: Vec<Spanned<Stmt<'src>>>,
    pub trailing_expr: Option<Box<Spanned<Expr<'src>>>>,
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
        body: Block<'src>,
    },
    While {
        condition: Box<Spanned<Expr<'src>>>,
        invariant: Option<Spanned<Expr<'src>>>,
        body: Block<'src>,
    },
    Region {
        body: Block<'src>,
    },
}

// Function parameter
#[derive(Debug)]
pub struct Parameter<'src> {
    pub name: &'src str,
    pub ty: Spanned<Type<'src>>,
}

// Function body is a Block
pub type FunctionBody<'src> = Block<'src>;

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

// Constant declaration
#[derive(Debug)]
pub struct Constant<'src> {
    pub name: &'src str,
    pub ty: Spanned<Type<'src>>,
    pub value: Spanned<Expr<'src>>,
}

// Program
#[derive(Debug)]
pub struct Program<'src> {
    pub constants: Vec<Spanned<Constant<'src>>>,
    pub functions: Vec<Spanned<Function<'src>>>,
}
