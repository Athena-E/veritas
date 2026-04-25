use crate::common::ast::{BinOp, Literal, UnaryOp};
use crate::common::ownership::OwnershipMode;
use crate::common::span::{Span, Spanned};
use crate::common::types::{IProposition, IType};

/// A typed block: statements followed by an optional trailing expression (the block's value)
#[derive(Clone, Debug)]
pub struct TBlock<'src> {
    pub statements: Vec<Spanned<TStmt<'src>>>,
    pub trailing_expr: Option<Box<Spanned<TExpr<'src>>>>,
}

/// Typed expression AST - output of type checking
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum TExpr<'src> {
    /// Error recovery node (reserved for future error recovery)
    Error {
        ty: IType<'src>,
    },

    Literal {
        value: Literal,
        ty: IType<'src>,
    },

    Variable {
        name: String,
        ty: IType<'src>,
    },

    BinOp {
        op: BinOp,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
        ty: IType<'src>,
    },

    UnaryOp {
        op: UnaryOp,
        operand: Box<Spanned<Self>>,
        ty: IType<'src>,
    },

    Call {
        func_name: String,
        args: Vec<Spanned<Self>>,
        arg_ownerships: Vec<OwnershipMode>,
        ownership: OwnershipMode,
        ty: IType<'src>,
    },

    Index {
        base: Box<Spanned<Self>>,
        index: Box<Spanned<Self>>,
        ty: IType<'src>,
    },

    ArrayInit {
        value: Box<Spanned<Self>>,
        length: Box<Spanned<Self>>,
        ty: IType<'src>,
    },

    If {
        cond: Box<Spanned<Self>>,
        then_block: TBlock<'src>,
        else_block: Option<TBlock<'src>>,
        ty: IType<'src>,
    },
}

impl<'src> TExpr<'src> {
    pub fn get_type(&self) -> &IType<'src> {
        match self {
            TExpr::Error { ty } => ty,
            TExpr::Literal { ty, .. } => ty,
            TExpr::Variable { ty, .. } => ty,
            TExpr::BinOp { ty, .. } => ty,
            TExpr::UnaryOp { ty, .. } => ty,
            TExpr::Call { ty, .. } => ty,
            TExpr::Index { ty, .. } => ty,
            TExpr::ArrayInit { ty, .. } => ty,
            TExpr::If { ty, .. } => ty,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TStmt<'src> {
    Let {
        is_mut: bool,
        name: String,
        declared_ty: IType<'src>,
        value: Spanned<TExpr<'src>>,
        checked_ty: IType<'src>,
        ownership: OwnershipMode,
    },

    Assignment {
        lhs: Spanned<TExpr<'src>>,
        rhs: Spanned<TExpr<'src>>,
        ownership: OwnershipMode,
    },

    Return {
        expr: Box<Spanned<TExpr<'src>>>,
        ownership: OwnershipMode,
    },

    Expr(Spanned<TExpr<'src>>),

    For {
        var: String,
        var_ty: IType<'src>,
        start: Box<Spanned<TExpr<'src>>>,
        end: Box<Spanned<TExpr<'src>>>,
        invariant: Option<crate::backend::dtal::constraints::Constraint>,
        body: TBlock<'src>,
    },

    While {
        condition: Box<Spanned<TExpr<'src>>>,
        invariant: Option<crate::backend::dtal::constraints::Constraint>,
        body: TBlock<'src>,
    },

    Region {
        body: TBlock<'src>,
    },
}

#[derive(Clone, Debug)]
pub struct TParameter<'src> {
    pub name: String,
    pub ty: IType<'src>,
}

// Typed function body is a TBlock
pub type TFunctionBody<'src> = TBlock<'src>;

/// Typed function - output of type checking a function
#[derive(Clone, Debug)]
pub struct TFunction<'src> {
    pub name: String,
    pub parameters: Vec<TParameter<'src>>,
    pub return_type: IType<'src>,
    pub returns_owned: bool,
    pub precondition: Option<IProposition<'src>>,
    pub postcondition: Option<IProposition<'src>>,
    pub body: TFunctionBody<'src>,
    #[allow(dead_code)]
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TProgram<'src> {
    pub functions: Vec<TFunction<'src>>,
}
