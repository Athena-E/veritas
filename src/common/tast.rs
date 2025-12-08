use crate::common::ast::{BinOp, Literal, UnaryOp};
use crate::common::span::{Span, Spanned};
use crate::common::types::IType;

#[derive(Clone, Debug)]
pub enum TExpr<'src> {
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
        then_block: Vec<Spanned<TStmt<'src>>>,
        else_block: Option<Vec<Spanned<TStmt<'src>>>>,
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
    },

    Assignment {
        lhs: Spanned<TExpr<'src>>,
        rhs: Spanned<TExpr<'src>>,
    },

    Return {
        expr: Box<Spanned<TExpr<'src>>>,
    },

    Expr(Spanned<TExpr<'src>>),

    For {
        var: String,
        var_ty: IType<'src>,
        start: Box<Spanned<TExpr<'src>>>,
        end: Box<Spanned<TExpr<'src>>>,
        invariant: Option<Spanned<TExpr<'src>>>,
        body: Vec<Spanned<TStmt<'src>>>,
    },
}

#[derive(Clone, Debug)]
pub struct TParameter<'src> {
    pub name: String,
    pub ty: IType<'src>,
}

#[derive(Clone, Debug)]
pub struct TFunctionBody<'src> {
    pub statements: Vec<Spanned<TStmt<'src>>>,
    pub return_expr: Option<Box<Spanned<TExpr<'src>>>>,
}

#[derive(Clone, Debug)]
pub struct TFunction<'src> {
    pub name: String,
    pub parameters: Vec<TParameter<'src>>,
    pub return_type: IType<'src>,
    pub body: TFunctionBody<'src>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TProgram<'src> {
    pub functions: Vec<TFunction<'src>>,
}
