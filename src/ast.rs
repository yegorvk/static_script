use crate::session::with_session;
use crate::symbol::Symbol;
use crate::{session_scoped, Span};
use derive_more::Display;
use std::cell::Cell;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct NodeId(u32);

impl NodeId {
    fn new() -> NodeId {
        session_scoped! {
            static NEXT: Cell<u32> = Cell::new(0);
        }

        NodeId(with_session(&NEXT, |next| next.replace(next.get() + 1)))
    }
}

#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Expr {
        Expr {
            id: NodeId::new(),
            kind,
            span,
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Paren(Box<Expr>),
    Ident(Ident),
    Lit(Lit),
}

#[derive(Debug, Copy, Clone, Display)]
#[display("{name}")]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum LitKind {
    I32(i32),
    F32(f32),
}

#[derive(Debug, Copy, Clone, Display)]
#[display("{kind}")]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("%")]
    Mod,
    #[display("^")]
    Exp,
}

#[derive(Debug, Copy, Clone, Display)]
#[display("{kind}")]
pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum UnOpKind {
    #[display("+")]
    Plus,
    #[display("-")]
    Neg,
}

pub mod visitor {
    use crate::ast::Expr;

    pub trait Visitor {
        type Result;

        fn visit_expr(&mut self, expr: &Expr) -> Self::Result;
    }

    pub trait Visit {
        fn visit<V>(&self, visitor: &mut V) -> V::Result
        where
            V: Visitor;
    }

    impl Visit for Expr {
        fn visit<V>(&self, visitor: &mut V) -> V::Result
        where
            V: Visitor,
        {
            visitor.visit_expr(self)
        }
    }
}
