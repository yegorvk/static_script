use crate::ast::visitor::{Visit, Visitor};
use crate::ast::{BinOpKind, Expr, ExprKind, LitKind, NodeId, UnOpKind};
use crate::session::with_session;
use crate::symbol::Symbol;
use crate::{session_scoped, PlainType};
use derive_more::Display;
use fxhash::FxHashMap;
use rpds::HashTrieMap;
use std::cell::Cell;
use std::error::Error;
use std::fmt::Formatter;
use std::num::NonZeroU32;

#[derive(Debug, Clone)]
pub struct ResolveError(String);

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Error for ResolveError {}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum Type {
    #[display("i32")]
    I32,
    #[display("f32")]
    F32,
    #[display("()")]
    Unit,
}

impl From<PlainType> for Type {
    fn from(value: PlainType) -> Self {
        match value {
            PlainType::I32 => Type::I32,
            PlainType::F32 => Type::F32,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Var {
    id: NonZeroU32,
    ty: Type,
}

impl Var {
    fn new(ty: Type) -> Var {
        session_scoped! {
            static NEXT: Cell<u32> = Cell::new(1);
        }

        let id = with_session(&NEXT, |next| next.replace(next.get() + 1));
        let id = NonZeroU32::new(id).unwrap();

        Var { id, ty }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Def {
    Var(Var),
}

#[derive(Debug, Default)]
pub struct AstContext {
    ty: FxHashMap<NodeId, Type>,
    def: FxHashMap<NodeId, Def>,
}

impl AstContext {
    fn new() -> AstContext {
        Default::default()
    }

    pub fn get_ty(&self, node_id: NodeId) -> Option<Type> {
        self.ty.get(&node_id).copied()
    }

    pub fn get_def(&self, node_id: NodeId) -> Option<Def> {
        self.def.get(&node_id).copied()
    }
}

#[derive(Debug, Clone, Default)]
struct Scope {
    map: HashTrieMap<Symbol, Def>,
}

impl Scope {
    fn new() -> Scope {
        Default::default()
    }

    fn nested(parent: &Scope) -> Scope {
        parent.clone()
    }

    fn get(&self, ident: Symbol) -> Option<Def> {
        self.map.get(&ident).copied()
    }

    fn contains(&self, ident: Symbol) -> bool {
        self.map.contains_key(&ident)
    }

    fn insert(&mut self, ident: Symbol, def: Def) {
        self.map.insert_mut(ident, def);
    }
}

#[derive(Debug, Default)]
pub struct ResolverBuilder {
    vars: FxHashMap<Symbol, Type>,
}

impl ResolverBuilder {
    pub fn new() -> ResolverBuilder {
        Default::default()
    }

    pub fn with_var(&mut self, name: Symbol, ty: Type) -> &mut ResolverBuilder {
        self.vars.insert(name, ty);
        self
    }

    pub fn with_vars<I>(&mut self, iter: I) -> &mut ResolverBuilder
    where
        I: IntoIterator<Item = (Symbol, Type)>,
    {
        for (name, ty) in iter {
            self.with_var(name, ty);
        }

        self
    }

    pub fn resolve(&self, ast: &Expr) -> Result<AstContext, ResolveError> {
        let mut resolver = Resolver::new();

        for (name, ty) in &self.vars {
            resolver
                .current_scope_mut()
                .insert(*name, Def::Var(Var::new(*ty)));
        }

        resolver.resolve(ast)
    }
}

#[derive(Debug)]
struct Resolver {
    ctx: AstContext,
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            ctx: AstContext::new(),
            scopes: vec![Scope::new()],
        }
    }

    pub fn resolve(mut self, expr: &Expr) -> Result<AstContext, ResolveError> {
        expr.visit(&mut self)?;
        Ok(self.ctx)
    }

    fn current_scope(&self) -> &Scope {
        &self.scopes[self.scopes.len() - 1]
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver::new()
    }
}

impl Visitor for Resolver {
    type Result = Result<(), ResolveError>;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        match &expr.kind {
            ExprKind::Binary(op, left, right) => {
                self.visit_expr(left)?;
                self.visit_expr(right)?;

                let left_ty = self.ctx.ty[&left.id];
                let right_ty = self.ctx.ty[&right.id];

                if left_ty != right_ty || !has_binop_defined(op.kind, left_ty) {
                    let diagnostic = format!(
                        "Error: cannot apply binary operator `{}` to types `{}` and `{}`.",
                        op, left_ty, right_ty,
                    );

                    return Err(ResolveError(diagnostic));
                }

                self.ctx.ty.insert(expr.id, left_ty);
            }
            ExprKind::Unary(op, term) => {
                self.visit_expr(term)?;
                let ty = self.ctx.ty[&term.id];

                if !has_unop_defined(op.kind, ty) {
                    let diagnostic = format!(
                        "Error: cannot apply unary operator `{}` to type `{}`.",
                        op, ty,
                    );

                    return Err(ResolveError(diagnostic));
                }

                self.ctx.ty.insert(expr.id, ty);
            }
            ExprKind::Paren(nested) => {
                self.visit_expr(nested)?;
                self.ctx.ty.insert(expr.id, self.ctx.ty[&nested.id]);
            }
            ExprKind::Ident(ident) => {
                let def = self.current_scope().get(ident.name).ok_or_else(|| {
                    let diagnostic = format!("Error: `{ident}` is not defined.");
                    ResolveError(diagnostic)
                })?;

                match def {
                    Def::Var(var) => {
                        self.ctx.ty.insert(expr.id, var.ty);
                        self.ctx.def.insert(expr.id, Def::Var(var));
                    }
                }
            }
            ExprKind::Lit(lit) => {
                let ty = match lit.kind {
                    LitKind::I32(_) => Type::I32,
                    LitKind::F32(_) => Type::F32,
                    LitKind::Unit => Type::Unit,
                };

                self.ctx.ty.insert(expr.id, ty);
            }
        };

        Ok(())
    }
}

const fn has_binop_defined(op: BinOpKind, ty: Type) -> bool {
    match op {
        BinOpKind::Add => matches!(ty, Type::I32 | Type::F32),
        BinOpKind::Sub => matches!(ty, Type::I32 | Type::F32),
        BinOpKind::Mul => matches!(ty, Type::I32 | Type::F32),
        BinOpKind::Div => matches!(ty, Type::I32 | Type::F32),
        BinOpKind::Mod => matches!(ty, Type::I32),
        BinOpKind::Exp => matches!(ty, Type::I32 | Type::F32),
    }
}

const fn has_unop_defined(op: UnOpKind, ty: Type) -> bool {
    match op {
        UnOpKind::Plus => matches!(ty, Type::I32 | Type::F32),
        UnOpKind::Neg => matches!(ty, Type::I32 | Type::F32),
    }
}
