use crate::ast::visitor::{Visit, Visitor};
use crate::ast::{BinOpKind, Expr, ExprKind, LitKind, UnOpKind};
use crate::resolver::{Context, Def, Type, Var};
use fxhash::FxHashMap;
use std::iter;
use thiserror::Error;
use walrus::ir::{BinaryOp, UnaryOp};
use walrus::{FunctionBuilder, InstrSeqBuilder, LocalId, Module, ModuleConfig, ValType};

pub const WASM_EVAL_FUNC: &str = "eval";

#[derive(Debug, Error)]
pub enum WasmGenError {
    #[error("`{0}` cannot be used as a program argument type")]
    InvalidParamType(Type),
    #[error("`{0}` cannot be used as a program return type")]
    InvalidReturnType(Type),
}

pub fn generate_wasm(
    root_expr: &Expr,
    ctx: &Context,
    params: &[Var],
) -> Result<Vec<u8>, WasmGenError> {
    let mut module = Module::with_config(ModuleConfig::new());

    let args: Vec<LocalId> = params.iter()
        .map(|param| {
            let ty = map_type(param.ty()).ok_or_else(|| {
                WasmGenError::InvalidParamType(param.ty())
            })?;

            Ok(module.locals.add(ty))
        })
        .collect::<Result<_, _>>()?;

    let result = {
        let ty = ctx.get_ty(root_expr.id).unwrap();
        map_type(ty).ok_or(WasmGenError::InvalidReturnType(ty))?
    };

    let param_types: Vec<ValType> = args.iter()
        .map(|local| module.locals.get(*local).ty())
        .collect();

    let mut eval = FunctionBuilder::new(
        &mut module.types,
        &param_types,
        &[result],
    );

    let mut locals: FxHashMap<Var, LocalId> = iter::zip(
        params.iter().copied(),
        args.iter().copied(),
    ).collect();

    root_expr.visit(&mut Generator { ctx, locals: &mut locals, instrs: eval.func_body() });

    let eval = eval.finish(args, &mut module.funcs);

    module.exports.add(WASM_EVAL_FUNC, eval);
    Ok(module.emit_wasm())
}

struct Generator<'a> {
    ctx: &'a Context,
    locals: &'a mut FxHashMap<Var, LocalId>,
    instrs: InstrSeqBuilder<'a>,
}

impl Visitor for Generator<'_> {
    type Result = ();

    fn visit_expr(&mut self, expr: &Expr) -> Self::Result {
        let out_ty = self.ctx.get_ty(expr.id).unwrap();

        match &expr.kind {
            ExprKind::Binary(op, left, right) => {
                self.visit_expr(left);
                self.visit_expr(right);

                match (op.kind, out_ty) {
                    (BinOpKind::Add, Type::I32) => {
                        self.instrs.binop(BinaryOp::I32Add);
                    }
                    (BinOpKind::Add, Type::F32) => {
                        self.instrs.binop(BinaryOp::F32Add);
                    }
                    (BinOpKind::Sub, Type::I32) => {
                        self.instrs.binop(BinaryOp::I32Sub);
                    }
                    (BinOpKind::Sub, Type::F32) => {
                        self.instrs.binop(BinaryOp::F32Sub);
                    }
                    (BinOpKind::Mul, Type::I32) => {
                        self.instrs.binop(BinaryOp::I32Mul);
                    }
                    (BinOpKind::Mul, Type::F32) => {
                        self.instrs.binop(BinaryOp::F32Mul);
                    }
                    (BinOpKind::Div, Type::I32) => {
                        self.instrs.binop(BinaryOp::I32DivS);
                    }
                    (BinOpKind::Div, Type::F32) => {
                        self.instrs.binop(BinaryOp::F32Div);
                    }
                    (BinOpKind::Mod, Type::I32) => {
                        self.instrs.binop(BinaryOp::I32RemS);
                    }
                    (BinOpKind::Exp, Type::I32) => {
                        todo!("implement integer exponentiation");
                    }
                    (BinOpKind::Exp, Type::F32) => {
                        todo!("implement floating-point exponentiation");
                    }
                    _ => unreachable!("logic error"),
                }
            }
            ExprKind::Unary(op, term) => match (op.kind, out_ty) {
                (UnOpKind::Plus, _) => {
                    self.visit_expr(term);
                }
                (UnOpKind::Neg, Type::I32) => {
                    self.instrs.i32_const(0);
                    self.visit_expr(term);
                    self.instrs.binop(BinaryOp::I32Sub);
                }
                (UnOpKind::Neg, Type::F32) => {
                    self.visit_expr(term);
                    self.instrs.unop(UnaryOp::F32Neg);
                }
                _ => unreachable!("logic error"),
            },
            ExprKind::Paren(nested) => {
                nested.visit(self);
            }
            ExprKind::Ident(_) => {
                let def = self.ctx.get_def(expr.id).unwrap();

                match def {
                    Def::Var(var) => {
                        let local = self.locals[&var];
                        self.instrs.local_get(local);
                    }
                }
            }
            ExprKind::Lit(lit) => match lit.kind {
                LitKind::I32(val) => {
                    self.instrs.i32_const(val);
                }
                LitKind::F32(val) => {
                    self.instrs.f32_const(val);
                }
            },
        }
    }
}

fn map_type(ty: Type) -> Option<ValType> {
    match ty {
        Type::I32 => Some(ValType::I32),
        Type::F32 => Some(ValType::F32),
        Type::Unit => None,
    }
}
