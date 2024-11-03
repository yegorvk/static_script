use crate::ast::visitor::{Visit, Visitor};
use crate::ast::{BinOpKind, Expr, ExprKind, LitKind, UnOpKind};
use crate::resolver::{AstContext, Type};
use crate::{PlainType, ProgramConfig};
use walrus::ir::{BinaryOp, UnaryOp};
use walrus::{FunctionBuilder, InstrSeqBuilder, LocalId, Module, ModuleConfig, ValType};

pub const WASM_EVAL_FUNC: &str = "eval";

pub fn generate_wasm(ast: &Expr, ctx: &AstContext, config: &ProgramConfig) -> Vec<u8> {
    let mut module = Module::with_config(ModuleConfig::new());

    let params: Vec<ValType> = config
        .params()
        .iter()
        .map(|var| match var.ty {
            PlainType::I32 => ValType::I32,
            PlainType::F32 => ValType::F32,
        })
        .collect();

    let result = match config.ret() {
        PlainType::I32 => ValType::I32,
        PlainType::F32 => ValType::F32,
    };

    let param_locals: Vec<LocalId> = params
        .iter()
        .map(|param| module.locals.add(*param))
        .collect();

    let mut eval = FunctionBuilder::new(&mut module.types, &params, &[result]);

    ast.visit(&mut Generator {
        ctx,
        instrs: eval.func_body(),
    });

    let eval = eval.finish(param_locals, &mut module.funcs);
    module.exports.add(WASM_EVAL_FUNC, eval);
    module.emit_wasm()
}

struct Generator<'ctx, 'func> {
    ctx: &'ctx AstContext,
    instrs: InstrSeqBuilder<'func>,
}

impl Visitor for Generator<'_, '_> {
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
                    _ => unreachable!(),
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
                _ => unreachable!(),
            },
            ExprKind::Paren(nested) => {
                nested.visit(self);
            }
            ExprKind::Ident(_ident) => {
                todo!("implement variables");
            }
            ExprKind::Lit(lit) => match lit.kind {
                LitKind::I32(val) => {
                    self.instrs.i32_const(val);
                }
                LitKind::F32(val) => {
                    self.instrs.f32_const(val);
                }
                LitKind::Unit => {}
            },
        }
    }
}
