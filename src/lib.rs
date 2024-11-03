#![allow(private_interfaces)]

mod ast;
mod generator;
mod resolver;
mod session;
mod symbol;

use crate::generator::generate_wasm;
use crate::parser::ExprParser;
use crate::resolver::{ResolveError, ResolverBuilder, Type};
use crate::symbol::Symbol;
pub use generator::WASM_EVAL_FUNC;
use lalrpop_util::lalrpop_mod;
use std::error::Error;
use std::fmt::{Display, Formatter};

lalrpop_mod!(parser);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PlainType {
    I32,
    F32,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: PlainType,
}

#[derive(Debug)]
struct ProgramConfig {
    params: Vec<Param>,
    ret: PlainType,
}

impl ProgramConfig {
    pub fn new(params: impl IntoIterator<Item = Param>, ret: PlainType) -> ProgramConfig {
        ProgramConfig {
            params: params.into_iter().collect(),
            ret,
        }
    }

    pub fn params(&self) -> &[Param] {
        &self.params
    }

    pub fn ret(&self) -> PlainType {
        self.ret
    }
}

pub enum CompileTarget {
    Wasm,
}

#[derive(Debug)]
pub struct CompileError(String);

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Error for CompileError {}

impl From<ResolveError> for CompileError {
    fn from(value: ResolveError) -> Self {
        CompileError(value.to_string())
    }
}

pub fn compile_program(
    src: &str,
    params: impl IntoIterator<Item = Param>,
    target: CompileTarget,
) -> Result<Vec<u8>, CompileError> {
    let params: Vec<Param> = params.into_iter().collect();

    let root_expr = ExprParser::new()
        .parse(src)
        .map_err(|e| CompileError(e.to_string()))?;

    let ctx = ResolverBuilder::new()
        .with_vars(params.iter().map(|p| (Symbol::from_str(&p.name), p.ty.into())))
        .resolve(&root_expr)?;

    let ret_ty = ctx.get_ty(root_expr.id).unwrap();

    let plain_ret_ty = match ret_ty {
        Type::I32 => PlainType::I32,
        Type::F32 => PlainType::F32,
        _ => {
            let err = format!("Error: `{ret_ty}` is not a valid program return type.");
            return Err(CompileError(err));
        }
    };

    let config = ProgramConfig::new(params, plain_ret_ty);

    let result = match target {
        CompileTarget::Wasm => generate_wasm(&root_expr, &ctx, &config),
    };

    Ok(result)
}

#[cfg(feature = "runtime")]
pub mod runner {
    use crate::{compile_program, CompileError, CompileTarget, WASM_EVAL_FUNC};
    use std::iter;
    use thiserror::Error;
    use wasmer::{imports, Instance, Module, Store};
    use wasmer_compiler_singlepass::Singlepass;

    #[derive(Debug, Error)]
    enum WasmerError {
        #[error(transparent)]
        CompileError(#[from] wasmer::CompileError),
        #[error(transparent)]
        InstantiationError(#[from] wasmer::InstantiationError),
        #[error(transparent)]
        ExportError(#[from] wasmer::ExportError),
        #[error(transparent)]
        RuntimeError(#[from] wasmer::RuntimeError),
    }

    #[derive(Debug, Error)]
    pub enum RunProgramError {
        #[error(transparent)]
        CompileError(#[from] CompileError),
        #[error(transparent)]
        RunError(#[from] WasmerError),
    }

    pub fn run_program(src: &str) -> Result<wasmer::Value, RunProgramError> {
        let wasm = compile_program(src, iter::empty(), CompileTarget::Wasm)?;
        Ok(run_compiled_program(&wasm)?)
    }

    fn run_compiled_program(wasm: &[u8]) -> Result<wasmer::Value, WasmerError> {
        let mut store = Store::new(Singlepass::default());
        let module = Module::new(&store, wasm)?;
        let instance = Instance::new(&mut store, &module, &imports! {})?;

        let eval = instance.exports.get_function(WASM_EVAL_FUNC)?;
        let result = eval.call(&mut store, &[])?;

        Ok(IntoIterator::into_iter(result).next().unwrap())
    }
}
