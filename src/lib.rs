#![allow(private_interfaces)]

mod ast;
mod generator;
mod resolver;
mod session;
mod symbol;

use crate::generator::generate_wasm;
use crate::parser::ExprParser;
use crate::resolver::{Def, ResolveError, ResolverBuilder, Var};
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

impl Param {
    pub fn new(name: impl Into<String>, ty: PlainType) -> Param {
        Param { name: name.into(), ty }
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
    params: &[Param],
    target: CompileTarget,
) -> Result<Vec<u8>, CompileError> {
    session::reset_session();

    let root_expr = ExprParser::new()
        .parse(src)
        .map_err(|e| CompileError(e.to_string()))?;

    let ctx = ResolverBuilder::new()
        .with_vars(
            params
                .iter()
                .map(|p| (Symbol::from_str(&p.name), p.ty.into())),
        )
        .resolve(&root_expr)?;

    let params: Vec<Var> = params.iter()
        .map(|param| ctx.toplevel_scope().get(Symbol::from_str(&param.name)).unwrap())
        .map(|def| match def {
            Def::Var(var) => var
        })
        .collect();

    let result = match target {
        CompileTarget::Wasm => {
            generate_wasm(&root_expr, &ctx, &params).map_err(|e| {
                CompileError(e.to_string())
            })?
        }
    };

    Ok(result)
}

#[cfg(feature = "runtime")]
pub mod runner {
    use crate::{compile_program, CompileError, CompileTarget, Param, WASM_EVAL_FUNC};
    use thiserror::Error;
    use wasmer::{imports, Instance, Module, Store};
    use wasmer_compiler_singlepass::Singlepass;

    #[derive(Debug, Error)]
    enum WasmerError {
        #[error(transparent)]
        Compile(#[from] wasmer::CompileError),
        #[error(transparent)]
        Instantiation(#[from] Box<wasmer::InstantiationError>),
        #[error(transparent)]
        Export(#[from] wasmer::ExportError),
        #[error(transparent)]
        Runtime(#[from] wasmer::RuntimeError),
    }

    #[derive(Debug, Error)]
    pub enum RunProgramError {
        #[error(transparent)]
        BuildModuleError(#[from] CompileError),
        #[error(transparent)]
        RunError(#[from] WasmerError),
    }

    pub fn run_program(
        src: &str,
        params: &[Param],
        args: &[wasmer::Value],
    ) -> Result<wasmer::Value, RunProgramError> {
        let wasm = compile_program(src, params, CompileTarget::Wasm)?;
        Ok(run_compiled_program(&wasm, args)?)
    }

    fn run_compiled_program(
        wasm: &[u8],
        args: &[wasmer::Value],
    ) -> Result<wasmer::Value, WasmerError> {
        let mut store = Store::new(Singlepass::default());
        let module = Module::new(&store, wasm)?;
        let imports = imports! {};
        let instance = Instance::new(&mut store, &module, &imports).map_err(Box::new)?;

        let eval = instance.exports.get_function(WASM_EVAL_FUNC)?;
        let result = eval.call(&mut store, args)?;

        Ok(IntoIterator::into_iter(result).next().unwrap())
    }
}
