use static_script::{compile_program, CompileTarget, PlainType, WASM_EVAL_FUNC};
use std::iter;
use wasmer::{imports, Instance, Module, Store, Type, Value};
use wasmer_compiler_singlepass::Singlepass;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn run_test(src: &str, ret_ty: PlainType, expected: Value) -> Result<()> {
    let wasm = compile_program(src, iter::empty(), CompileTarget::Wasm)?;

    let mut store = Store::new(Singlepass::default());
    let module = Module::new(&store, &wasm)?;
    let instance = Instance::new(&mut store, &module, &imports! {})?;

    let eval = instance.exports.get_function(WASM_EVAL_FUNC)?;
    let result = eval.call(&mut store, &[])?;

    assert_eq!(result.len(), 1);
    let result = IntoIterator::into_iter(result).next().unwrap();

    assert_eq!(
        result.ty(),
        match ret_ty {
            PlainType::I32 => Type::I32,
            PlainType::F32 => Type::F32,
        }
    );

    assert_eq!(result, expected);
    Ok(())
}

macro_rules! tests {
    ($($name:ident, $src:expr => ($ty:ident, $val:expr)),* $(,)?) => {
        $(
            #[test]
            fn $name() -> Result<()> {
                let (ret_ty, expected) = tests!(@result $ty $val);
                run_test($src, ret_ty, expected)
            }
        )*
    };

    (@result $ty:ident $val:expr) => {
        (tests!(@result_type $ty), Value::from($val as $ty))
    };

    (@result_type i32) => { PlainType::I32 };
    (@result_type f32) => { PlainType::F32 };
}

tests! {
    const_i32, "123" => (i32, 123),
    const_f32_1, "123.56" => (f32, 123.56),
    const_f32_2, ".56" => (f32, 0.56),
    const_f32_3, "123." => (f32, 123),
    const_f32_4, "13.56E4" => (f32, 13.56E4),
    const_f32_5, "13.56E-5" => (f32, 13.56E-5),

    i32_un_plus, "+123" => (i32, 123),
    f32_un_plus, "+123.56" => (f32, 123.56),
    i32_neg, "-123" => (i32, -123),
    f32_neg, "-123.56" => (f32, -123.56),
    i32_un_multiple, "+- \n \t+  +--123" => (i32, -123),
    f32_un_multiple, " +-  +-123.56" => (f32, 123.56),

    i32_add, "123 + 456" => (i32, 123 + 456),
    i32_sub, "123 - 456" => (i32, 123 - 456),
    i32_mul, "123 * 456" => (i32, 123 * 456),
    i32_div, "45671 / 23" => (i32, 45671 / 23),
    i32_mod, "45671 % 23" => (i32, 45671 % 23),
    f32_add, "123.34 + 456.01" => (f32, 123.34_f32 + 456.01_f32),
    f32_sub, "123.34 - 456.03" => (f32, 123.34_f32 - 456.03_f32),
    f32_mul, "123.67 * 456.098" => (f32, 123.67_f32 * 456.098_f32),
    f32_div, "45671.023 / 23.0E-1" => (f32, 45671.023_f32 / 23.0E-1_f32),

    precedence_1, "2 + 3 * 41 / 2" => (i32, 2 + ((3 * 41) / 2)),
    precedence_2, "2 + 3 * (41 / 2)" => (i32, 2 + (3 * (41 / 2))),
}
