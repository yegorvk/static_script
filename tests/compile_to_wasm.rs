use static_script::runner::run_program;
use static_script::{Param, PlainType};
use wasmer::{Type, Value};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn run_test(
    src: &str,
    params: &[Param],
    ret_ty: PlainType,
    args: &[Value],
    expected: Value,
) -> Result<()> {
    let result = run_program(src, params, args)?;

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
    ($($name:ident, $( [$($params:tt)*], )? $src:literal => ($ty:ident, $val:expr)),* $(,)?) => {
        $(
            #[test]
            #[allow(unused_mut)]
            fn $name() -> Result<()> {
                let (ret_ty, expected) = tests!(@result $ty $val);
                let (params, args) = tests!(@params $( $($params)* )*);
                run_test($src, &params, ret_ty, &args, expected)
            }
        )*
    };
    
    (@params $($param_name:ident: $param_ty:ident = $arg:expr),* $(,)?) => {
        {
            let mut params: Vec<Param> = vec![];
            let mut args: Vec<Value> = vec![];
            
            $(
                params.push(Param::new(stringify!($param_name), tests!(@parse_type $param_ty)));
                args.push(($arg as $param_ty).into());
            )*
            
            (params, args)
        }
    };
    
    (@params) => { ([], []) };

    (@result $ty:ident $val:expr) => {
        (tests!(@parse_type $ty), Value::from($val as $ty))
    };

    (@parse_type i32) => { PlainType::I32 };
    (@parse_type f32) => { PlainType::F32 };
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
    
    param_1, [x: i32 = 11], "13 + 2 * x + 1" => (i32, 13 + 2 * 11 + 1),
    param_2, [x: f32 = 11.5], "13.1 + 2.2 * x" => (f32, 13.1_f32 + 2.2_f32 * 11.5_f32),
    param_3, [x: i32 = 11, y: i32 = -1], "x*y - 2*x + 3*y" => (i32, -11 - 2 * 11 - 3),
    param_4, [long_name_123: f32 = 1e9], "long_name_123*2.0" => (f32, 1e9_f32 * 2f32),
}
