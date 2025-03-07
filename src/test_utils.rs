macro_rules! test_std_lox_test {
    ($name:ident, $folder:ident, $expected:expr, $stdout:expr) => {
        paste::paste! {
            #[test]
            fn [< test_ $name >]() {
                let input = include_str!(concat!("../../tests/", stringify!($folder),"/", stringify!($name),  ".lox"));
                let env = Context::new().with_stdout();
                let ast: Ast = parse(input).unwrap();
                let res = ast.evaluate(&env).unwrap();
                let stdout = env.stdout().unwrap();
                assert_eq!(res, $expected);
                assert_eq!(stdout, $stdout);
            }
        }
    };
}

pub(crate) use test_std_lox_test;
