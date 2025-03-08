macro_rules! include_test_file {
    ($folder:ident, $name:ident) => {
        include_str!(concat!(
            manifest_dir_macros::path!("tests"),
            "/",
            stringify!($folder),
            "/",
            stringify!($name),
            ".lox"
        ))
    };
}
macro_rules! test_std_lox {
    ($name:ident, $folder:ident, $expected:expr, $stdout:expr) => {
        paste::paste! {
            #[test_log::test]
            fn [< test_ $name >]() {
                let input = crate::test_utils::include_test_file!($folder, $name);
                let env = Context::new().with_stdout();
                let ast = parse(input).unwrap();
                let res = ast.evaluate(&env).unwrap();
                let stdout = env.stdout().unwrap();
                assert_eq!(res, $expected);
                assert_eq!(stdout, $stdout);
            }
        }
    };
}

macro_rules! test_std_lox_parse_error {
    ($name:ident, $folder:ident, $expected:expr) => {
        paste::paste! {
            #[test_log::test]
            fn [< test_ $name >]() {
                let input = crate::test_utils::include_test_file!($folder, $name);
                let res = parse(input);
                assert!(res.is_err());
                assert_eq!(res.unwrap_err().to_string(), $expected);
            }
        }
    };
}

macro_rules! test_std_lox_eval_error {
    ($name:ident, $folder:ident, $expected:expr) => {
        paste::paste! {
            #[test_log::test]
            fn [< test_ $name >]() {
                let input = crate::test_utils::include_test_file!($folder, $name);
                let ast = parse(input).unwrap();
                let res = ast.run();
                assert!(res.is_err());
                assert_eq!(res.unwrap_err(), $expected);
            }
        }
    };
}

pub(crate) use include_test_file;
pub(crate) use test_std_lox;
pub(crate) use test_std_lox_eval_error;
pub(crate) use test_std_lox_parse_error;
