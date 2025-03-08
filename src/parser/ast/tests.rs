use super::*;

use crate::test_utils::{test_std_lox, test_std_lox_eval_error, test_std_lox_parse_error};

#[test_log::test]
fn test_mul_div() {
    let input = "1 / 2 * 3;";
    let ast = parse(input).unwrap();
    let res = ast.evaluate(&Context::new());
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Value::Number(1.0 / 2.0 * 3.0));
}

#[test_log::test]
fn test_print_var() {
    let input = "var a = 1; print a;";
    let ast = parse(input).unwrap();
    let res = ast.run();
    assert!(res.is_ok());
}

#[test_log::test]
fn test_print_empty() {
    let input = "print;";
    let ast = parse(input);
    assert!(ast.is_err());
    assert_eq!(
        ast.unwrap_err().to_string(),
        "[line 1] Error at ';': Expect expression."
    );
}

#[test_log::test]
fn test_print_not_keyword() {
    let input = "
            fun printAndModify() {}
            printAndModify();";
    let ast = parse(input).unwrap();
    let res = ast.run();
    assert!(res.is_ok());
}

#[test_log::test]
fn test_var() {
    let input = r#"
// This program tests statements that don't have any side effects
19 - 93 >= -46 * 2 / 46 + 77;
true == true;
("hello" == "bar") == ("baz" != "world");
print true;
"#;
    let ast = parse(input).unwrap();
    let res = ast.run();
    assert!(res.is_ok());
}

#[test_log::test]
fn test_block() {
    let input = r"
            var a = 1;
            {
                var b = 2;
                a = 5;
                b = 3;
                a + b;
            }
        ";
    let env = Context::new();
    let ast: Ast = parse(input).unwrap();
    let res = ast.evaluate(&env).unwrap();
    assert_eq!(res, Value::Number(8.0));
}

#[test_log::test]
fn test_condition() {
    let input = r"if (true) {3;} else {4;}";
    let env = Context::new();
    let ast: Ast = parse(input).unwrap();
    let res = ast.evaluate(&env).unwrap();
    assert_eq!(res, Value::Number(3.0));
}

#[test_log::test]
fn test_for_loop() {
    let input = r"
            var a = 0;
            for (var i = 0; i < 10; i = i + 1) {
                a = a + i;
            }
            a;
        ";
    let env = Context::new();
    let ast: Ast = parse(input).unwrap();
    let res = ast.evaluate(&env).unwrap();
    assert_eq!(res, Value::Number(45.0));
}

#[test_log::test]
fn test_nested_fn() {
    let input = r"
            fun fib(n) {
              if (n < 2) return n;
              return fib(n - 2) + fib(n - 1);
            }
            fib(10);
        ";

    let env = Context::new();
    let ast: Ast = parse(input).unwrap();
    let res = ast.evaluate(&env).unwrap();
    assert_eq!(res, Value::Number(55.0));
}

#[test_log::test]
fn test_static_scope() {
    let input = r#"
            var a = "global";
            {
              fun showA() {
                print a;
              }

              showA();
              var a = "block";
              showA();
            }"#;

    let env = Context::new();
    let ast: Ast = parse(input).unwrap();
    let res = ast.evaluate(&env).unwrap();
    assert_eq!(res, Value::Nil);
}

mod closure {
    use super::*;

    // TODO: classes
    // test_std_lox_test!(close_over_method_parameter, closure, Value::Nil, "a\n");

    test_std_lox!(unused_later_closure, closure, Value::Nil, "a\n");
    test_std_lox!(unused_closure, closure, Value::Nil, "ok\n");
    test_std_lox!(
        reference_closure_multiple_times,
        closure,
        Value::Nil,
        "a\na\n"
    );
    test_std_lox!(open_closure_in_function, closure, Value::Nil, "local\n");
    test_std_lox!(closed_closure_in_function, closure, Value::Nil, "local\n");
    test_std_lox!(close_over_later_variable, closure, Value::Nil, "b\na\n");
    test_std_lox!(
        close_over_function_parameter,
        closure,
        Value::Nil,
        "param\n"
    );
    test_std_lox!(
        assign_to_shadowed_later,
        closure,
        Value::Nil,
        "inner\nassigned\n"
    );
    test_std_lox!(
        assign_to_closure,
        closure,
        Value::Nil,
        "local\nafter f\nafter f\nafter g\n"
    );
    test_std_lox!(nested_closure, closure, Value::Nil, "a\nb\nc\n");
    test_std_lox!(
        shadow_closure_with_local,
        closure,
        Value::Nil,
        "closure\nshadow\nclosure\n"
    );
    test_std_lox!(reuse_closure_slot, closure, Value::Nil, "a\n");
}

mod variable {
    use super::*;

    // TODO: classes
    // test_std_lox!(local_from_method, variable, Value::Nil, "variable\n");

    test_std_lox_parse_error!(
        collide_with_parameter,
        variable,
        "Error at 'a': Already a variable with this name in this scope."
    );
    test_std_lox_parse_error!(
        duplicate_local,
        variable,
        "Error at 'a': Already a variable with this name in this scope."
    );
    test_std_lox_parse_error!(
        duplicate_parameter,
        variable,
        "Error at 'arg': Already a variable with this name in this scope."
    );
    test_std_lox!(early_bound, variable, Value::Nil, "outer\nouter\n");
    test_std_lox!(
        in_middle_of_block,
        variable,
        Value::Nil,
        "a\na b\na c\na b d\n"
    );
    test_std_lox!(in_nested_block, variable, Value::Nil, "outer\n");
    test_std_lox!(redeclare_global, variable, Value::Nil, "nil\n");
    test_std_lox!(redefine_global, variable, Value::Nil, "2\n");
    test_std_lox!(
        scope_reuse_in_different_blocks,
        variable,
        Value::Nil,
        "first\nsecond\n"
    );
    test_std_lox!(shadow_and_local, variable, Value::Nil, "outer\ninner\n");
    test_std_lox!(shadow_global, variable, Value::Nil, "shadow\nglobal\n");
    test_std_lox!(shadow_local, variable, Value::Nil, "shadow\nlocal\n");
    test_std_lox_eval_error!(
        undefined_global,
        variable,
        EvaluateError::UndefinedVariable("notDefined".to_string())
    );
    test_std_lox_eval_error!(
        undefined_local,
        variable,
        EvaluateError::UndefinedVariable("notDefined".to_string())
    );
    test_std_lox!(uninitialized, variable, Value::Nil, "nil\n");
    test_std_lox!(unreached_undefined, variable, Value::Nil, "ok\n");
    test_std_lox_parse_error!(
        use_false_as_var,
        variable,
        "[line 2] Error at 'false': Expect variable name."
    );
    test_std_lox!(use_global_in_initializer, variable, Value::Nil, "value\n");
    test_std_lox_parse_error!(
        use_local_in_initializer,
        variable,
        "Error at 'a': Can't read local variable in its own initializer."
    );
    test_std_lox_parse_error!(
        use_nil_as_var,
        variable,
        "[line 2] Error at 'nil': Expect variable name."
    );
    test_std_lox_parse_error!(
        use_this_as_var,
        variable,
        "[line 2] Error at 'this': Expect variable name."
    );
}

mod r#while {
    use super::*;

    // TODO: classes
    // test_std_lox_test!(class_in_body, while, Value::Nil, "a\n");
    test_std_lox!(closure_in_body, while, Value::Nil, "1\n2\n3\n");
    test_std_lox_parse_error!(
        fun_in_body,
        while,
        "[line 2] Error at 'fun': Expect expression."
    );
    test_std_lox!(return_closure, while, Value::Nil, "i\n");
    test_std_lox!(return_inside, while, Value::Nil, "i\n");
    test_std_lox!(syntax, while, Value::Nil, "1\n2\n3\n0\n1\n2\n");
    test_std_lox_parse_error!(
        var_in_body,
        while,
        "[line 2] Error at 'var': Expect expression."
    );
}
