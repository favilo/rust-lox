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

mod assignment {
    use super::*;

    test_std_lox!(associativity, assignment, Value::Nil, "c\nc\nc\n");
    test_std_lox!(global, assignment, Value::Nil, "before\nafter\narg\narg\n");
    test_std_lox!(local, assignment, Value::Nil, "before\nafter\narg\narg\n");
    test_std_lox!(syntax, assignment, Value::Nil, "var\nvar\n");

    test_std_lox_parse_error!(
        grouping,
        assignment,
        "[line 2] Error at '=': Invalid assignment target."
    );
    test_std_lox_parse_error!(
        infix_operator,
        assignment,
        "[line 3] Error at '=': Expect ';'."
    );
    test_std_lox_parse_error!(
        prefix_operator,
        assignment,
        "[line 2] Error at '=': Expect ';'."
    );
    test_std_lox_parse_error!(
        to_this,
        assignment,
        "[line 3] Error at '=': Invalid assignment target."
    );
    test_std_lox_eval_error!(
        undefined,
        assignment,
        EvaluateError::UndefinedVariable("unknown".into())
    );
}

// mod benchmark {
//     use super::*;

// 		test_std_lox!(binary_trees, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(equality, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(fib, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(instantiation, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(invocation, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(method_call, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(properties, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(string_equality, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(trees, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(zoo, benchmark, Value::Nil, "a\n");
// 		test_std_lox!(zoo_batch, benchmark, Value::Nil, "a\n");
// }

mod block {
    use super::*;

    test_std_lox!(empty, block, Value::Nil, "ok\n");
    test_std_lox!(scope, block, Value::Nil, "inner\nouter\n");
}

mod bool {
    use super::*;

    test_std_lox!(
        equality,
        bool,
        Value::Nil,
        "true\nfalse\nfalse\ntrue\n\
            false\nfalse\nfalse\nfalse\nfalse\n\
            false\ntrue\ntrue\nfalse\n\
            true\ntrue\ntrue\ntrue\ntrue\n"
    );
    test_std_lox!(not, bool, Value::Nil, "false\ntrue\ntrue\n");
}

mod call {
    use super::*;

    test_std_lox_eval_error!(bool, call, matches => EvaluateError::NotCallable(_));
    test_std_lox_eval_error!(nil, call, matches => EvaluateError::NotCallable(_));
    test_std_lox_eval_error!(num, call, matches => EvaluateError::NotCallable(_));
    test_std_lox_eval_error!(
        string,
        call,
        EvaluateError::NotCallable(Value::String("str".into()))
    );
    test_std_lox_eval_error!(object, call, matches => EvaluateError::NotCallable(_));
}

mod class {
    use super::*;

    test_std_lox!(empty, class, Value::Nil, "Foo\n");
    test_std_lox_parse_error!(
        inherit_self,
        class,
        "[line 1] Error at 'Foo': A class can't inherit from itself."
    );
    test_std_lox!(
        inherited_method,
        class,
        Value::Nil,
        "in foo\nin bar\nin baz\n"
    );
    test_std_lox!(local_inherit_other, class, Value::Nil, "B\n");
    test_std_lox_parse_error!(
        local_inherit_self,
        class,
        "[line 2] Error at 'Foo': A class can't inherit from itself."
    );
    test_std_lox!(local_reference_self, class, Value::Nil, "Foo\n");
    test_std_lox!(reference_self, class, Value::Nil, "Foo\n");
}

mod closure {
    use super::*;

    test_std_lox!(close_over_method_parameter, closure, Value::Nil, "param\n");
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

mod comments {
    use super::*;

    test_std_lox!(line_at_eof, comments, Value::Nil, "ok\n");
    test_std_lox!(only_line_comment, comments, Value::Nil, "");
    test_std_lox!(only_line_comment_and_line, comments, Value::Nil, "");
    test_std_lox!(unicode, comments, Value::Nil, "ok\n");
}

mod constructor {
    use super::*;

    test_std_lox!(arguments, constructor, Value::Nil, "init\n1\n2\n");
    test_std_lox!(
        call_init_early_return,
        constructor,
        Value::Nil,
        "init\ninit\nFoo instance\n"
    );
    test_std_lox!(
        call_init_explicitly,
        constructor,
        Value::Nil,
        "Foo.init(one)\nFoo.init(two)\nFoo instance\ninit\n"
    );
    test_std_lox!(default, constructor, Value::Nil, "Foo instance\n");
    test_std_lox_eval_error!(
        default_arguments,
        constructor,
        EvaluateError::ArgumentMismatch {
            expected: 0,
            got: 2
        }
    );
    test_std_lox!(
        early_return,
        constructor,
        Value::Nil,
        "init\nFoo instance\n"
    );
    test_std_lox_eval_error!(
        extra_arguments,
        constructor,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 4
        }
    );
    test_std_lox!(
        init_not_method,
        constructor,
        Value::Nil,
        "not initializer\n"
    );
    test_std_lox_eval_error!(
        missing_arguments,
        constructor,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 1
        }
    );
    test_std_lox!(
        return_in_nested_function,
        constructor,
        Value::Nil,
        "bar\nFoo instance\n"
    );
    test_std_lox_parse_error!(
        return_value,
        constructor,
        "Error at 'return': Can't return a value from an initializer."
    );
}

mod expressions {
    use super::*;

    test_std_lox!(evaluate, expressions, Value::Number(2.0), "");
    // Not dealing with tokens
    // test_std_lox!(parse, expressions, Value::Nil, "a\n");
}

mod field {
    use super::*;

    test_std_lox!(call_function_field, field, Value::Nil, "bar\n1\n2\n");
    test_std_lox_eval_error!(call_nonfunction_field, field, matches => EvaluateError::NotCallable(_));
    test_std_lox!(
        get_and_set_method,
        field,
        Value::Nil,
        "other\n1\nmethod\n2\n"
    );
    test_std_lox_eval_error!(
        get_on_bool,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        get_on_class,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        get_on_function,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        get_on_nil,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        get_on_num,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        get_on_string,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox!(
        many,
        field,
        Value::Nil,
        "apple\napricot\navocado\nbanana\nbilberry\nblackberry\n\
            blackcurrant\nblueberry\nboysenberry\ncantaloupe\ncherimoya\n\
            cherry\nclementine\ncloudberry\ncoconut\ncranberry\ncurrant\n\
            damson\ndate\ndragonfruit\ndurian\nelderberry\nfeijoa\nfig\n\
            gooseberry\ngrape\ngrapefruit\nguava\nhoneydew\nhuckleberry\n\
            jabuticaba\njackfruit\njambul\njujube\njuniper\nkiwifruit\n\
            kumquat\nlemon\nlime\nlongan\nloquat\nlychee\nmandarine\nmango\n\
            marionberry\nmelon\nmiracle\nmulberry\nnance\nnectarine\nolive\n\
            orange\npapaya\npassionfruit\npeach\npear\npersimmon\nphysalis\n\
            pineapple\nplantain\nplum\nplumcot\npomegranate\npomelo\nquince\n\
            raisin\nrambutan\nraspberry\nredcurrant\nsalak\nsalmonberry\n\
            satsuma\nstrawberry\ntamarillo\ntamarind\ntangerine\ntomato\n\
            watermelon\nyuzu\n"
    );
    test_std_lox!(method, field, Value::Nil, "got method\narg\n");
    test_std_lox!(method_binds_this, field, Value::Nil, "foo1\n1\n");
    test_std_lox!(
        on_instance,
        field,
        Value::Nil,
        "bar value\nbaz value\nbar value\nbaz value\n"
    );
    test_std_lox_eval_error!(
        set_evaluation_order,
        field,
        EvaluateError::UndefinedVariable("undefined1".into())
    );
    test_std_lox_eval_error!(
        set_on_bool,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        set_on_class,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        set_on_function,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        set_on_nil,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        set_on_num,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        set_on_string,
        field,
        EvaluateError::TypeMismatch {
            expected: "instance".into()
        }
    );
    test_std_lox_eval_error!(
        undefined,
        field,
        EvaluateError::UndefinedProperty("bar".into())
    );
}

mod r#for {
    use super::*;

    test_std_lox_parse_error!(
        class_in_body,
        for,
        "[line 2] Error at 'class': Expect expression."
    );
    test_std_lox!(closure_in_body, for, Value::Nil, "4\n1\n4\n2\n4\n3\n");
    test_std_lox_parse_error!(
        fun_in_body,
        for,
        "[line 2] Error at 'fun': Expect expression."
    );
    test_std_lox!(return_closure, for, Value::Nil, "i\n");
    test_std_lox!(return_inside, for, Value::Nil, "i\n");
    test_std_lox!(scope, for, Value::Nil, "0\n-1\nafter\n0\n");
    test_std_lox_parse_error!(
        statement_condition,
        for,
        "[line 3] Error at '{};': Expect expression."
    );
    test_std_lox_parse_error!(
        statement_increment,
        for,
        "[line 2] Error at '{})': Expect expression."
    );
    test_std_lox_parse_error!(
        statement_initializer,
        for,
        "[line 3] Error at '{};': Expect expression."
    );
    test_std_lox!(
        syntax,
        for,
        Value::Nil,
        "1\n2\n3\n0\n1\n2\ndone\n0\n1\n0\n1\n2\n0\n1\n"
    );
    test_std_lox_parse_error!(
        var_in_body,
        for,
        "[line 2] Error at 'var': Expect expression."
    );
}

mod function {
    use super::*;

    test_std_lox_parse_error!(
        body_must_be_block,
        function,
        "[line 3] Error at '123;': Expect '{'."
    );
    test_std_lox!(empty_body, function, Value::Nil, "nil\n");
    test_std_lox_eval_error!(
        extra_arguments,
        function,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 4
        }
    );
    test_std_lox_eval_error!(
        local_mutual_recursion,
        function,
        EvaluateError::UndefinedVariable("isOdd".into())
    );
    test_std_lox!(local_recursion, function, Value::Nil, "21\n");
    test_std_lox_eval_error!(
        missing_arguments,
        function,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 1
        }
    );
    // TODO: Deal with token fetching during error... Low priority
    test_std_lox_parse_error!(
        missing_comma_in_parameters,
        function,
        "[line 3] Error at 'c,': Expect ')' after parameters."
    );
    test_std_lox!(mutual_recursion, function, Value::Nil, "true\ntrue\n");
    test_std_lox!(
        nested_call_with_arguments,
        function,
        Value::Nil,
        "hello world\n"
    );
    test_std_lox!(
        parameters,
        function,
        Value::Nil,
        "0\n1\n3\n6\n10\n15\n21\n28\n36\n"
    );
    test_std_lox!(print, function, Value::Nil, "<fn foo>\n<native fn>\n");
    test_std_lox!(recursion, function, Value::Nil, "21\n");
    test_std_lox_parse_error!(
        too_many_arguments,
        function,
        "[line 260] Error at '': Can't have more than 255 arguments."
    );
    test_std_lox_parse_error!(
        too_many_parameters,
        function,
        "[line 257] Error at '': Can't have more than 255 parameters."
    );
}

mod r#if {
    use super::*;

    test_std_lox!(dangling_else, if, Value::Nil, "good\n");
    test_std_lox!(else, if, Value::Nil, "good\ngood\nblock\n");
    test_std_lox_parse_error!(
        fun_in_else,
        if,
        "[line 2] Error at 'fun': Expect expression."
    );
    test_std_lox_parse_error!(
        fun_in_then,
        if,
        "[line 2] Error at 'fun': Expect expression."
    );
    test_std_lox!(if, if, Value::Nil, "good\nblock\ntrue\n");
    test_std_lox!(truth, if, Value::Nil, "false\nnil\ntrue\n0\nempty\n");
    test_std_lox_parse_error!(
        var_in_else,
        if,
        "[line 2] Error at 'var': Expect expression."
    );
    test_std_lox_parse_error!(
        var_in_then,
        if,
        "[line 2] Error at 'var': Expect expression."
    );
    test_std_lox_parse_error!(
        class_in_else,
        if,
        "[line 2] Error at 'class': Expect expression."
    );
    test_std_lox_parse_error!(
        class_in_then,
        if,
        "[line 2] Error at 'class': Expect expression."
    );
}

mod inheritance {
    use super::*;

    test_std_lox!(constructor, inheritance, Value::Nil, "value\n");
    test_std_lox!(inherit_from_function, inheritance, Value::Nil, "a\n");
    test_std_lox!(inherit_from_nil, inheritance, Value::Nil, "a\n");
    test_std_lox!(inherit_from_number, inheritance, Value::Nil, "a\n");
    test_std_lox!(inherit_methods, inheritance, Value::Nil, "a\n");
    test_std_lox!(parenthesized_superclass, inheritance, Value::Nil, "a\n");
    test_std_lox!(set_fields_from_base_class, inheritance, Value::Nil, "a\n");
}

// TODO: Deal with limits later
// mod limit {
//     use super::*;

//     // This wants to fail with too large of a loop body
//     #[ignore]
//     test_std_lox_parse_error!(loop_too_large, limit, "Block body too large: 32768");
//     // Edited the file to not return a function, because I don't want to comapre values
//     test_std_lox!(no_reuse_constants, limit, Value::Nil, "");
//     test_std_lox_eval_error!(stack_overflow, limit, EvaluateError::StackOverflow);
//     test_std_lox!(too_many_constants, limit, Value::Nil, "a\n");
//     test_std_lox!(too_many_locals, limit, Value::Nil, "a\n");
//     test_std_lox!(too_many_upvalues, limit, Value::Nil, "a\n");
// }

mod logical_operator {
    use super::*;

    test_std_lox!(
        and,
        logical_operator,
        Value::Nil,
        "false\n1\nfalse\ntrue\n3\ntrue\nfalse\n"
    );
    test_std_lox!(
        and_truth,
        logical_operator,
        Value::Nil,
        "false\nnil\nok\nok\nok\n"
    );
    test_std_lox!(
        or,
        logical_operator,
        Value::Nil,
        "1\n1\ntrue\nfalse\nfalse\nfalse\ntrue\n"
    );
    test_std_lox!(
        or_truth,
        logical_operator,
        Value::Nil,
        "ok\nok\ntrue\n0\ns\n"
    );
}

mod method {
    use super::*;

    test_std_lox!(
        arity,
        method,
        Value::Nil,
        "no args\n1\n3\n6\n10\n15\n21\n28\n36\n"
    );
    test_std_lox!(empty_block, method, Value::Nil, "nil\n");
    test_std_lox_eval_error!(
        extra_arguments,
        method,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 4
        }
    );
    test_std_lox_eval_error!(
        missing_arguments,
        method,
        EvaluateError::ArgumentMismatch {
            expected: 2,
            got: 1
        }
    );
    test_std_lox_eval_error!(
        not_found,
        method,
        EvaluateError::UndefinedProperty("unknown".into())
    );
    test_std_lox!(print_bound_method, method, Value::Nil, "<fn method>\n");
    test_std_lox_eval_error!(
        refer_to_name,
        method,
        EvaluateError::UndefinedVariable("method".into())
    );
    test_std_lox_parse_error!(
        too_many_arguments,
        method,
        "[line 259] Error at '': Can't have more than 255 arguments."
    );
    test_std_lox_parse_error!(
        too_many_parameters,
        method,
        "[line 258] Error at '': Can't have more than 255 parameters."
    );
}

mod nil {
    use super::*;

    test_std_lox!(literal, nil, Value::Nil, "nil\n");
}

mod number {
    use super::*;

    test_std_lox!(
        literals,
        number,
        Value::Nil,
        "123\n987654\n0\n-0\n123.456\n-0.001\n"
    );
    test_std_lox!(
        nan_equality,
        number,
        Value::Nil,
        "false\ntrue\nfalse\ntrue\n"
    );

    test_std_lox_parse_error!(
        decimal_point_at_eof,
        number,
        "[line 2] Error at '': Expect property name after '.'."
    );
    // This is supposed to be an error, but I'm fine with leading dots.
    test_std_lox!(leading_dot, number, Value::Number(0.123), "");
    test_std_lox_parse_error!(
        trailing_dot,
        number,
        "[line 2] Error at ';': Expect property name after '.'."
    );
}

mod operator {
    use super::*;

    test_std_lox!(add, operator, Value::Nil, "579\nstring\n");
    test_std_lox_eval_error!(
        add_bool_nil,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox_eval_error!(
        add_bool_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox_eval_error!(
        add_bool_string,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox_eval_error!(
        add_nil_nil,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox_eval_error!(
        add_num_nil,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox_eval_error!(
        add_string_nil,
        operator,
        EvaluateError::TypeMismatch {
            expected: "two numbers or two strings".into()
        }
    );
    test_std_lox!(
        comparison,
        operator,
        Value::Nil,
        "true\nfalse\nfalse\ntrue\ntrue\nfalse\nfalse\nfalse\ntrue\nfalse\ntrue\ntrue\n\
            false\nfalse\nfalse\nfalse\ntrue\ntrue\ntrue\ntrue\n"
    );
    test_std_lox!(divide, operator, Value::Nil, "4\n1\n");
    test_std_lox_eval_error!(
        divide_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        divide_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox!(
        equals,
        operator,
        Value::Nil,
        "true\ntrue\nfalse\ntrue\nfalse\ntrue\nfalse\nfalse\nfalse\nfalse\n"
    );
    test_std_lox_eval_error!(
        greater_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        greater_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        greater_or_equal_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        greater_or_equal_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        less_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        less_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        less_or_equal_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        less_or_equal_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox!(multiply, operator, Value::Nil, "15\n3.702\n");
    test_std_lox_eval_error!(
        multiply_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        multiply_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox!(negate, operator, Value::Nil, "-3\n3\n-3\n");
    test_std_lox_eval_error!(
        negate_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "number".into()
        }
    );
    test_std_lox!(
        not,
        operator,
        Value::Nil,
        "false\ntrue\ntrue\nfalse\nfalse\ntrue\nfalse\nfalse\n"
    );
    test_std_lox!(
        not_equals,
        operator,
        Value::Nil,
        "false\nfalse\ntrue\nfalse\ntrue\nfalse\ntrue\ntrue\ntrue\ntrue\n"
    );
    test_std_lox!(subtract, operator, Value::Nil, "1\n0\n");
    test_std_lox_eval_error!(
        subtract_nonnum_num,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );
    test_std_lox_eval_error!(
        subtract_num_nonnum,
        operator,
        EvaluateError::TypeMismatch {
            expected: "numbers".into()
        }
    );

    test_std_lox!(
        equals_class,
        operator,
        Value::Nil,
        "true\nfalse\nfalse\ntrue\nfalse\nfalse\nfalse\nfalse\n"
    );
    // TODO: Closure's for methods need new implementation to pass `this` to the method at
    // `GetProperty` time
    // test_std_lox!(equals_method, operator, Value::Nil, "true\nfalse\n");

    test_std_lox!(not_class, operator, Value::Nil, "false\nfalse\n");
}

mod print {
    use super::*;

    test_std_lox_parse_error!(
        missing_argument,
        print,
        "[line 2] Error at ';': Expect expression."
    );
}

mod regression {
    use super::*;

    test_std_lox!(_40, regression, Value::Nil, "false\n");

    // TODO: inheritance
    // test_std_lox!(_394, regression, Value::Nil, "a\n");
}

mod r#return {
    use super::*;

    test_std_lox!(after_else, return, Value::Nil, "ok\n");
    test_std_lox!(after_if, return, Value::Nil, "ok\n");
    test_std_lox!(after_while, return, Value::Nil, "ok\n");
    test_std_lox_eval_error!(at_top_level, return, EvaluateError::TopLevelReturn);
    test_std_lox!(in_function, return, Value::Nil, "ok\n");
    test_std_lox!(return_nil_if_no_value, return, Value::Nil, "nil\n");
    test_std_lox!(in_method, return, Value::Nil, "ok\n");
}

// I don't want to deal with Tokens right this moment
// mod scanning {
//     use super::*;

//     test_std_lox!(identifiers, scanning, Value::Nil, "a\n");
//     test_std_lox!(keywords, scanning, Value::Nil, "a\n");
//     test_std_lox!(numbers, scanning, Value::Nil, "a\n");
//     test_std_lox!(punctuators, scanning, Value::Nil, "a\n");
//     test_std_lox!(strings, scanning, Value::Nil, "a\n");
//     test_std_lox!(whitespace, scanning, Value::Nil, "a\n");
// }

mod string {
    use super::*;

    test_std_lox_eval_error!(
        error_after_multiline,
        string,
        EvaluateError::UndefinedVariable("err".into())
    );
    test_std_lox!(literals, string, Value::Nil, "()\na string\nA~¶Þॐஃ\n");
    test_std_lox!(multiline, string, Value::Nil, "1\n2\n3\n");
    test_std_lox_parse_error!(
        unterminated,
        string,
        "[line 2] Error at 'this': Unterminated string."
    );
}

// mod super {}
// mod this {}

mod variable {
    use super::*;

    test_std_lox!(local_from_method, variable, Value::Nil, "variable\n");

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

    test_std_lox_parse_error!(
        class_in_body,
        while,
        "[line 2] Error at 'class': Expect expression."
    );
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
