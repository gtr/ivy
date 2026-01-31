//! Tests for the Ivy evaluator.

#[cfg(test)]
mod env_tests {
    use crate::env::Env;
    use crate::value::Value;
    use ivy_syntax::Span;

    #[test]
    fn test_define_and_get() {
        let env = Env::new();
        env.define("x", Value::Int(42), false);
        assert!(matches!(env.get("x"), Some(Value::Int(42))));
    }

    #[test]
    fn test_undefined_variable() {
        let env = Env::new();
        assert!(env.get("undefined").is_none());
    }

    #[test]
    fn test_scoping() {
        let env = Env::new();
        env.define("x", Value::Int(1), false);

        env.push_scope();
        env.define("x", Value::Int(2), false);
        assert!(matches!(env.get("x"), Some(Value::Int(2))));

        env.pop_scope();
        assert!(matches!(env.get("x"), Some(Value::Int(1))));
    }

    #[test]
    fn test_mutable_assignment() {
        let env = Env::new();
        let span = Span::new(0, 1);

        env.define("x", Value::Int(1), true); // mutable
        assert!(env.assign("x", Value::Int(2), span).is_ok());
        assert!(matches!(env.get("x"), Some(Value::Int(2))));
    }

    #[test]
    fn test_immutable_assignment_fails() {
        let env = Env::new();
        let span = Span::new(0, 1);

        env.define("x", Value::Int(1), false); // immutable
        assert!(env.assign("x", Value::Int(2), span).is_err());
    }

    #[test]
    fn test_list_bindings() {
        let env = Env::new();
        env.define("alpha", Value::Int(1), false);
        env.define("beta", Value::Int(2), false);
        env.define("gamma", Value::Int(3), false);

        let bindings = env.list_bindings();
        assert!(bindings.contains(&"alpha".to_string()));
        assert!(bindings.contains(&"beta".to_string()));
        assert!(bindings.contains(&"gamma".to_string()));
    }

    #[test]
    fn test_fork_independence() {
        let env = Env::new();
        env.define("x", Value::Int(1), false);

        let forked = env.fork();
        forked.define("y", Value::Int(2), false);

        assert!(env.get("y").is_none());
        assert!(forked.get("x").is_some());
        assert!(forked.get("y").is_some());
    }
}

#[cfg(test)]
mod value_tests {
    use crate::value::{vec_to_list, ListValue, Value};

    #[test]
    fn test_value_type_names() {
        assert_eq!(Value::Unit.type_name(), "()");
        assert_eq!(Value::Bool(true).type_name(), "Bool");
        assert_eq!(Value::Int(42).type_name(), "Int");
        assert_eq!(Value::Float(3.14).type_name(), "Float");
        assert_eq!(Value::String("hello".to_string()).type_name(), "String");
        assert_eq!(Value::Char('a').type_name(), "Char");
        assert_eq!(
            Value::Module {
                name: "Math".to_string()
            }
            .type_name(),
            "<module Math>"
        );
    }

    #[test]
    fn test_vec_to_list() {
        let list = vec_to_list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        if let Value::List(l) = list {
            let vec = l.to_vec();
            assert_eq!(vec.len(), 3);
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_empty_list() {
        let list = vec_to_list(vec![]);
        if let Value::List(l) = list {
            assert!(matches!(l.as_ref(), ListValue::Nil));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_tuple() {
        let tuple = Value::Tuple(vec![Value::Int(1), Value::String("hello".to_string())]);
        assert_eq!(tuple.type_name(), "(_, _)");
    }

    #[test]
    fn test_constructor() {
        let some = Value::Constructor {
            type_name: "Option".to_string(),
            variant: "Some".to_string(),
            fields: vec![Value::Int(42)],
        };
        assert_eq!(some.type_name(), "Option");
        assert_eq!(format!("{}", some), "Some(42)");
    }

    #[test]
    fn test_value_display() {
        assert_eq!(format!("{}", Value::Unit), "()");
        assert_eq!(format!("{}", Value::Bool(true)), "true");
        assert_eq!(format!("{}", Value::Int(42)), "42");
        assert_eq!(format!("{}", Value::String("hi".to_string())), "hi");
        assert_eq!(format!("{:?}", Value::String("hi".to_string())), "\"hi\"");
    }
}

#[cfg(test)]
mod interpreter_tests {
    use crate::Interpreter;
    use crate::Value;

    fn eval(code: &str) -> Result<Value, String> {
        let mut interp = Interpreter::new();
        let program = ivy_parse::parse(code).map_err(|e| format!("{:?}", e))?;
        interp.eval_program(&program).map_err(|e| format!("{:?}", e))
    }

    #[test]
    fn test_literals() {
        assert!(matches!(eval("42;"), Ok(Value::Int(42))));
        assert!(matches!(eval("true;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("false;"), Ok(Value::Bool(false))));
        assert!(matches!(eval("();"), Ok(Value::Unit)));
    }

    #[test]
    fn test_arithmetic() {
        assert!(matches!(eval("1 + 2;"), Ok(Value::Int(3))));
        assert!(matches!(eval("10 - 3;"), Ok(Value::Int(7))));
        assert!(matches!(eval("4 * 5;"), Ok(Value::Int(20))));
        assert!(matches!(eval("15 / 3;"), Ok(Value::Int(5))));
        assert!(matches!(eval("17 % 5;"), Ok(Value::Int(2))));
    }

    #[test]
    fn test_comparison() {
        assert!(matches!(eval("1 < 2;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("2 > 1;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("1 == 1;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("1 != 2;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("1 <= 1;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("2 >= 2;"), Ok(Value::Bool(true))));
    }

    #[test]
    fn test_boolean_ops() {
        assert!(matches!(eval("true and true;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("true and false;"), Ok(Value::Bool(false))));
        assert!(matches!(eval("false or true;"), Ok(Value::Bool(true))));
        assert!(matches!(eval("false or false;"), Ok(Value::Bool(false))));
        assert!(matches!(eval("!true;"), Ok(Value::Bool(false))));
        assert!(matches!(eval("!false;"), Ok(Value::Bool(true))));
    }

    #[test]
    fn test_short_circuit() {
        assert!(matches!(eval("false and (1/0 == 1);"), Ok(Value::Bool(false))));
        assert!(matches!(eval("true or (1/0 == 1);"), Ok(Value::Bool(true))));
    }

    #[test]
    fn test_string_concat() {
        if let Ok(Value::String(s)) = eval(r#""hello" ++ " world";"#) {
            assert_eq!(s, "hello world");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_let_binding() {
        assert!(matches!(eval("let x = 42; x;"), Ok(Value::Int(42))));
    }

    #[test]
    fn test_let_shadowing() {
        assert!(matches!(eval("let x = 1; let x = 2; x;"), Ok(Value::Int(2))));
    }

    #[test]
    fn test_mutable_binding() {
        assert!(matches!(eval("let mut x = 1; x = 2; x;"), Ok(Value::Int(2))));
    }

    #[test]
    fn test_immutable_assignment_error() {
        assert!(eval("let x = 1; x = 2;").is_err());
    }

    #[test]
    fn test_if_expression() {
        assert!(matches!(eval("if true then 1 else 2;"), Ok(Value::Int(1))));
        assert!(matches!(eval("if false then 1 else 2;"), Ok(Value::Int(2))));
    }

    #[test]
    fn test_function_definition() {
        assert!(matches!(eval("fn add(x, y) => x + y; add(2, 3);"), Ok(Value::Int(5))));
    }

    #[test]
    fn test_recursive_function() {
        let code = r#"
      fn fact(0) => 1;
      fn fact(n) => n * fact(n - 1);
      fact(5);
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(120))));
    }

    #[test]
    fn test_lambda() {
        // Lambda as immediately invoked function expression
        assert!(matches!(eval("(fn (x) => x + 1)(5);"), Ok(Value::Int(6))));
        // Lambda stored in variable
        assert!(matches!(eval("let f = fn (x) => x * 2; f(3);"), Ok(Value::Int(6))));
    }

    #[test]
    fn test_partial_application() {
        assert!(matches!(
            eval("fn add(x, y) => x + y; let add5 = add(5); add5(3);"),
            Ok(Value::Int(8))
        ));
        assert!(matches!(eval("fn add(x, y) => x + y; add(1)(2);"), Ok(Value::Int(3))));
        assert!(matches!(
            eval("fn foo(a, b, c) => a + b + c; foo(1)(2)(3);"),
            Ok(Value::Int(6))
        ));
        assert!(matches!(
            eval("fn foo(a, b, c) => a + b + c; foo(1, 2)(3);"),
            Ok(Value::Int(6))
        ));
        assert!(matches!(
            eval("fn foo(a, b, c) => a + b + c; foo(1)(2, 3);"),
            Ok(Value::Int(6))
        ));
        assert!(matches!(
            eval("fn add(x, y) => x + y; let f = add(10); f(5);"),
            Ok(Value::Int(15))
        ));
        assert!(matches!(
            eval("fn makeAdder(x) => fn (y) => x + y; makeAdder(10)(5);"),
            Ok(Value::Int(15))
        ));
    }

    #[test]
    fn test_list_literal() {
        if let Ok(Value::List(l)) = eval("[1, 2, 3];") {
            assert_eq!(l.to_vec().len(), 3);
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_cons_operator() {
        if let Ok(Value::List(l)) = eval("[1 | [2, 3]];") {
            let vec = l.to_vec();
            assert_eq!(vec.len(), 3);
            assert!(matches!(vec[0], Value::Int(1)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_tuple_literal() {
        if let Ok(Value::Tuple(t)) = eval("(1, 2, 3);") {
            assert_eq!(t.len(), 3);
        } else {
            panic!("Expected tuple");
        }
    }

    #[test]
    fn test_tuple_index() {
        assert!(matches!(eval("(1, 2, 3)[0];"), Ok(Value::Int(1))));
        assert!(matches!(eval("(1, 2, 3)[2];"), Ok(Value::Int(3))));
    }

    #[test]
    fn test_match_expression() {
        let code = r#"
      match Some(42) with
      | None -> 0
      | Some(x) -> x
      end;
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(42))));
    }

    #[test]
    fn test_match_list() {
        let code = r#"
      match [1, 2, 3] with
      | [] -> 0
      | [x | _] -> x
      end;
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(1))));
    }

    #[test]
    fn test_do_block() {
        let code = r#"
      do {
        let x = 1;
        let y = 2;
        x + y
      };
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(3))));
    }

    #[test]
    fn test_type_definition() {
        let code = r#"
      type Color = | Red | Green | Blue;
      Red;
    "#;
        if let Ok(Value::Constructor { variant, .. }) = eval(code) {
            assert_eq!(variant, "Red");
        } else {
            panic!("Expected constructor");
        }
    }

    #[test]
    fn test_record_type() {
        let code = r#"
      type Point = { x: Int, y: Int };
      Point { x: 10, y: 20 };
    "#;
        if let Ok(Value::Record { type_name, fields }) = eval(code) {
            assert_eq!(type_name, "Point");
            assert!(matches!(fields.get("x"), Some(Value::Int(10))));
            assert!(matches!(fields.get("y"), Some(Value::Int(20))));
        } else {
            panic!("Expected record");
        }
    }

    #[test]
    fn test_record_field_access() {
        let code = r#"
      type Point = { x: Int, y: Int };
      let p = Point { x: 10, y: 20 };
      p.x;
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(10))));
    }

    #[test]
    fn test_pattern_matching_in_function() {
        let code = r#"
      fn fib(0) => 0;
      fn fib(1) => 1;
      fn fib(n) => fib(n-1) + fib(n-2);
      fib(10);
    "#;
        assert!(matches!(eval(code), Ok(Value::Int(55))));
    }

    #[test]
    fn test_guarded_function() {
        let code = r#"
      fn classify(n)
        | n < 0 => "negative"
        | n == 0 => "zero"
        | n > 0 => "positive";
      classify(-5);
    "#;
        if let Ok(Value::String(s)) = eval(code) {
            assert_eq!(s, "negative");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_division_by_zero() {
        assert!(eval("1 / 0;").is_err());
    }

    #[test]
    fn test_undefined_variable_error() {
        assert!(eval("undefined_var;").is_err());
    }

    #[test]
    fn test_type_error() {
        assert!(eval("1 + true;").is_err());
    }

    #[test]
    fn test_arity_mismatch() {
        assert!(eval("fn f(x) => x; f(1, 2);").is_err());
    }

    #[test]
    fn test_not_callable() {
        assert!(eval("42(1);").is_err());
    }

    #[test]
    fn test_list_bindings() {
        let mut interp = Interpreter::new();
        let program = ivy_parse::parse("let myVar = 42;").unwrap();
        interp.eval_program(&program).unwrap();

        let bindings = interp.list_bindings();
        assert!(bindings.contains(&"myVar".to_string()));
        // Builtins should be filtered out
        assert!(!bindings.contains(&"print".to_string()));
    }
}

#[cfg(test)]
mod loader_tests {
    use crate::loader::ModuleLoader;
    use std::path::PathBuf;

    #[test]
    fn test_resolve_path_not_found() {
        let loader = ModuleLoader::new(vec![PathBuf::from("/nonexistent")]);
        assert!(loader.resolve_path(&["Math".to_string()]).is_none());
    }

    #[test]
    fn test_empty_search_paths() {
        let loader = ModuleLoader::new(vec![]);
        assert!(loader.resolve_path(&["Anything".to_string()]).is_none());
    }

    #[test]
    fn test_add_search_path() {
        let mut loader = ModuleLoader::new(vec![]);
        loader.add_search_path(PathBuf::from("/some/path"));
        // Can't easily test resolution without actual files, but we can test the loader doesn't crash
    }

    #[test]
    fn test_is_loaded() {
        let loader = ModuleLoader::new(vec![]);
        assert!(!loader.is_loaded("NonexistentModule"));
    }
}
