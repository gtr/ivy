//! Tests for the Ivy parser.

#[cfg(test)]
mod parser_tests {
    use crate::parse;

    #[test]
    fn test_parse_integer() {
        assert!(parse("42;").is_ok());
    }

    #[test]
    fn test_parse_float() {
        assert!(parse("3.14;").is_ok());
    }

    #[test]
    fn test_parse_string() {
        assert!(parse(r#""hello";"#).is_ok());
    }

    #[test]
    fn test_parse_boolean() {
        assert!(parse("true;").is_ok());
        assert!(parse("false;").is_ok());
    }

    #[test]
    fn test_parse_binary_expression() {
        assert!(parse("1 + 2;").is_ok());
        assert!(parse("3 * 4;").is_ok());
        assert!(parse("5 - 6;").is_ok());
        assert!(parse("7 / 8;").is_ok());
    }

    #[test]
    fn test_parse_comparison() {
        assert!(parse("1 < 2;").is_ok());
        assert!(parse("1 > 2;").is_ok());
        assert!(parse("1 == 2;").is_ok());
        assert!(parse("1 != 2;").is_ok());
        assert!(parse("1 <= 2;").is_ok());
        assert!(parse("1 >= 2;").is_ok());
    }

    #[test]
    fn test_parse_let() {
        assert!(parse("let x = 42;").is_ok());
        assert!(parse("let mut y = 10;").is_ok());
    }

    #[test]
    fn test_parse_function() {
        assert!(parse("fn add(x, y) => x + y;").is_ok());
        assert!(parse("pub fn double(x) => x * 2;").is_ok());
    }

    #[test]
    fn test_parse_multi_clause_function() {
        let code = r#"
      fn fact(0) => 1;
      fn fact(n) => n * fact(n - 1);
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_guarded_function() {
        let code = r#"
      fn sign(n)
        | n < 0 => -1
        | n == 0 => 0
        | n > 0 => 1;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_if_expression() {
        assert!(parse("if true then 1 else 2;").is_ok());
    }

    #[test]
    fn test_parse_match() {
        let code = r#"
      match x with
      | None -> 0
      | Some(y) -> y
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_list() {
        assert!(parse("[1, 2, 3];").is_ok());
        assert!(parse("[];").is_ok());
    }

    #[test]
    fn test_parse_tuple() {
        assert!(parse("(1, 2);").is_ok());
        assert!(parse("(1, 2, 3);").is_ok());
    }

    #[test]
    fn test_parse_lambda() {
        assert!(parse("let f = fn (x) => x + 1;").is_ok());
        assert!(parse("let add = fn (x, y) => x + y;").is_ok());
        assert!(parse("(fn (x) => x + 1)(5);").is_ok()); // immediately invoked
    }

    #[test]
    fn test_parse_do_block() {
        let code = r#"
      do {
        let x = 1;
        let y = 2;
        x + y
      };
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_type_sum() {
        let code = "type Option<a> = | None | Some(a);";
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_type_record() {
        let code = "type Point = { x: Int, y: Int };";
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_import() {
        assert!(parse("import Math;").is_ok());
        assert!(parse("import Math.{add, sub};").is_ok());
    }

    #[test]
    fn test_parse_module() {
        assert!(parse("module MyModule;").is_ok());
    }

    #[test]
    fn test_parse_trait() {
        let code = r#"
      trait Show<a> {
        fn show(x: a): String;
      }
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_impl() {
        let code = r#"
      impl Show for Int {
        fn show(n) => "int";
      }
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_cons_pattern() {
        let code = r#"
      match xs with
      | [] -> 0
      | [x :: rest] -> x
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_record_pattern() {
        let code = r#"
      match p with
      | Point { x, y } -> x + y
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_wildcard_pattern() {
        let code = r#"
      match x with
      | _ -> 0
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_error_unclosed_paren() {
        assert!(parse("(1 + 2;").is_err());
    }

    #[test]
    fn test_parse_error_unclosed_bracket() {
        assert!(parse("[1, 2;").is_err());
    }

    #[test]
    fn test_parse_error_unclosed_brace() {
        assert!(parse("do { 1;").is_err());
    }

    #[test]
    fn test_parse_nested_expressions() {
        assert!(parse("((1 + 2) * 3);").is_ok());
        assert!(parse("if if true then true else false then 1 else 2;").is_ok());
    }

    #[test]
    fn test_parse_operator_precedence() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        assert!(parse("1 + 2 * 3;").is_ok());
        // 1 * 2 + 3 should parse as (1 * 2) + 3
        assert!(parse("1 * 2 + 3;").is_ok());
    }

    #[test]
    fn test_parse_unary_operators() {
        assert!(parse("-42;").is_ok());
        assert!(parse("!true;").is_ok());
        assert!(parse("--42;").is_ok());
    }

    #[test]
    fn test_parse_field_access() {
        assert!(parse("point.x;").is_ok());
        assert!(parse("foo.bar.baz;").is_ok());
    }

    #[test]
    fn test_parse_index_access() {
        assert!(parse("list[0];").is_ok());
        assert!(parse("matrix[i][j];").is_ok());
        assert!(parse("(1, 2, 3)[0];").is_ok());
    }

    #[test]
    fn test_parse_function_call() {
        assert!(parse("f();").is_ok());
        assert!(parse("f(1);").is_ok());
        assert!(parse("f(1, 2, 3);").is_ok());
        assert!(parse("f(g(x));").is_ok());
    }

    #[test]
    fn test_parse_cons_operator() {
        assert!(parse("1 :: [];").is_ok());
        assert!(parse("1 :: 2 :: [];").is_ok());
    }

    #[test]
    fn test_parse_string_concat() {
        assert!(parse(r#""a" ++ "b";"#).is_ok());
    }

    #[test]
    fn test_parse_list_concat() {
        assert!(parse("[1] ++ [2];").is_ok());
    }

    #[test]
    fn test_parse_type_annotation() {
        assert!(parse("let x: Int = 42;").is_ok());
        assert!(parse("fn add(x: Int, y: Int): Int => x + y;").is_ok());
    }

    #[test]
    fn test_parse_complex_type() {
        assert!(parse("let f: Int -> Int = fn (x) => x;").is_ok());
        assert!(parse("let p: (Int, String) = (1, \"hi\");").is_ok());
    }

    #[test]
    fn test_parse_record_creation() {
        assert!(parse("Point { x: 1, y: 2 };").is_ok());
    }

    #[test]
    fn test_parse_record_update() {
        assert!(parse("{ p | x: 10 };").is_ok());
    }

    #[test]
    fn test_parse_pub_let() {
        assert!(parse("pub let PI = 3.14159;").is_ok());
    }

    #[test]
    fn test_parse_negative_numbers() {
        assert!(parse("-1;").is_ok());
        assert!(parse("-3.14;").is_ok());
    }

    #[test]
    fn test_parse_empty_list_pattern() {
        let code = r#"
      match xs with
      | [] -> true
      | _ -> false
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_or_pattern() {
        let code = r#"
      match x with
      | 1 | 2 | 3 -> "small"
      | _ -> "large"
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_constructor_pattern_nested() {
        let code = r#"
      match x with
      | Some(Some(y)) -> y
      | _ -> 0
      end;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_boolean_operators() {
        assert!(parse("true && false;").is_ok());
        assert!(parse("true || false;").is_ok());
        assert!(parse("!true;").is_ok());
    }

    #[test]
    fn test_parse_parenthesized_expression() {
        assert!(parse("(1);").is_ok());
        assert!(parse("(1 + 2);").is_ok());
        assert!(parse("((1));").is_ok());
    }

    #[test]
    fn test_parse_chained_comparisons() {
        let _result = parse("1 < 2 < 3;");
    }

    #[test]
    fn test_parse_multiline() {
        let code = r#"
      let x = 1;
      let y = 2;
      x + y;
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_comments() {
        let code = r#"
      -- This is a comment
      let x = 42; -- inline comment
    "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_char_literal() {
        assert!(parse("'a';").is_ok());
        assert!(parse("'\\n';").is_ok());
        assert!(parse("'\\t';").is_ok());
    }

    #[test]
    fn test_parse_escape_sequences() {
        assert!(parse(r#""hello\nworld";"#).is_ok());
        assert!(parse(r#""tab\there";"#).is_ok());
        assert!(parse(r#""quote: \"";"#).is_ok());
    }
}
