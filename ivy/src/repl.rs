use std::process;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use ivy_eval::{Interpreter, Value};
use ivy_parse::ParseError;

use crate::driver::{load_prelude, print_eval_error, print_parse_error, print_type_error, BLUE, GREEN, RESET};

fn print_repl_help() {
    println!("Ivy REPL Commands:");
    println!();
    println!("  :help, :h          Show this help message");
    println!("  :quit, :q          Exit the REPL");
    println!("  :reset, :r         Reset interpreter state (clear all definitions)");
    println!("  :type <expr>, :t   Show the inferred type of an expression");
    println!("  :env               Show all defined names in current scope");
    println!();
    println!("Multi-line input is supported. The REPL will continue prompting");
    println!("with '...>' when it detects unclosed brackets or expressions.");
}

/// Check if input is incomplete (has unclosed delimiters or ends with continuation).
/// TODO(gtr): this can def be done better
fn is_incomplete_input(input: &str, error: &ParseError) -> bool {
    match error {
        ParseError::UnexpectedEof { .. } => true,
        ParseError::Unterminated { .. } => true,
        _ => {
            let mut paren_depth = 0i32;
            let mut bracket_depth = 0i32;
            let mut brace_depth = 0i32;
            let mut in_string = false;
            let mut prev_char = '\0';

            for ch in input.chars() {
                if in_string {
                    if ch == '"' && prev_char != '\\' {
                        in_string = false;
                    }
                } else {
                    match ch {
                        '"' => in_string = true,
                        '(' => paren_depth += 1,
                        ')' => paren_depth -= 1,
                        '[' => bracket_depth += 1,
                        ']' => bracket_depth -= 1,
                        '{' => brace_depth += 1,
                        '}' => brace_depth -= 1,
                        _ => {}
                    }
                }
                prev_char = ch;
            }

            paren_depth > 0 || bracket_depth > 0 || brace_depth > 0 || in_string
        }
    }
}

fn print_greeting() {
    print!("{}", GREEN);
    println!(r"  _");
    println!(r" (_)_   ___   _");
    println!(r" | \ \ / / | | |");
    println!(r" | |\ V /| |_| |");
    println!(r" |_| \_/  \__, |");
    println!(r"          |___/ ");
    println!("{}", RESET);
    println!("Ivy - the friendly functional programming language");
    println!(
        "Type {}:help{} for commands, {}:q{} to quit\n",
        GREEN, RESET, GREEN, RESET
    );
}

pub(crate) fn repl(no_prelude: bool) {
    print_greeting();

    let mut rl = match DefaultEditor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("Failed to initialize REPL: {}", e);
            process::exit(1);
        }
    };

    let mut interp = Interpreter::with_builtins();
    let mut loader = ivy_parse::ModuleLoader::new(ivy_utils::get_default_search_paths());
    let mut type_checker = ivy_types::TypeChecker::new();
    let mut type_env = ivy_types::TypeEnv::with_builtins();
    if !no_prelude {
        load_prelude(&mut interp, &mut type_checker, &mut type_env, &mut loader);
    }

    let mut input_buffer = String::new();
    let mut continuation = false;

    loop {
        let prompt = if continuation {
            format!("{}...>{} ", GREEN, RESET)
        } else {
            format!("{}ivy>{} ", GREEN, RESET)
        };

        match rl.readline(&prompt) {
            Ok(line) => {
                let line_trimmed = line.trim();

                if line_trimmed.is_empty() {
                    if continuation {
                        // Empty line in multi-line mode... try to parse what we have
                        input_buffer.push('\n');
                    }
                    continue;
                }

                if !continuation && line_trimmed.starts_with(':') {
                    let _ = rl.add_history_entry(line_trimmed);

                    let parts: Vec<&str> = line_trimmed.splitn(2, ' ').collect();
                    let cmd = parts[0];
                    let arg = parts.get(1).map(|s| s.trim());

                    match cmd {
                        ":q" | ":quit" => break,

                        ":h" | ":help" => {
                            print_repl_help();
                        }

                        ":r" | ":reset" => {
                            interp = Interpreter::with_builtins();
                            loader = ivy_parse::ModuleLoader::new(ivy_utils::get_default_search_paths());
                            type_checker = ivy_types::TypeChecker::new();
                            type_env = ivy_types::TypeEnv::with_builtins();
                            if !no_prelude {
                                load_prelude(&mut interp, &mut type_checker, &mut type_env, &mut loader);
                            }
                            println!("Interpreter state reset.");
                        }

                        ":env" => {
                            let mut all_names: Vec<String> = interp
                                .list_bindings()
                                .into_iter()
                                .filter(|name| !name.starts_with("__"))
                                .collect();
                            for (module_name, exports) in interp.list_module_exports() {
                                for export_name in exports {
                                    all_names.push(format!("{}.{}", module_name, export_name));
                                }
                            }

                            all_names.sort();

                            if all_names.is_empty() {
                                println!("(no user-defined bindings)");
                            } else {
                                println!("Defined names:");
                                for name in all_names {
                                    println!("  {}", name);
                                }
                            }
                        }

                        ":t" | ":type" => {
                            if let Some(expr_str) = arg {
                                let parse_result =
                                    ivy_parse::parse(expr_str).or_else(|_| ivy_parse::parse(&format!("{};", expr_str)));

                                match parse_result {
                                    Ok(program) if !program.declarations.is_empty() => {
                                        let decl = &program.declarations[0];

                                        use ivy_syntax::Decl;
                                        let expr = match &decl.node {
                                            Decl::Let { value, .. } => Some(value),
                                            _ => None,
                                        };

                                        if let Some(expr) = expr {
                                            match type_checker.infer(expr, &type_env) {
                                                Ok(ty) => {
                                                    let final_ty = type_checker.finalize(&ty).normalize();
                                                    println!(
                                                        "{}{}{} :: {}{}{}",
                                                        BLUE, expr_str, RESET, GREEN, final_ty, RESET
                                                    );
                                                }
                                                Err(e) => {
                                                    print_type_error(e, expr_str, "<repl>");
                                                }
                                            }
                                        } else {
                                            eprintln!("Expected expression, not declaration");
                                        }
                                    }
                                    Err(e) => {
                                        print_parse_error(e, expr_str, "<repl>");
                                    }
                                    _ => {
                                        eprintln!("Failed to parse expression");
                                    }
                                }
                            } else {
                                eprintln!("Usage: :type <expression>");
                            }
                        }

                        _ => {
                            eprintln!("Unknown command: {}. Type :help for available commands.", cmd);
                        }
                    }
                    continue;
                }

                if continuation {
                    input_buffer.push('\n');
                    input_buffer.push_str(&line);
                } else {
                    input_buffer = line.clone();
                }

                let _ = rl.add_history_entry(&input_buffer);
                let result =
                    ivy_parse::parse(&input_buffer).or_else(|_| ivy_parse::parse(&format!("{};", input_buffer)));
                let source_for_errors = input_buffer.clone();

                match result {
                    Ok(program) => {
                        continuation = false;
                        input_buffer.clear();

                        match ivy_types::check_program_with_env(&program, &mut type_checker, &mut type_env, &mut loader)
                        {
                            Ok(()) => match interp.eval_program_with_loader(&program, &mut loader) {
                                Ok(value) => {
                                    if !matches!(value, Value::Unit) {
                                        println!("{:?}", value);
                                    }
                                }
                                Err(e) => {
                                    print_eval_error(e, &source_for_errors, "repl");
                                }
                            },
                            Err(e) => {
                                print_type_error(e, &source_for_errors, "repl");
                            }
                        }
                    }
                    Err(e) => {
                        if is_incomplete_input(&input_buffer, &e) {
                            continuation = true;
                        } else {
                            print_parse_error(e, &source_for_errors, "repl");
                            continuation = false;
                            input_buffer.clear();
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continuation = false;
                input_buffer.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }
}
