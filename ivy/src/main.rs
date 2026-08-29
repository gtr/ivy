use std::env;
use std::error::Error as StdError;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use miette::{Diagnostic, NamedSource};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use thiserror::Error;

use ivy_eval::{EvalError, Interpreter, Value};
use ivy_parse::ParseError;
use ivy_types::TypeError;

const GREEN: &str = "\x1b[32m";
const BLUE: &str = "\x1b[36m";
const RESET: &str = "\x1b[0m";

const PRELUDE_FILE: &str = "lib/prelude.ivy";

/// Wraps any `Diagnostic` error with the source code so miette can render
/// labels against it. The inner error provides its own `#[label]` attributes.
#[derive(Error, Debug, Diagnostic)]
#[error("{inner}")]
struct SourcedError<E: StdError + Diagnostic + 'static> {
    #[source_code]
    src: NamedSource<String>,
    #[diagnostic_source]
    #[source]
    inner: E,
}

impl<E: StdError + Diagnostic + 'static> SourcedError<E> {
    fn new(error: E, source: &str, filename: &str) -> Self {
        Self {
            src: NamedSource::new(filename, source.to_string()),
            inner: error,
        }
    }
}

fn print_parse_error(error: ParseError, source: &str, filename: &str) {
    eprintln!("{:?}", miette::Report::new(SourcedError::new(error, source, filename)));
}

fn print_eval_error(error: EvalError, source: &str, filename: &str) {
    eprintln!("{:?}", miette::Report::new(SourcedError::new(error, source, filename)));
}

fn print_type_error(error: TypeError, source: &str, filename: &str) {
    // For module type errors, render against the module's own source.
    if let TypeError::ModuleTypeError {
        file_path,
        module_source,
        inner,
        ..
    } = &error
    {
        let inner = inner.as_ref().clone();
        let file_path = file_path.clone();
        let module_source = module_source.clone();
        print_type_error(inner, &module_source, &file_path);
        return;
    }
    eprintln!("{:?}", miette::Report::new(SourcedError::new(error, source, filename)));
}

fn print_usage() {
    println!("Usage:");
    println!("  ivy <file>        Run an Ivy program");
    println!("  ivy               Start the Ivy REPL");
    println!();
    println!("Options:");
    println!("  -c, --check       Type check without running");
    println!("  -t, --tree        Print the syntax tree");
    println!("      --no-prelude  Do not auto-load the standard prelude");
    println!("  -h, --help        Print this help message");
}

fn check_file(path: &str, source: &str, no_prelude: bool) {
    match ivy_parse::parse(source) {
        Ok(program) => {
            let search_paths = build_search_paths(path);
            let mut loader = ivy_parse::ModuleLoader::new(search_paths);
            let mut type_checker = ivy_types::TypeChecker::new();
            let mut type_env = ivy_types::TypeEnv::with_builtins();
            let mut interp = Interpreter::with_builtins();
            if !no_prelude {
                load_prelude(&mut interp, &mut type_checker, &mut type_env, &mut loader);
            }

            match ivy_types::check_program_with_env(&program, &mut type_checker, &mut type_env, &mut loader) {
                Ok(()) => {
                    println!("{}OK{}: {} type checks successfully", GREEN, RESET, path);
                }
                Err(e) => {
                    print_type_error(e, source, path);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            print_parse_error(e, source, path);
            process::exit(1);
        }
    }
}

fn run_file(path: &str, show_tree: bool, type_check: bool, no_prelude: bool) {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path, e);
            process::exit(1);
        }
    };

    if type_check {
        check_file(path, &source, no_prelude);
        return;
    }

    match ivy_parse::parse(&source) {
        Ok(program) => {
            if show_tree {
                println!("{:#?}", program);
            } else {
                let search_paths = build_search_paths(path);
                let mut loader = ivy_parse::ModuleLoader::new(search_paths);
                let mut type_checker = ivy_types::TypeChecker::new();
                let mut type_env = ivy_types::TypeEnv::with_builtins();
                let mut interp = Interpreter::with_builtins();
                if !no_prelude {
                    load_prelude(&mut interp, &mut type_checker, &mut type_env, &mut loader);
                }

                match ivy_types::check_program_with_env(&program, &mut type_checker, &mut type_env, &mut loader) {
                    Ok(()) => match interp.eval_program_with_loader(&program, &mut loader) {
                        Ok(_) => {}
                        Err(e) => {
                            print_eval_error(e, &source, path);
                            process::exit(1);
                        }
                    },
                    Err(e) => {
                        print_type_error(e, &source, path);
                        process::exit(1);
                    }
                }
            }
        }
        Err(e) => {
            print_parse_error(e, &source, path);
            process::exit(1);
        }
    }
}

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

/// Build search paths for a file, prepending its parent directory.
fn build_search_paths(path: &str) -> Vec<PathBuf> {
    let mut search_paths = ivy_utils::get_default_search_paths();
    if let Some(parent) = Path::new(path).parent() {
        if let Ok(abs_parent) = fs::canonicalize(parent) {
            search_paths.insert(0, abs_parent);
        }
    }
    search_paths
}

/// Load the prelude into both the type checker and evaluator (parsed once).
fn load_prelude(
    interp: &mut Interpreter,
    type_checker: &mut ivy_types::TypeChecker,
    type_env: &mut ivy_types::TypeEnv,
    loader: &mut ivy_parse::ModuleLoader,
) {
    let prelude_paths = [
        env::current_dir().ok().map(|d| d.join(PRELUDE_FILE)),
        env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|d| d.join(PRELUDE_FILE))),
        env::current_exe()
            .ok()
            .and_then(|p| p.parent().and_then(|d| d.parent().map(|d| d.join(PRELUDE_FILE)))),
    ];
    for path in prelude_paths.into_iter().flatten() {
        if path.exists() {
            if let Ok(source) = fs::read_to_string(&path) {
                if let Ok(program) = ivy_parse::parse(&source) {
                    let _ = ivy_types::check_program_with_env(&program, type_checker, type_env, loader);
                    interp.load_program(&program);
                }
            }
            break;
        }
    }
}

fn repl(no_prelude: bool) {
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

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut show_tree = false;
    let mut type_check = false;
    let mut no_prelude = false;
    let mut file_path: Option<&str> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_usage();
                return;
            }
            "-t" | "--tree" => {
                show_tree = true;
            }
            "-c" | "--check" => {
                type_check = true;
            }
            "--no-prelude" => {
                no_prelude = true;
            }
            arg if !arg.starts_with('-') => {
                file_path = Some(arg);
            }
            arg => {
                eprintln!("Unknown option: {}", arg);
                print_usage();
                process::exit(1);
            }
        }
        i += 1;
    }

    match file_path {
        Some(path) => run_file(path, show_tree, type_check, no_prelude),
        None => repl(no_prelude),
    }
}
