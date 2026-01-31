//! Ivy CLI
//!
//! Command-line interface for the Ivy programming language. It's actually a REPL (read, print,
//! evaluate, loop) to be fair. Also contains logic for errors and REPL commands.

use std::env;
use std::fs;
use std::path::Path;

use miette::{Diagnostic, NamedSource, SourceSpan};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use thiserror::Error;

use ivy_eval::{EvalError, Interpreter, Value};
use ivy_parse::ParseError;
use ivy_types::TypeError;

const GREEN: &str = "\x1b[32m";
const BLUE: &str = "\x1b[36m";
const RESET: &str = "\x1b[0m";

/// Wrapper for ParseError that includes source code for miette pretty printing.
#[derive(Error, Debug, Diagnostic)]
#[error("{inner}")]
struct SourcedParseError {
    #[source_code]
    src: NamedSource<String>,
    #[label("{}", label_text)]
    span: SourceSpan,
    label_text: String,
    inner: ParseError,
}

impl SourcedParseError {
    fn new(error: ParseError, source: &str, filename: &str) -> Self {
        let (span, label_text) = match &error {
            ParseError::UnexpectedToken { span, found, .. } => (
                (span.start, span.end - span.start).into(),
                format!("found {} here", found),
            ),
            ParseError::UnexpectedEof { span, expected, .. } => (
                (span.start, span.end.saturating_sub(span.start).max(1)).into(),
                format!("expected {}", expected),
            ),
            ParseError::Unterminated { start, .. } => (
                (start.start, start.end - start.start).into(),
                "started here".to_string(),
            ),
            ParseError::InvalidEscape { span, .. } => {
                ((span.start, span.end - span.start).into(), "invalid escape".to_string())
            }
            ParseError::InvalidNumber { span } => {
                ((span.start, span.end - span.start).into(), "invalid number".to_string())
            }
            ParseError::InvalidChar { span } => (
                (span.start, span.end - span.start).into(),
                "invalid character".to_string(),
            ),
            ParseError::UnexpectedChar { span, .. } => (
                (span.start, span.end - span.start).into(),
                "unexpected character".to_string(),
            ),
            ParseError::InvalidPattern { span } => (
                (span.start, span.end - span.start).into(),
                "not a valid pattern".to_string(),
            ),
        };
        Self {
            src: NamedSource::new(filename, source.to_string()),
            span,
            label_text,
            inner: error,
        }
    }
}

/// Wrapper for EvalError that includes source code for pretty printing.
#[derive(Error, Debug, Diagnostic)]
#[error("{inner}")]
struct SourcedEvalError {
    #[source_code]
    src: NamedSource<String>,
    #[label("{}", label_text)]
    span: SourceSpan,
    label_text: String,
    inner: EvalError,
}

impl SourcedEvalError {
    fn new(error: EvalError, source: &str, filename: &str) -> Self {
        let (span, label_text) = match &error {
            EvalError::UndefinedVariable { span, .. } => (
                (span.start, span.end - span.start).into(),
                "not found in scope".to_string(),
            ),
            EvalError::ImmutableAssignment { span, .. } => (
                (span.start, span.end - span.start).into(),
                "binding is immutable".to_string(),
            ),
            EvalError::TypeError { span, expected, .. } => (
                (span.start, span.end - span.start).into(),
                format!("expected {}", expected),
            ),
            EvalError::MatchFailed { span } => (
                (span.start, span.end - span.start).into(),
                "no pattern matched".to_string(),
            ),
            EvalError::ArityMismatch { span, expected, got } => (
                (span.start, span.end - span.start).into(),
                format!("expected {} args, got {}", expected, got),
            ),
            EvalError::NotCallable { span, .. } => (
                (span.start, span.end - span.start).into(),
                "cannot call this value".to_string(),
            ),
            EvalError::DivisionByZero { span } => (
                (span.start, span.end - span.start).into(),
                "division by zero".to_string(),
            ),
            EvalError::IndexOutOfBounds { span, index, length } => (
                (span.start, span.end - span.start).into(),
                format!("index {} out of bounds (len {})", index, length),
            ),
            EvalError::UnknownField { span, field, .. } => (
                (span.start, span.end - span.start).into(),
                format!("field '{}' not found", field),
            ),
            EvalError::UnknownConstructor { span, .. } => (
                (span.start, span.end - span.start).into(),
                "constructor not defined".to_string(),
            ),
            EvalError::ModuleError { span, .. } => {
                ((span.start, span.end - span.start).into(), "module error".to_string())
            }
            EvalError::PrivateItem { span, .. } => {
                ((span.start, span.end - span.start).into(), "not accessible".to_string())
            }
            EvalError::UndefinedModule { span, .. } => (
                (span.start, span.end - span.start).into(),
                "module not found".to_string(),
            ),
        };
        Self {
            src: NamedSource::new(filename, source.to_string()),
            span,
            label_text,
            inner: error,
        }
    }
}

/// Wrapper for TypeError that includes source code for pretty printing.
#[derive(Error, Debug, Diagnostic)]
#[error("{inner}")]
struct SourcedTypeError {
    #[source_code]
    src: NamedSource<String>,
    #[label("{}", label_text)]
    span: SourceSpan,
    label_text: String,
    inner: TypeError,
}

impl SourcedTypeError {
    fn new(error: TypeError, source: &str, filename: &str) -> Self {
        use ivy_types::TypeErrorKind;
        let span_range = error.span;
        let label_text = match &error.kind {
            TypeErrorKind::Mismatch { expected, found } => {
                format!("expected {}, found {}", expected, found)
            }
            TypeErrorKind::InfiniteType { .. } => "infinite type".to_string(),
            TypeErrorKind::UndefinedVariable { name } => format!("'{}' not found", name),
            TypeErrorKind::UndefinedType { name } => format!("type '{}' not found", name),
            TypeErrorKind::UndefinedConstructor { name } => {
                format!("constructor '{}' not found", name)
            }
            TypeErrorKind::ArityMismatch { expected, found, .. } => {
                format!("expected {} args, got {}", expected, found)
            }
            TypeErrorKind::NotCallable { ty } => format!("'{}' is not callable", ty),
            TypeErrorKind::NotARecord { ty } => format!("'{}' is not a record", ty),
            TypeErrorKind::UndefinedField { field, .. } => format!("field '{}' not found", field),
            TypeErrorKind::NotIndexable { ty } => format!("'{}' cannot be indexed", ty),
            TypeErrorKind::PatternMismatch { .. } => "pattern mismatch".to_string(),
            TypeErrorKind::NonExhaustive { .. } => "non-exhaustive patterns".to_string(),
            TypeErrorKind::DuplicateDefinition { name } => format!("'{}' already defined", name),
            TypeErrorKind::AnnotationMismatch { annotated, inferred } => {
                format!("annotation {} doesn't match inferred {}", annotated, inferred)
            }
            TypeErrorKind::RecordFieldCount { expected, found, .. } => {
                format!("expected {} field(s), found {}", expected, found)
            }
            TypeErrorKind::MissingField { field, .. } => {
                format!("missing field `{}`", field)
            }
        };
        Self {
            src: NamedSource::new(filename, source.to_string()),
            span: (span_range.start, span_range.end - span_range.start).into(),
            label_text,
            inner: error,
        }
    }
}

fn print_parse_error(error: ParseError, source: &str, filename: &str) {
    let sourced = SourcedParseError::new(error, source, filename);
    eprintln!("{:?}", miette::Report::new(sourced));
}

fn print_eval_error(error: EvalError, source: &str, filename: &str) {
    let sourced = SourcedEvalError::new(error, source, filename);
    eprintln!("{:?}", miette::Report::new(sourced));
}

fn print_type_error(error: TypeError, source: &str, filename: &str) {
    let sourced = SourcedTypeError::new(error, source, filename);
    eprintln!("{:?}", miette::Report::new(sourced));
}

fn print_usage() {
    println!("Usage:");
    println!("  ivy <file>      Run an Ivy program");
    println!("  ivy             Start the Ivy REPL");
    println!();
    println!("Options:");
    println!("  -c, --check     Type check without running");
    println!("  -t, --tree      Print the syntax tree");
    println!("  -h, --help      Print this help message");
}

fn check_file(path: &str, source: &str) {
    match ivy_parse::parse(source) {
        Ok(program) => match ivy_types::check_program(&program) {
            Ok(()) => {
                println!("{}OK{}: {} type checks successfully", GREEN, RESET, path);
            }
            Err(e) => {
                print_type_error(e, source, path);
                std::process::exit(1);
            }
        },
        Err(e) => {
            print_parse_error(e, source, path);
            std::process::exit(1);
        }
    }
}

fn run_file(path: &str, show_tree: bool, type_check: bool) {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path, e);
            std::process::exit(1);
        }
    };

    if type_check {
        check_file(path, &source);
        return;
    }

    match ivy_parse::parse(&source) {
        Ok(program) => {
            if show_tree {
                println!("{:#?}", program);
            } else {
                // Type check first
                match ivy_types::check_program(&program) {
                    Ok(()) => {
                        let mut interp = Interpreter::new();

                        if let Some(parent) = Path::new(path).parent() {
                            if let Ok(abs_parent) = fs::canonicalize(parent) {
                                interp.add_search_path(abs_parent);
                            }
                        }

                        match interp.eval_program(&program) {
                            Ok(_) => {}
                            Err(e) => {
                                print_eval_error(e, &source, path);
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(e) => {
                        print_type_error(e, &source, path);
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(e) => {
            print_parse_error(e, &source, path);
            std::process::exit(1);
        }
    }
}

fn print_repl_help() {
    println!("Ivy REPL Commands:");
    println!();
    println!("  :help, :h          Show this help message");
    println!("  :quit, :q          Exit the REPL");
    println!("  :reset, :r         Reset interpreter state (clear all definitions)");
    println!("  :load <path>, :l   Load and execute a file");
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
    println!(r"          |___/  v0.2");
    println!("{}", RESET);
    println!("Ivy - the friendly functional programming language");
    println!(
        "Type {}:help{} for commands, {}:q{} to quit\n",
        GREEN, RESET, GREEN, RESET
    );
}

/// Load prelude types into the type environment.
fn load_prelude_types(type_checker: &mut ivy_types::TypeChecker, type_env: &mut ivy_types::TypeEnv) {
    let prelude_paths = [
        env::current_dir().ok().map(|d| d.join("lib/prelude.ivy")),
        env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|d| d.join("lib/prelude.ivy"))),
        env::current_exe()
            .ok()
            .and_then(|p| p.parent().and_then(|d| d.parent().map(|d| d.join("lib/prelude.ivy")))),
    ];
    for path_opt in prelude_paths {
        if let Some(path) = path_opt {
            if path.exists() {
                if let Ok(source) = fs::read_to_string(&path) {
                    if let Ok(program) = ivy_parse::parse(&source) {
                        let _ = ivy_types::check_program_with_env(&program, type_checker, type_env);
                    }
                }
                break;
            }
        }
    }
}

fn repl() {
    print_greeting();

    let mut rl = match DefaultEditor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("Failed to initialize REPL: {}", e);
            std::process::exit(1);
        }
    };

    let mut interp = Interpreter::new();
    let mut type_checker = ivy_types::TypeChecker::new();
    let mut type_env = ivy_types::TypeEnv::with_builtins();
    load_prelude_types(&mut type_checker, &mut type_env);

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
                            interp = Interpreter::new();
                            type_checker = ivy_types::TypeChecker::new();
                            type_env = ivy_types::TypeEnv::with_builtins();
                            load_prelude_types(&mut type_checker, &mut type_env);
                            println!("Interpreter state reset.");
                        }

                        ":l" | ":load" => {
                            if let Some(path) = arg {
                                match fs::read_to_string(path) {
                                    Ok(source) => match ivy_parse::parse(&source) {
                                        Ok(program) => match ivy_types::check_program(&program) {
                                            Ok(()) => match interp.eval_program(&program) {
                                                Ok(_) => {
                                                    println!("Loaded '{}'", path);
                                                }
                                                Err(e) => {
                                                    print_eval_error(e, &source, path);
                                                }
                                            },
                                            Err(e) => {
                                                print_type_error(e, &source, path);
                                            }
                                        },
                                        Err(e) => {
                                            print_parse_error(e, &source, path);
                                        }
                                    },
                                    Err(e) => {
                                        eprintln!("Error reading '{}': {}", path, e);
                                    }
                                }
                            } else {
                                eprintln!("Usage: :load <path>");
                            }
                        }

                        ":env" => {
                            let bindings = interp.list_bindings();
                            if bindings.is_empty() {
                                println!("(no user-defined bindings)");
                            } else {
                                println!("Defined names:");
                                for name in bindings {
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

                // Keep a copy of the source for error reporting
                let source_for_errors = input_buffer.clone();

                match result {
                    Ok(program) => {
                        continuation = false;
                        input_buffer.clear();

                        match ivy_types::check_program_with_env(&program, &mut type_checker, &mut type_env) {
                            Ok(()) => match interp.eval_program(&program) {
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
            arg if !arg.starts_with('-') => {
                file_path = Some(arg);
            }
            arg => {
                eprintln!("Unknown option: {}", arg);
                print_usage();
                std::process::exit(1);
            }
        }
        i += 1;
    }

    match file_path {
        Some(path) => run_file(path, show_tree, type_check),
        None => repl(),
    }
}
