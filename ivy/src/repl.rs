use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::history::DefaultHistory;
use rustyline::validate::Validator;
use rustyline::{Context, Editor, Helper};

use ivy_eval::{Interpreter, Value};
use ivy_parse::{ModuleLoader, ParseError};
use ivy_syntax::{collect_pattern_names, Decl, Program};
use ivy_types::{TypeChecker, TypeEnv};

use crate::driver::{
    file_dir, load_prelude, print_eval_error, print_parse_error, print_type_error, print_warnings, BLUE, GREEN, RESET,
};

#[derive(Clone, Copy)]
enum Command {
    Help,
    Quit,
    Load,
    Reload,
    Reset,
    Type,
    Env,
}

struct CommandInfo {
    command: Command,
    names: &'static [&'static str],
    argument: &'static str,
    help: &'static str,
}

const COMMANDS: &[CommandInfo] = &[
    CommandInfo {
        command: Command::Help,
        names: &[":help", ":h"],
        argument: "",
        help: "Show this help message",
    },
    CommandInfo {
        command: Command::Quit,
        names: &[":quit", ":q"],
        argument: "",
        help: "Exit the REPL",
    },
    CommandInfo {
        command: Command::Load,
        names: &[":load", ":l"],
        argument: "<file>",
        help: "Load a file into the session",
    },
    CommandInfo {
        command: Command::Reload,
        names: &[":reload", ":r!"],
        argument: "",
        help: "Reset, then reload the last loaded file",
    },
    CommandInfo {
        command: Command::Reset,
        names: &[":reset", ":r", ":clear"],
        argument: "",
        help: "Reset session state",
    },
    CommandInfo {
        command: Command::Type,
        names: &[":type", ":t"],
        argument: "<expr>",
        help: "Show the inferred type of an expression",
    },
    CommandInfo {
        command: Command::Env,
        names: &[":env"],
        argument: "",
        help: "Show all defined names and their types",
    },
];

impl Command {
    fn find(name: &str) -> Option<Command> {
        COMMANDS
            .iter()
            .find(|info| info.names.contains(&name))
            .map(|info| info.command)
    }
}

fn print_repl_help() {
    println!("Ivy REPL Commands:");
    println!();
    for info in COMMANDS {
        let mut usage = format!("{} {}", info.names[0], info.argument).trim_end().to_string();
        for alias in &info.names[1..] {
            usage.push_str(", ");
            usage.push_str(alias);
        }
        println!("  {usage:<24} {}", info.help);
    }
    println!();
    println!("Tab auto-completes commands, in-scope names, and :load paths. History");
    println!("persists in ~/.ivy_history. Multi-line input continues with `...>`.");
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

struct ReplState {
    interp: Interpreter,
    loader: ModuleLoader,
    type_checker: TypeChecker,
    type_env: TypeEnv,
}

impl ReplState {
    fn new(no_prelude: bool) -> Self {
        let mut state = ReplState {
            interp: Interpreter::with_builtins(),
            loader: ModuleLoader::new(ivy_utils::get_default_search_paths()),
            type_checker: TypeChecker::new(),
            type_env: TypeEnv::with_builtins(),
        };
        if !no_prelude {
            load_prelude(
                &mut state.interp,
                &mut state.type_checker,
                &mut state.type_env,
                &mut state.loader,
            );
        }
        state
    }

    fn typecheck_and_evaluate(&mut self, program: &Program, source: &str, name: &str) -> bool {
        match ivy_types::check_program_with_env(program, &mut self.type_checker, &mut self.type_env, &mut self.loader) {
            Ok(()) => {
                print_warnings(program, &self.type_checker, source, name);
                match self.interp.eval_program_with_loader(program, &mut self.loader) {
                    Ok(value) => {
                        if !matches!(value, Value::Unit) {
                            println!("{:?}", value);
                        }
                        true
                    }
                    Err(e) => {
                        print_eval_error(e, source, name);
                        false
                    }
                }
            }
            Err(e) => {
                print_type_error(e, source, name);
                false
            }
        }
    }

    fn load_file(&mut self, path: &str) {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading file '{}': {}", path, e);
                return;
            }
        };
        if let Some(dir) = file_dir(path) {
            self.loader.prepend_search_path(dir);
        }
        match ivy_parse::parse(&source) {
            Ok(program) => {
                if self.typecheck_and_evaluate(&program, &source, path) {
                    let count = definition_names(&program).len();
                    let plural = if count == 1 { "" } else { "s" };
                    println!("Loaded {}{}{} ({} definition{})", GREEN, path, RESET, count, plural);
                }
            }
            Err(e) => print_parse_error(e, &source, path),
        }
    }
}

fn definition_names(program: &Program) -> HashSet<String> {
    let mut names = HashSet::new();
    for decl in &program.declarations {
        match &decl.node {
            Decl::Fn(fn_decl) => {
                names.insert(fn_decl.name.name.clone());
            }
            Decl::Type { name, .. } | Decl::Trait { name, .. } => {
                names.insert(name.name.clone());
            }
            Decl::Let { pattern, .. } => collect_pattern_names(&pattern.node, &mut names),
            _ => {}
        }
    }
    names
}

fn completion_names(interp: &Interpreter, type_env: &ivy_types::TypeEnv) -> Vec<String> {
    let mut names: Vec<String> = type_env
        .binding_names()
        .filter(|n| !n.starts_with("__"))
        .map(str::to_string)
        .collect();
    for (module, exports) in interp.list_module_exports() {
        for export in exports {
            names.push(format!("{}.{}", module, export));
        }
    }
    names
}

fn scheme_string(scheme: &ivy_types::Scheme) -> String {
    let norm = scheme.normalize();
    let mut rendered = norm.ty.to_string();
    let bounds: Vec<String> = norm
        .constraints
        .iter()
        .filter(|c| c.type_args.iter().any(|t| !t.free_vars().is_empty()))
        .map(ToString::to_string)
        .collect();
    if !bounds.is_empty() {
        rendered.push_str(&format!(" where {}", bounds.join(", ")));
    }
    rendered
}

fn print_env(interp: &Interpreter, type_env: &ivy_types::TypeEnv) {
    let mut rows: Vec<(String, String)> = Vec::new();
    for name in interp.list_bindings().into_iter().filter(|n| !n.starts_with("__")) {
        if let Some(scheme) = type_env.get(&name) {
            rows.push((name, scheme_string(scheme)));
        }
    }
    for (module, exports) in interp.list_module_exports() {
        for export in exports {
            let ty = type_env
                .get_module_export(&module, &export)
                .map_or_else(|| "?".to_string(), scheme_string);
            rows.push((format!("{}.{}", module, export), ty));
        }
    }
    rows.sort();

    if rows.is_empty() {
        println!("(no user-defined bindings)");
        return;
    }
    let width = rows.iter().map(|(name, _)| name.len()).max().unwrap_or(0);
    println!("Defined symbols:");
    for (name, ty) in rows {
        println!(
            "  {}{:<width$}{} :: {}{}{}",
            BLUE,
            name,
            RESET,
            GREEN,
            ty,
            RESET,
            width = width
        );
    }
}

pub(crate) fn repl(no_prelude: bool) {
    print_greeting();

    let mut rl: Editor<IvyHelper, DefaultHistory> = match Editor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("Failed to initialize REPL: {}", e);
            process::exit(1);
        }
    };
    rl.set_helper(Some(IvyHelper::new()));

    let history_path = env::var_os("HOME").map(|home| Path::new(&home).join(".ivy_history"));
    if let Some(path) = &history_path {
        let _ = rl.load_history(path);
    }

    let mut state = ReplState::new(no_prelude);

    let mut input_buffer = String::new();
    let mut continuation = false;
    let mut last_loaded: Option<String> = None;

    loop {
        if let Some(helper) = rl.helper_mut() {
            helper.set_names(completion_names(&state.interp, &state.type_env));
        }

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

                    match Command::find(cmd) {
                        Some(Command::Quit) => break,

                        Some(Command::Help) => {
                            print_repl_help();
                        }

                        Some(Command::Reset) => {
                            state = ReplState::new(no_prelude);
                            println!("Session state reset.");
                        }

                        Some(Command::Load) => match arg {
                            Some(path) => {
                                last_loaded = Some(path.to_string());
                                state.load_file(path);
                            }
                            None => eprintln!("Usage: :load <file>"),
                        },

                        Some(Command::Reload) => match last_loaded.clone() {
                            Some(path) => {
                                state = ReplState::new(no_prelude);
                                state.load_file(&path);
                            }
                            None => eprintln!("Nothing to reload yet, use :load <file> first."),
                        },

                        Some(Command::Env) => print_env(&state.interp, &state.type_env),

                        Some(Command::Type) => {
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
                                            let base = state.type_checker.constraints.len();
                                            let result = state.type_checker.infer(expr, &state.type_env);
                                            // Take the constraints this inference introduced, even on failure, so
                                            // `:type` doesn't pollute session state
                                            let raw: Vec<ivy_types::TraitConstraint> =
                                                state.type_checker.constraints.drain(base..).map(|(c, _)| c).collect();
                                            match result {
                                                Ok(ty) => {
                                                    let final_ty = state.type_checker.finalize(&ty);
                                                    let constraints = raw
                                                        .iter()
                                                        .map(|c| ivy_types::TraitConstraint {
                                                            trait_name: c.trait_name.clone(),
                                                            type_args: c
                                                                .type_args
                                                                .iter()
                                                                .map(|t| state.type_checker.subst.apply(t))
                                                                .collect(),
                                                        })
                                                        .collect();
                                                    let scheme = ivy_types::Scheme::poly_with_constraints(
                                                        vec![],
                                                        constraints,
                                                        final_ty,
                                                    );
                                                    println!(
                                                        "{}{}{} :: {}{}{}",
                                                        BLUE,
                                                        expr_str,
                                                        RESET,
                                                        GREEN,
                                                        scheme_string(&scheme),
                                                        RESET
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
                                eprintln!("Usage: :type <expr>");
                            }
                        }

                        None => {
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

                        state.typecheck_and_evaluate(&program, &source_for_errors, "<repl>");
                    }
                    Err(e) => {
                        if is_incomplete_input(&input_buffer, &e) {
                            continuation = true;
                        } else {
                            print_parse_error(e, &source_for_errors, "<repl>");
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

    if let Some(path) = &history_path {
        let _ = rl.save_history(path);
    }
}

struct IvyHelper {
    names: Vec<String>,
    filename: FilenameCompleter,
}

impl IvyHelper {
    fn new() -> Self {
        Self {
            names: Vec::new(),
            filename: FilenameCompleter::new(),
        }
    }

    fn set_names(&mut self, mut names: Vec<String>) {
        names.sort();
        names.dedup();
        self.names = names;
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '?' || c == '.'
}

impl Completer for IvyHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, ctx: &Context<'_>) -> rustyline::Result<(usize, Vec<Pair>)> {
        let prefix = &line[..pos];
        let trimmed = prefix.trim_start();

        if let Some((command, _)) = trimmed.split_once(' ') {
            if matches!(Command::find(command), Some(Command::Load)) {
                return self.filename.complete(line, pos, ctx);
            }
        }

        if trimmed.starts_with(':') && !trimmed.contains(' ') {
            let start = prefix.rfind(':').unwrap_or(0);
            let word = &prefix[start..];
            let candidates = COMMANDS
                .iter()
                .map(|info| info.names[0])
                .filter(|name| name.starts_with(word))
                .map(|name| Pair {
                    display: name.to_string(),
                    replacement: name.to_string(),
                })
                .collect();
            return Ok((start, candidates));
        }

        let start = prefix.rfind(|c: char| !is_ident_char(c)).map_or(0, |i| i + 1);
        let word = &prefix[start..];
        if word.is_empty() {
            return Ok((pos, Vec::new()));
        }
        let candidates = self
            .names
            .iter()
            .filter(|n| n.starts_with(word))
            .map(|n| Pair {
                display: n.clone(),
                replacement: n.clone(),
            })
            .collect();
        Ok((start, candidates))
    }
}

// rustyline requires these...
// TODO(gtr): maybe use rustyline's `derive` feature to genereate these

impl Hinter for IvyHelper {
    type Hint = String;
}

impl Highlighter for IvyHelper {}
impl Validator for IvyHelper {}

impl Helper for IvyHelper {}
