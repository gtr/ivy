use std::env;
use std::error::Error as StdError;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use miette::{Diagnostic, NamedSource, Report};
use thiserror::Error;

use ivy_eval::{EvalError, Interpreter};
use ivy_parse::ParseError;
use ivy_syntax::{hints, Program};
use ivy_types::{unreachable_clause_warnings, TypeChecker, TypeError};

pub(crate) const GREEN: &str = "\x1b[32m";
pub(crate) const BLUE: &str = "\x1b[36m";
pub(crate) const RESET: &str = "\x1b[0m";

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

pub(crate) fn print_parse_error(error: ParseError, source: &str, filename: &str) {
    eprintln!("{:?}", miette::Report::new(SourcedError::new(error, source, filename)));
}

pub(crate) fn print_eval_error(error: EvalError, source: &str, filename: &str) {
    eprintln!("{:?}", miette::Report::new(SourcedError::new(error, source, filename)));
}

pub(crate) fn print_warnings(program: &Program, checker: &TypeChecker, source: &str, filename: &str) {
    for warning in unreachable_clause_warnings(&program.declarations, &checker.registry) {
        eprintln!("{:?}", Report::new(SourcedError::new(warning, source, filename)));
    }
}

pub(crate) fn print_type_error(error: TypeError, source: &str, filename: &str) {
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
                    print_warnings(&program, &type_checker, source, path);
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

pub(crate) fn run_file(path: &str, show_tree: bool, type_check: bool, no_prelude: bool) {
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
                    Ok(()) => {
                        print_warnings(&program, &type_checker, &source, path);
                        match interp.eval_program_with_loader(&program, &mut loader) {
                            Ok(_) => {}
                            Err(e) => {
                                print_eval_error(e, &source, path);
                                process::exit(1);
                            }
                        }
                    }
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

/// Build search paths for a file, prepending its parent directory.
fn build_search_paths(path: &str) -> Vec<PathBuf> {
    let mut search_paths = ivy_utils::get_default_search_paths();
    if let Some(dir) = file_dir(path) {
        search_paths.insert(0, dir);
    }
    search_paths
}

pub(crate) fn file_dir(path: &str) -> Option<PathBuf> {
    fs::canonicalize(Path::new(path).parent()?).ok()
}

/// Load the prelude into both the type checker and evaluator (parsed once).
pub(crate) fn load_prelude(
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
    let Some(path) = prelude_paths.into_iter().flatten().find(|path| path.exists()) else {
        eprintln!("{}", hints::missing_prelude(PRELUDE_FILE));
        return;
    };
    let filename = path.display().to_string();
    let Ok(source) = fs::read_to_string(&path) else {
        eprintln!("warning: couldn't read the prelude at {filename}");
        return;
    };
    match ivy_parse::parse(&source) {
        Ok(program) => {
            if let Err(error) = ivy_types::check_program_with_env(&program, type_checker, type_env, loader) {
                print_type_error(error, &source, &filename);
            }
            interp.load_program(&program);
        }
        Err(error) => print_parse_error(error, &source, &filename),
    }
}
