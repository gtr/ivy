mod driver;
mod repl;

use std::env;
use std::process;
use std::thread::Builder;

const MB: usize = 1024 * 1024;

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

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut show_tree = false;
    let mut type_check = false;
    let mut no_prelude = false;
    let mut file_path: Option<String> = None;

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
                file_path = Some(arg.to_string());
            }
            arg => {
                eprintln!("Unknown option: {}", arg);
                print_usage();
                process::exit(1);
            }
        }
        i += 1;
    }

    // TODO(gtr): temporary until we build a vm interpreter
    let child = Builder::new()
        .stack_size(256 * MB)
        .spawn(move || match file_path {
            Some(path) => driver::run_file(&path, show_tree, type_check, no_prelude),
            None => repl::repl(no_prelude),
        })
        .expect("failed to spawn interpreter thread");
    child.join().expect("interpreter thread panicked");
}
