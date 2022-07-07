mod lexer;
mod errors;
mod repl;
mod parser;

use std::env;
use std::fs;
use crate::lexer::lexer::*;
use crate::repl::repl::*;
use crate::parser::parser::*;

fn print_usage() {
    print!("Usage:\n ivy file [options]\n\n");
    print!(" file\t\tthe file containing ivy source code;\n");
    print!("\t\tstarts the ivy REPL if not specified\n");
    print!("Options:\n");
    print!(" -t, --tree\tprints a syntax tree of the source code\n");
    print!(" -h, --help\tprints this message\n\n");
}

fn open_file(file_name: &String) {
    let contents = fs::read_to_string(file_name).unwrap();

    let program = contents.trim_end();

    let token_result = lex(&program);
    if token_result.is_some() {
        parse(token_result.unwrap());
    }
}

fn main() {
    let mut args: Vec<String> = env::args().collect();
    let last = &args[args.len()-1];
    if last == "--tree" || last == "-t" {
        args.pop();
    } else if last == "--help" ||last == "-h" {
        print_usage();
        return;
    }
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        open_file(&args[1]);
    } else {
        print_usage();
    }
}
