mod lexer;
mod errors;

use lexer::lexer::*;
use std::io;
use std::io::Write;
use std::env;
use colored::Colorize;

const PROMPT: &str = "ivy> ";

fn repl() {
    print_repl_message();

    loop {
        print!("{}", PROMPT.green().bold());
        
        let mut buffer = String::new();
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                let program = buffer.trim_end();
                if program == "(quit)" {
                    return;
                }
            
                let token_result = lex(&program);
            
                print_tokens(token_result);
            }
            Err(error) => println!("error: {error}"),
        }
    }
}

fn print_usage() {
    print!("Usage:\n ivy file [options]\n\n");
    print!(" file\t\tthe file containing ivy source code;\n");
    print!("\t\tstarts the ivy REPL if not specified\n");
    print!("Options:\n");
    print!(" -t, --tree\tprints a syntax tree of the source code\n");
    print!(" -h, --help\tprints this message\n\n");
}

fn print_repl_message() {
    print!("The ivy programming language (v0.1)\n");
	print!("enter (quit) to quit.\n\n");
}

fn main() {
    let mut args: Vec<String> = env::args().collect();
    // let mut tree_flag = false;
    let last = &args[args.len()-1];
    if last == "--tree" || last == "-t" {
        // tree_flag = true;
        args.pop();
    } else if last == "--help" ||last == "-h" {
        print_usage();
        return;
    }
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        // Unimplemented.
    } else {
        print_usage();
    }
}
