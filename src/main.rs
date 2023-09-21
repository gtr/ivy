mod lexer;
mod errors;
mod repl;
mod parser;
mod ivm;

use std::env;
use std::fs;
use crate::lexer::lexer::*;
use crate::repl::repl::*;
use crate::parser::parser::*;
use crate::parser::ast::*;
use crate::parser::printer::*;

fn print_usage() {
  print!("Usage:\n ivy file [options]\n\n");
  println!(" file\t\tthe file containing ivy source code;");
  println!("\t\tstarts the ivy REPL if not specified");
  println!("Options:");
  println!(" -t, --tree\tprints a syntax tree of the source code");
  print!(" -h, --help\tprints this message\n\n");
}

fn open_file(file_name: &String) {
  let contents = fs::read_to_string(file_name).unwrap();
  let program = contents.trim_end();
  let token_result = lex(program, file_name);

  if token_result.is_some() {
    let tokens = token_result.unwrap();
    // for token in tokens {
    //     println!("{} ", token);
    // }
    // let r = parse(token_result.unwrap());
    
    let ast = parse(tokens);
    match ast {
      Err(err) => {err.show_error(&contents, file_name);}
      Ok(root) => {print_tree(root);}
    }
  }
}

fn main() {
  let mut args: Vec<String> = env::args().collect();
  let last = &args[args.len() - 1];
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
