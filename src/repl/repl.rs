
use crate::lexer::lexer::*;
use crate::parser::parser::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;

const PROMPT: &str = "\x1b[92mivy>\x1b[0m ";
const VERSION: &str = "v0.2.0";

fn print_repl_message() {
    print!("The ivy programming language ({})\n", VERSION);
	print!("enter (quit) to quit.\n");
}

pub fn repl() {
    print_repl_message();

    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {};

    loop {
        println!();
        let readline = rl.readline(PROMPT);

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line == "(quit)" {
                    return;
                }
            
                let token_result = lex(&line);
                if token_result.is_some() {
                    parse(token_result.unwrap());
                }
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof)=> {
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
