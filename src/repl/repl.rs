
use crate::lexer::lexer::*;
use crate::parser::parser::*;
use crate::parser::ast::*;

use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline::error::ReadlineError;

const INDENT: &str = "  ";
const PROMPT: &str = "\x1b[92mivy>\x1b[0m ";
const VERSION: &str = "v0.2.0";
const REPL: &str = "REPL";

const REPT_MSG: &str = "repeat the last command";
const LOAD_MSG: &str = "loads an ivy package to the REPL";
const LIST_MSG: &str = "displays this list of commands";
const TREE_MSG: &str = "displays the abstract syntax tree for <expr>";
const TYPE_MSG: &str = "displays the type of <expr>";
const EXPR_MSG: &str = "evaluates <expr>";

fn print_repl_message() {
    println!("The ivy programming language ({})", VERSION);
	println!("enter :quit to quit.");
}

/// Handles commands of the form of `:cmd`. These are mainly having to do with 
/// modifying or viewing the REPL environment.
fn handle_command(cmd: String) {
    match cmd.as_str() {
        ":l" | ":list" => {
            println!("List of commands available from the Ivy REPL:\n");
            println!("{INDENT}:                 {REPT_MSG}");
            println!("{INDENT}:list             {LIST_MSG}");
            println!("{INDENT}:load <package>   {LOAD_MSG}");
            println!("{INDENT}:tree <expr>      {TREE_MSG}");
            println!("{INDENT}:type <expr>      {TYPE_MSG}");
            println!("");
            println!("{INDENT}<expr>            {EXPR_MSG}");
        },
        ":t" | ":type"  => {
            println!("{TYPE_MSG}");
        },
        ":load" => {
            println!("{LOAD_MSG}");
        },
        ":" => {
            println!("{REPT_MSG}");
        },
        _ => {
            println!("Unrecognized command. Enter `:list` for a list of available commands.");
        }
    }
}

pub fn repl() {
    print_repl_message();

    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {};

    rl.bind_sequence(
        KeyEvent(KeyCode::Tab, Modifiers::NONE),
        EventHandler::Simple(Cmd::Insert(1, "\t".to_string()))
    );
    
    loop {
        let readline = rl.readline(PROMPT);

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line == ":quit" {
                    return;
                }

                if line.chars().next().unwrap() == ':' {
                    handle_command(line);
                } else {
                    let token_result = lex(&line, REPL);
                    if token_result.is_some() {
                        let tokens = token_result.unwrap();
                        // for token in tokens {    
                        //     println!("{} ", token);
                        // }
                        let ast = parse(tokens);
                        match ast {
                            Err(err) => {err.show_error(&line, REPL);}
                            Ok(root) => {print_tree(root);}
                        }
                    }
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
