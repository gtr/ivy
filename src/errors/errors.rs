use std::fmt;
use colored::Colorize;

pub enum IvyError {
    LexerError(String),
}

impl fmt::Display for IvyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            IvyError::LexerError(s) => {
                let msg = "Lexer error:".red();
                write!(f, "{} {}", msg, s)
            },
        }
    }
}