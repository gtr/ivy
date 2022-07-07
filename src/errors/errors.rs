use std::fmt;
use colored::Colorize;

pub enum IvyError {
    LexerError(String),
    ParserError(String),
}

impl fmt::Display for IvyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            IvyError::LexerError(s) => write!(f, "{} {}", "Lexer error:".red(), s),
            IvyError::ParserError(s) => write!(f, "{} {}", "Parser error:".red(), s),
        }
    }
}