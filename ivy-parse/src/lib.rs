//! Ivy Parse
//!
//! Lexer and parser for the Ivy programming language

pub mod error;
pub mod lexer;
pub mod loader;
pub mod parser;
#[cfg(test)]
mod tests;
pub mod token;

pub use error::{ParseError, ParseResult};
pub use loader::{ModuleLoadError, ModuleLoader, ParsedModule};
pub use parser::parse;
