//! Ivy Eval
//!
//! Tree-walking interpreter for Ivy.

mod builtins;
mod env;
mod error;
mod eval;
mod loader;
mod pattern;
#[cfg(test)]
mod tests;
mod value;

pub use env::Env;
pub use error::{EvalError, EvalResult};
pub use eval::Interpreter;
pub use loader::{LoadError, Module, ModuleLoader};
pub use value::Value;
