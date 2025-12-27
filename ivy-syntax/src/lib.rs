//! Ivy Syntax
//!
//! This crate defines the Abstract Syntax Tree (AST) types for the Ivy
//! programming language. just pure data types

pub mod ast;
pub mod decl;
pub mod expr;
pub mod lit;
pub mod op;
pub mod pattern;
pub mod span;
pub mod types;

pub use ast::{Ident, Program};
pub use decl::{Decl, FnBody, FnDecl, GuardedExpr, RecordField, TypeBody, Variant};
pub use expr::{Expr, FieldInit, MatchArm, Param};
pub use lit::Literal;
pub use op::{BinOp, UnaryOp};
pub use pattern::{FieldPattern, Pattern};
pub use span::{Span, Spanned};
pub use types::TypeExpr;
