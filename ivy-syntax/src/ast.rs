//! Core AST types.

use crate::decl::Decl;
use crate::span::{Span, Spanned};

/// A complete Ivy program.
#[derive(Debug, Clone)]
pub struct Program {
    /// The top-level declarations.
    pub declarations: Vec<Spanned<Decl>>,
    /// The span covering the entire program.
    pub span: Span,
}

impl Program {
    /// Create a new program.
    pub fn new(declarations: Vec<Spanned<Decl>>, span: Span) -> Self {
        Self { declarations, span }
    }

    /// Create an empty program.
    pub fn empty() -> Self {
        Self {
            declarations: Vec::new(),
            span: Span::point(0),
        }
    }
}

/// An identifier (variable or type name).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    /// The identifier name.
    pub name: String,
    /// The source location.
    pub span: Span,
}

impl Ident {
    /// Create a new identifier.
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }

    /// Check if this identifier starts with an uppercase letter (type/constructor).
    pub fn is_type_name(&self) -> bool {
        self.name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
    }

    /// Check if this identifier starts with a lowercase letter (value).
    pub fn is_value_name(&self) -> bool {
        self.name
            .chars()
            .next()
            .map(|c| c.is_lowercase() || c == '_')
            .unwrap_or(false)
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
