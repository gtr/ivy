use crate::ast::Ident;
use crate::lit::Literal;
use crate::span::{Span, Spanned};

/// Pattern variants for matching.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Wildcard: _
    Wildcard,

    /// Literal pattern: 0, "hello", true
    Lit(Literal),

    /// Variable binding: x, name
    Var(Ident),

    /// Constructor pattern: Some(x), None
    Constructor { name: Ident, args: Vec<Spanned<Pattern>> },

    /// Tuple pattern: (a, b, c)
    Tuple { elements: Vec<Spanned<Pattern>> },

    /// List pattern: [a, b, c]
    List { elements: Vec<Spanned<Pattern>> },

    /// Cons pattern: head :: tail
    Cons {
        head: Box<Spanned<Pattern>>,
        tail: Box<Spanned<Pattern>>,
    },

    /// Record pattern: Person { name, age }
    Record { name: Ident, fields: Vec<FieldPattern> },

    /// Or pattern: pat1 | pat2
    Or {
        left: Box<Spanned<Pattern>>,
        right: Box<Spanned<Pattern>>,
    },
}

/// Field pattern: name or name: pattern
#[derive(Debug, Clone)]
pub struct FieldPattern {
    /// The field name.
    pub name: Ident,
    /// Optional pattern (if None, binds field name as variable).
    pub pattern: Option<Spanned<Pattern>>,
    /// The span of the field pattern.
    pub span: Span,
}

impl FieldPattern {
    /// Create a new field pattern with explicit pattern.
    pub fn new(name: Ident, pattern: Option<Spanned<Pattern>>, span: Span) -> Self {
        Self { name, pattern, span }
    }

    /// Create a simple field pattern that just binds the field name.
    pub fn simple(name: Ident) -> Self {
        let span = name.span;
        Self {
            name,
            pattern: None,
            span,
        }
    }
}
