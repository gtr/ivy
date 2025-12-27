//! Literal values.

/// Literal values in Ivy.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal: 42, -17
    Int(i64),
    /// Float literal: 3.14, -0.5
    Float(f64),
    /// String literal: "hello"
    String(String),
    /// Character literal: 'a', '\n'
    Char(char),
    /// Boolean literal: true, false
    Bool(bool),
    /// Unit literal: ()
    Unit,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(n) => write!(f, "{}", n),
            Literal::Float(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
        }
    }
}
