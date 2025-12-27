//! Expression types.

use crate::ast::Ident;
use crate::lit::Literal;
use crate::op::{BinOp, UnaryOp};
use crate::pattern::Pattern;
use crate::span::{Span, Spanned};
use crate::types::TypeExpr;

/// Expression variants
#[derive(Debug, Clone)]
pub enum Expr {
    /// Literal value: 42, "hello", true, ()
    Lit(Literal),

    /// Variable reference: x, myFunc
    Var(Ident),

    /// Binary operation: a + b, x && y
    Binary {
        left: Box<Spanned<Expr>>,
        op: Spanned<BinOp>,
        right: Box<Spanned<Expr>>,
    },

    /// Unary operation: -x, !flag
    Unary {
        op: Spanned<UnaryOp>,
        operand: Box<Spanned<Expr>>,
    },

    /// Let binding (expression form): let x = 1
    Let {
        is_mut: bool,
        pattern: Box<Spanned<Pattern>>,
        ty: Option<Box<Spanned<TypeExpr>>>,
        value: Box<Spanned<Expr>>,
    },

    /// Assignment: x = 42
    Assign {
        target: Box<Spanned<Expr>>,
        value: Box<Spanned<Expr>>,
    },

    /// If expression: if cond then a else b
    If {
        condition: Box<Spanned<Expr>>,
        then_branch: Box<Spanned<Expr>>,
        else_branch: Box<Spanned<Expr>>,
    },

    /// Match expression: match x with | pat -> expr end
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },

    /// Anonymous function: fn (x, y) => x + y
    Lambda {
        params: Vec<Param>,
        return_ty: Option<Box<Spanned<TypeExpr>>>,
        body: Box<Spanned<Expr>>,
    },

    /// Function call: f(x, y)
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },

    /// Field access: obj.field
    Field { object: Box<Spanned<Expr>>, field: Ident },

    /// Index access: arr[i]
    Index {
        object: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },

    /// Do block: do { stmt; stmt; expr }
    Do { body: Vec<Spanned<Expr>> },

    /// Tuple: (a, b, c)
    Tuple { elements: Vec<Spanned<Expr>> },

    /// List literal: [1, 2, 3]
    List { elements: Vec<Spanned<Expr>> },

    /// Record construction: Person { name: "Alice", age: 30 }
    Record { name: Ident, fields: Vec<FieldInit> },

    /// Record update: { person | age: 31 }
    RecordUpdate {
        base: Box<Spanned<Expr>>,
        updates: Vec<FieldInit>,
    },

    /// Grouped expression: (expr) - for precedence
    Paren { inner: Box<Spanned<Expr>> },
}

/// A match arm: | pattern -> expr
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// The pattern to match.
    pub pattern: Spanned<Pattern>,
    /// The body expression.
    pub body: Spanned<Expr>,
    /// The span of the entire arm.
    pub span: Span,
}

impl MatchArm {
    /// Create a new match arm.
    pub fn new(pattern: Spanned<Pattern>, body: Spanned<Expr>, span: Span) -> Self {
        Self { pattern, body, span }
    }
}

/// Function parameter with optional type.
#[derive(Debug, Clone)]
pub struct Param {
    /// The parameter pattern (usually just a name, but can be a pattern).
    pub pattern: Spanned<Pattern>,
    /// Optional type annotation.
    pub ty: Option<Spanned<TypeExpr>>,
    /// The span of the parameter.
    pub span: Span,
}

impl Param {
    /// Create a new parameter
    pub fn new(pattern: Spanned<Pattern>, ty: Option<Spanned<TypeExpr>>, span: Span) -> Self {
        Self { pattern, ty, span }
    }

    /// Create a simple named parameter without type
    pub fn named(name: Ident) -> Self {
        let span = name.span;
        Self {
            pattern: Spanned::new(Pattern::Var(name), span),
            ty: None,
            span,
        }
    }
}

/// Field initializer: name: expr
#[derive(Debug, Clone)]
pub struct FieldInit {
    /// The field name.
    pub name: Ident,
    /// The field value.
    pub value: Spanned<Expr>,
    /// The span of the entire initializer
    pub span: Span,
}

impl FieldInit {
    /// Create a new field initializer
    pub fn new(name: Ident, value: Spanned<Expr>, span: Span) -> Self {
        Self { name, value, span }
    }
}
