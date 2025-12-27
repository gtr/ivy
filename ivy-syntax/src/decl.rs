//! Declaration types.

use crate::ast::Ident;
use crate::expr::{Expr, Param};
use crate::pattern::Pattern;
use crate::span::{Span, Spanned};
use crate::types::TypeExpr;

/// Top-level declaration variants.
#[derive(Debug, Clone)]
pub enum Decl {
    /// Module declaration: `module Math;`, for example
    Module { name: Ident },

    /// Import: `import Math;` or `import Math.{add, sub};`
    Import {
        path: Vec<Ident>,
        /// None = import all, Some = selective import
        items: Option<Vec<Ident>>,
    },

    /// Type definition: `type Option<a> = | None | Some(a);`
    Type {
        is_pub: bool,
        name: Ident,
        params: Vec<Ident>,
        body: TypeBody,
    },

    /// Trait definition: `trait Show<a> { ... }`
    Trait {
        name: Ident,
        param: Ident,
        items: Vec<TraitItem>,
    },

    /// Implementation: `impl Show for Int { ... }`
    Impl {
        trait_name: Ident,
        for_type: Spanned<TypeExpr>,
        where_clause: Vec<Constraint>,
        methods: Vec<Spanned<FnDecl>>,
    },

    /// Let binding (top-level): `let x = 42;`
    Let {
        is_pub: bool,
        is_mut: bool,
        pattern: Spanned<Pattern>,
        ty: Option<Spanned<TypeExpr>>,
        value: Spanned<Expr>,
    },

    /// Function declaration: `fn add(x, y) => x + y;`
    Fn(FnDecl),
}

/// Function declaration.
#[derive(Debug, Clone)]
pub struct FnDecl {
    /// Whether this function is public.
    pub is_pub: bool,
    /// The function name.
    pub name: Ident,
    /// The function parameters.
    pub params: Vec<Param>,
    /// Optional return type annotation.
    pub return_ty: Option<Spanned<TypeExpr>>,
    /// The function body.
    pub body: FnBody,
    /// The span of the entire declaration.
    pub span: Span,
}

impl FnDecl {
    /// Create a new function declaration.
    pub fn new(
        is_pub: bool,
        name: Ident,
        params: Vec<Param>,
        return_ty: Option<Spanned<TypeExpr>>,
        body: FnBody,
        span: Span,
    ) -> Self {
        Self {
            is_pub,
            name,
            params,
            return_ty,
            body,
            span,
        }
    }
}

/// Function body - either simple expression or guarded clauses.
#[derive(Debug, Clone)]
pub enum FnBody {
    /// Simple: => expr
    Expr(Spanned<Expr>),
    /// Guarded: | cond1 => expr1 | cond2 => expr2
    Guards(Vec<GuardedExpr>),
}

/// A guarded expression: | condition => result
#[derive(Debug, Clone)]
pub struct GuardedExpr {
    /// The guard condition.
    pub guard: Spanned<Expr>,
    /// The body expression.
    pub body: Spanned<Expr>,
    /// The span of the entire guarded expression.
    pub span: Span,
}

impl GuardedExpr {
    /// Create a new guarded expression.
    pub fn new(guard: Spanned<Expr>, body: Spanned<Expr>, span: Span) -> Self {
        Self { guard, body, span }
    }
}

/// Type body - sum type or record type.
#[derive(Debug, Clone)]
pub enum TypeBody {
    /// Sum type: | None | Some(a)
    Sum(Vec<Variant>),
    /// Record type: { name: String, age: Int }
    Record(Vec<RecordField>),
}

/// Sum type variant: None or Some(a)
#[derive(Debug, Clone)]
pub struct Variant {
    /// The variant name.
    pub name: Ident,
    /// The variant fields (empty for unit variants).
    pub fields: Vec<Spanned<TypeExpr>>,
    /// The span of the variant.
    pub span: Span,
}

impl Variant {
    /// Create a new variant.
    pub fn new(name: Ident, fields: Vec<Spanned<TypeExpr>>, span: Span) -> Self {
        Self { name, fields, span }
    }

    /// Create a unit variant (no fields).
    pub fn unit(name: Ident, span: Span) -> Self {
        Self {
            name,
            fields: Vec::new(),
            span,
        }
    }
}

/// Record field: name: Type
#[derive(Debug, Clone)]
pub struct RecordField {
    /// The field name.
    pub name: Ident,
    /// The field type.
    pub ty: Spanned<TypeExpr>,
    /// The span of the field.
    pub span: Span,
}

impl RecordField {
    /// Create a new record field.
    pub fn new(name: Ident, ty: Spanned<TypeExpr>, span: Span) -> Self {
        Self { name, ty, span }
    }
}

/// Trait item: signature or default impl.
/// TODO(gtr): not fully fleshed out yet
#[derive(Debug, Clone)]
pub enum TraitItem {
    Signature {
        name: Ident,
        ty: Spanned<TypeExpr>,
        span: Span,
    },
    DefaultImpl(FnDecl),
}

/// Type constraint: Show<a>
#[derive(Debug, Clone)]
pub struct Constraint {
    /// The trait name.
    pub trait_name: Ident,
    /// The type argument.
    pub type_arg: Ident,
    /// The span of the constraint.
    pub span: Span,
}

impl Constraint {
    /// Create a new constraint.
    pub fn new(trait_name: Ident, type_arg: Ident, span: Span) -> Self {
        Self {
            trait_name,
            type_arg,
            span,
        }
    }
}
