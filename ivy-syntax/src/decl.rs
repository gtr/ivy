use std::collections::HashSet;

use crate::ast::Ident;
use crate::expr::{Expr, Param};
use crate::pattern::{collect_pattern_names, Pattern};
use crate::span::{Span, Spanned};
use crate::types::TypeExpr;

/// Import kind for the different import syntaxes.
#[derive(Debug, Clone)]
pub enum ImportKind {
    Qualified,
    Alias(Ident),
    All,
    Items(Vec<Ident>),
}

/// Top-level declaration variants.
#[derive(Debug, Clone)]
pub enum Decl {
    Module {
        name: Ident,
    },

    /// Import declaration with Python-style syntax
    Import {
        path: Vec<Ident>,
        kind: ImportKind,
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
        is_pub: bool,
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
    /// Standalone type signature: `fn factorial :: Int -> Int` or `let x :: Int`
    TypeSig {
        is_pub: bool,
        name: Ident,
        ty: Spanned<TypeExpr>,
        span: Span,
    },
}

/// Function declaration.
#[derive(Debug, Clone)]
pub struct FnDecl {
    pub is_pub: bool,
    pub name: Ident,
    pub params: Vec<Param>,
    pub return_ty: Option<Spanned<TypeExpr>>,
    pub body: FnBody,
    pub span: Span,
}

impl FnDecl {
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
    pub guard: Spanned<Expr>,
    pub body: Spanned<Expr>,
    pub span: Span,
}

impl GuardedExpr {
    pub fn new(guard: Spanned<Expr>, body: Spanned<Expr>, span: Span) -> Self {
        Self { guard, body, span }
    }
}

/// Type body: sum type, record type, or alias.
#[derive(Debug, Clone)]
pub enum TypeBody {
    /// Sum type: | None | Some(a)
    Sum(Vec<Variant>),
    /// Record type: { name: String, age: Int }
    Record(Vec<RecordField>),
    /// Type alias: = SomeType
    Alias(Spanned<TypeExpr>),
}

/// Sum type variant: None or Some(a)
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Ident,
    pub fields: Vec<Spanned<TypeExpr>>,
    pub span: Span,
}

impl Variant {
    pub fn new(name: Ident, fields: Vec<Spanned<TypeExpr>>, span: Span) -> Self {
        Self { name, fields, span }
    }
}

/// Record field: name: Type
#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: Ident,
    pub ty: Spanned<TypeExpr>,
    pub span: Span,
}

impl RecordField {
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
    pub trait_name: Ident,
    pub type_arg: Ident,
    pub span: Span,
}

impl Constraint {
    pub fn new(trait_name: Ident, type_arg: Ident, span: Span) -> Self {
        Self {
            trait_name,
            type_arg,
            span,
        }
    }
}

/// Collect all public declaration names from a list of declarations.
pub fn collect_public_names(decls: &[Spanned<Decl>]) -> HashSet<String> {
    let mut names = HashSet::new();
    for decl in decls {
        match &decl.node {
            Decl::Fn(fn_decl) if fn_decl.is_pub => {
                names.insert(fn_decl.name.name.clone());
            }
            Decl::Let {
                is_pub: true, pattern, ..
            } => {
                collect_pattern_names(&pattern.node, &mut names);
            }
            Decl::Type {
                is_pub: true,
                name,
                body,
                ..
            } => {
                names.insert(name.name.clone());
                if let TypeBody::Sum(variants) = body {
                    for variant in variants {
                        names.insert(variant.name.name.clone());
                    }
                }
            }
            _ => {}
        }
    }
    names
}
