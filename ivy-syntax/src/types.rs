//! Type expression types.

use crate::ast::Ident;
use crate::span::Spanned;

/// Type expression variants.
#[derive(Debug, Clone)]
pub enum TypeExpr {
    /// Named type: Int, String, Person
    Named(Ident),

    /// Type variable: a, b, T
    Var(Ident),

    /// Function type: Int -> String
    Function {
        param: Box<Spanned<TypeExpr>>,
        result: Box<Spanned<TypeExpr>>,
    },

    /// Generic application: Option<Int>, Result<T, E>
    Apply { base: Ident, args: Vec<Spanned<TypeExpr>> },

    /// Tuple type: (Int, String)
    Tuple { elements: Vec<Spanned<TypeExpr>> },

    /// List type: [Int]
    List { element: Box<Spanned<TypeExpr>> },

    /// Unit type: ()
    Unit,
}
