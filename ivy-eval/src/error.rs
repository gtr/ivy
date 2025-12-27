//! Error types for evaluation.

use ivy_syntax::Span;
use miette::Diagnostic;
use thiserror::Error;

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Error, Debug, Diagnostic)]
pub enum EvalError {
    #[error("Undefined variable: {name}")]
    #[diagnostic(code(ivy::eval::undefined_variable))]
    UndefinedVariable {
        name: String,
        #[label("not found in scope")]
        span: Span,
    },

    #[error("Cannot assign to immutable binding: {name}")]
    #[diagnostic(code(ivy::eval::immutable_assignment))]
    ImmutableAssignment {
        name: String,
        #[label("this binding is immutable")]
        span: Span,
    },

    #[error("Type error: expected {expected}, found {found}")]
    #[diagnostic(code(ivy::eval::type_error))]
    TypeError {
        expected: String,
        found: String,
        #[label("expected {expected}")]
        span: Span,
    },

    #[error("Pattern match failed: no matching clause")]
    #[diagnostic(code(ivy::eval::match_failed))]
    MatchFailed {
        #[label("no pattern matched this value")]
        span: Span,
    },

    #[error("Arity mismatch: expected {expected} arguments, got {got}")]
    #[diagnostic(code(ivy::eval::arity_mismatch))]
    ArityMismatch {
        expected: usize,
        got: usize,
        #[label("wrong number of arguments")]
        span: Span,
    },

    #[error("Not callable: {value_type}")]
    #[diagnostic(code(ivy::eval::not_callable))]
    NotCallable {
        value_type: String,
        #[label("cannot call this value")]
        span: Span,
    },

    #[error("Division by zero")]
    #[diagnostic(code(ivy::eval::division_by_zero))]
    DivisionByZero {
        #[label("division by zero")]
        span: Span,
    },

    #[error("Index out of bounds: {index} (length {length})")]
    #[diagnostic(code(ivy::eval::index_out_of_bounds))]
    IndexOutOfBounds {
        index: i64,
        length: usize,
        #[label("index out of bounds")]
        span: Span,
    },

    #[error("Unknown field: {field} on type {type_name}")]
    #[diagnostic(code(ivy::eval::unknown_field))]
    UnknownField {
        type_name: String,
        field: String,
        #[label("field not found")]
        span: Span,
    },

    #[error("Unknown constructor: {name}")]
    #[diagnostic(code(ivy::eval::unknown_constructor))]
    UnknownConstructor {
        name: String,
        #[label("constructor not defined")]
        span: Span,
    },

    #[error("Module load error: {message}")]
    #[diagnostic(code(ivy::eval::module_error))]
    ModuleError {
        message: String,
        #[label("error loading module")]
        span: Span,
    },

    #[error("Private item: {name} is not public in module {module}")]
    #[diagnostic(code(ivy::eval::private_item))]
    PrivateItem {
        name: String,
        module: String,
        #[label("not accessible")]
        span: Span,
    },

    #[error("Undefined module: {name}")]
    #[diagnostic(code(ivy::eval::undefined_module))]
    UndefinedModule {
        name: String,
        #[label("module not found")]
        span: Span,
    },
}
