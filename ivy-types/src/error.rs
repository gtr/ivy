use crate::types::{Type, TypeVar};
use ivy_syntax::Span;
use std::error::Error;
use std::fmt;

/// A type error with source location.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

/// The kind of type error.
#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    /// Two types that should be equal but are not.
    Mismatch { expected: Type, found: Type },

    /// Infinite type detected during unification
    InfiniteType { var: TypeVar, ty: Type },

    /// Variable not found in scope.
    UndefinedVariable { name: String },

    /// Type not found in scope.
    UndefinedType { name: String },

    /// Constructor not found.
    UndefinedConstructor { name: String },

    /// Wrong number of arguments.
    ArityMismatch {
        name: String,
        expected: usize,
        found: usize,
    },

    /// Trying to call a non-function.
    NotCallable { ty: Type },

    /// Trying to access a field on a non-record.
    NotARecord { ty: Type },

    /// Field not found in record.
    UndefinedField { record: String, field: String },

    /// Trying to index a non-list/tuple.
    NotIndexable { ty: Type },

    /// Pattern doesn't match the type being matched.
    PatternMismatch { pattern: String, ty: Type },

    /// Non-exhaustive pattern match.
    NonExhaustive { missing: Vec<String> },

    /// Duplicate definition.
    DuplicateDefinition { name: String },

    /// Type annotation doesn't match inferred type.
    AnnotationMismatch { annotated: Type, inferred: Type },

    /// Record field count mismatch
    RecordFieldCount {
        record: String,
        expected: usize,
        found: usize,
    },

    /// Missing field in record literal
    MissingField { record: String, field: String },

    /// Module not found
    ModuleNotFound { module: String },

    /// Circular import detected
    CircularImport { module: String, cycle: Vec<String> },

    /// IO error reading module
    ModuleIOError { module: String, error: String },

    /// Parse error in module
    ModuleParseError { module: String, error: String },

    /// Type error in module
    ModuleTypeError {
        module: String,
        file_path: String,
        source: String,
        inner: Box<TypeError>,
    },
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, span: Span) -> TypeError {
        TypeError { kind, span }
    }

    pub fn mismatch(expected: Type, found: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::Mismatch { expected, found }, span)
    }

    pub fn infinite_type(var: TypeVar, ty: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::InfiniteType { var, ty }, span)
    }

    pub fn undefined_variable(name: &str, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::UndefinedVariable { name: name.to_string() }, span)
    }

    pub fn undefined_type(name: &str, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::UndefinedType { name: name.to_string() }, span)
    }

    pub fn undefined_constructor(name: &str, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::UndefinedConstructor { name: name.to_string() }, span)
    }

    pub fn arity_mismatch(name: &str, expected: usize, found: usize, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::ArityMismatch {
                name: name.to_string(),
                expected,
                found,
            },
            span,
        )
    }

    pub fn not_callable(ty: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::NotCallable { ty }, span)
    }

    pub fn not_a_record(ty: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::NotARecord { ty }, span)
    }

    pub fn undefined_field(record: &str, field: &str, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::UndefinedField {
                record: record.to_string(),
                field: field.to_string(),
            },
            span,
        )
    }

    pub fn not_indexable(ty: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::NotIndexable { ty }, span)
    }

    pub fn duplicate_definition(name: &str, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::DuplicateDefinition { name: name.to_string() }, span)
    }

    pub fn annotation_mismatch(annotated: Type, inferred: Type, span: Span) -> TypeError {
        TypeError::new(TypeErrorKind::AnnotationMismatch { annotated, inferred }, span)
    }

    pub fn record_field_count(record: &str, expected: usize, found: usize, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::RecordFieldCount {
                record: record.to_string(),
                expected,
                found,
            },
            span,
        )
    }

    pub fn missing_field(record: &str, field: &str, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::MissingField {
                record: record.to_string(),
                field: field.to_string(),
            },
            span,
        )
    }

    pub fn module_not_found(module: &str, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::ModuleNotFound {
                module: module.to_string(),
            },
            span,
        )
    }

    pub fn circular_import(module: &str, cycle: Vec<String>, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::CircularImport {
                module: module.to_string(),
                cycle,
            },
            span,
        )
    }

    pub fn module_io_error(module: &str, error: &str, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::ModuleIOError {
                module: module.to_string(),
                error: error.to_string(),
            },
            span,
        )
    }

    pub fn module_parse_error(module: &str, error: &str, span: Span) -> TypeError {
        TypeError::new(
            TypeErrorKind::ModuleParseError {
                module: module.to_string(),
                error: error.to_string(),
            },
            span,
        )
    }

    pub fn module_type_error(module: &str, file_path: &str, source: &str, inner: TypeError) -> TypeError {
        let span = inner.span;
        TypeError::new(
            TypeErrorKind::ModuleTypeError {
                module: module.to_string(),
                file_path: file_path.to_string(),
                source: source.to_string(),
                inner: Box::new(inner),
            },
            span,
        )
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeErrorKind::Mismatch { expected, found } => {
                write!(f, "type mismatch: expected {}, found {}", expected, found)
            }
            TypeErrorKind::InfiniteType { var, ty } => {
                write!(
                    f,
                    "infinite type: {} occurs in {} (cannot construct infinite type)",
                    var, ty
                )
            }
            TypeErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: {}", name)
            }
            TypeErrorKind::UndefinedType { name } => {
                write!(f, "undefined type: {}", name)
            }
            TypeErrorKind::UndefinedConstructor { name } => {
                write!(f, "undefined constructor: {}", name)
            }
            TypeErrorKind::ArityMismatch { name, expected, found } => {
                write!(
                    f,
                    "{} expects {} argument{}, but {} {} provided",
                    name,
                    expected,
                    if *expected == 1 { "" } else { "s" },
                    found,
                    if *found == 1 { "was" } else { "were" }
                )
            }
            TypeErrorKind::NotCallable { ty } => {
                write!(f, "type {} is not callable", ty)
            }
            TypeErrorKind::NotARecord { ty } => {
                write!(f, "type {} is not a record", ty)
            }
            TypeErrorKind::UndefinedField { record, field } => {
                write!(f, "record {} has no field {}", record, field)
            }
            TypeErrorKind::NotIndexable { ty } => {
                write!(f, "type {} cannot be indexed", ty)
            }
            TypeErrorKind::PatternMismatch { pattern, ty } => {
                write!(f, "pattern {} does not match type {}", pattern, ty)
            }
            TypeErrorKind::NonExhaustive { missing } => {
                write!(f, "non-exhaustive patterns, missing: {}", missing.join(", "))
            }
            TypeErrorKind::DuplicateDefinition { name } => {
                write!(f, "duplicate definition: {}", name)
            }
            TypeErrorKind::AnnotationMismatch { annotated, inferred } => {
                write!(
                    f,
                    "type annotation {} doesn't match inferred type {}",
                    annotated, inferred
                )
            }
            TypeErrorKind::RecordFieldCount {
                record,
                expected,
                found,
            } => {
                write!(
                    f,
                    "record `{}`  has {} field{}, but {} {} provided",
                    record,
                    expected,
                    if *expected == 1 { "" } else { "s" },
                    found,
                    if *found == 1 { "was" } else { "were" }
                )
            }
            TypeErrorKind::MissingField { record, field } => {
                write!(f, "missing field `{}` in record {}", field, record)
            }
            TypeErrorKind::ModuleNotFound { module } => {
                write!(f, "module not found: {}", module)
            }
            TypeErrorKind::CircularImport { module, cycle } => {
                write!(
                    f,
                    "circular import detected: {} (cycle: {} -> {})",
                    module,
                    cycle.join(" -> "),
                    module
                )
            }
            TypeErrorKind::ModuleIOError { module, error } => {
                write!(f, "error reading module {}: {}", module, error)
            }
            TypeErrorKind::ModuleParseError { module, error } => {
                write!(f, "parse error in module {}: {}", module, error)
            }
            TypeErrorKind::ModuleTypeError { module, inner, .. } => {
                write!(f, "type error in module {}: {}", module, inner)
            }
        }
    }
}

impl Error for TypeError {}
pub type TypeResult<T> = Result<T, TypeError>;
