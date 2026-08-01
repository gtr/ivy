use crate::types::{Type, TypeVar};
use ivy_syntax::Span;
use miette::Diagnostic;
use thiserror::Error;

pub type TypeResult<T> = Result<T, TypeError>;

/// Backwards-compatible alias for use sites that pattern-match on a `kind`.
/// Prefer matching on `TypeError` variants directly.
pub type TypeErrorKind = TypeError;

#[derive(Error, Debug, Clone, Diagnostic)]
pub enum TypeError {
    #[error("type mismatch: expected {expected}, found {found}")]
    #[diagnostic(code(ivy::types::mismatch))]
    Mismatch {
        expected: Type,
        found: Type,
        #[label(primary, "this has type {found}")]
        span: Span,
        #[label("expected {expected} because of this")]
        expected_span: Option<Span>,
    },

    #[error("infinite type: {var} occurs in {ty}")]
    #[diagnostic(code(ivy::types::infinite_type))]
    InfiniteType {
        var: TypeVar,
        ty: Type,
        #[label("infinite type")]
        span: Span,
    },

    #[error("undefined variable: {name}")]
    #[diagnostic(
        code(ivy::types::undefined_variable),
        help("is `{name}` defined in scope? check imports and spelling")
    )]
    UndefinedVariable {
        name: String,
        #[label("'{name}' not found")]
        span: Span,
    },

    #[error("undefined type: {name}")]
    #[diagnostic(code(ivy::types::undefined_type))]
    UndefinedType {
        name: String,
        #[label("type '{name}' not found")]
        span: Span,
    },

    #[error("undefined constructor: {name}")]
    #[diagnostic(code(ivy::types::undefined_constructor))]
    UndefinedConstructor {
        name: String,
        #[label("constructor '{name}' not found")]
        span: Span,
    },

    #[error("{name} expects {expected} argument{}, but {found} {} provided", plural(*expected), was_were(*found))]
    #[diagnostic(code(ivy::types::arity_mismatch))]
    ArityMismatch {
        name: String,
        expected: usize,
        found: usize,
        #[label("expected {expected} args, got {found}")]
        span: Span,
    },

    #[error("type {ty} is not callable")]
    #[diagnostic(
        code(ivy::types::not_callable),
        help("only functions can be called; values of type `{ty}` are not")
    )]
    NotCallable {
        ty: Type,
        #[label("cannot be called")]
        span: Span,
    },

    #[error("type {ty} is not a record")]
    #[diagnostic(code(ivy::types::not_a_record))]
    NotARecord {
        ty: Type,
        #[label("not a record")]
        span: Span,
    },

    #[error("record {record} has no field {field}")]
    #[diagnostic(code(ivy::types::undefined_field))]
    UndefinedField {
        record: String,
        field: String,
        #[label("field '{field}' not found")]
        span: Span,
    },

    #[error("type {ty} cannot be indexed")]
    #[diagnostic(code(ivy::types::not_indexable))]
    NotIndexable {
        ty: Type,
        #[label("not indexable")]
        span: Span,
    },

    #[error("pattern {pattern} does not match type {ty}")]
    #[diagnostic(code(ivy::types::pattern_mismatch))]
    PatternMismatch {
        pattern: String,
        ty: Type,
        #[label("pattern doesn't match {ty}")]
        span: Span,
    },

    #[error("or-pattern alternatives must bind the same names")]
    #[diagnostic(
        code(ivy::types::or_pattern_binding_mismatch),
        help("each side of `pat1 | pat2` must bind the same set of identifiers")
    )]
    OrPatternBindingMismatch {
        #[label("alternatives bind different names")]
        span: Span,
    },

    #[error("non-exhaustive patterns, missing: {}", missing.join(", "))]
    #[diagnostic(
        code(ivy::types::non_exhaustive),
        help("add arms for: {}, or use `_` to catch all", missing.join(", "))
    )]
    NonExhaustive {
        missing: Vec<String>,
        #[label("this value can be {}", missing.join(", "))]
        span: Span,
    },

    #[error("duplicate definition: {name}")]
    #[diagnostic(code(ivy::types::duplicate_definition))]
    DuplicateDefinition {
        name: String,
        #[label("'{name}' already defined")]
        span: Span,
    },

    #[error("type annotation {annotated} doesn't match inferred type {inferred}")]
    #[diagnostic(code(ivy::types::annotation_mismatch))]
    AnnotationMismatch {
        annotated: Type,
        inferred: Type,
        #[label("annotated as {annotated}, inferred {inferred}")]
        span: Span,
    },

    #[error("record `{record}` has {expected} field{}, but {found} {} provided", plural(*expected), was_were(*found))]
    #[diagnostic(code(ivy::types::record_field_count))]
    RecordFieldCount {
        record: String,
        expected: usize,
        found: usize,
        #[label("expected {expected} fields, got {found}")]
        span: Span,
    },

    #[error("missing field `{field}` in record {record}")]
    #[diagnostic(code(ivy::types::missing_field))]
    MissingField {
        record: String,
        field: String,
        #[label("missing field '{field}'")]
        span: Span,
    },

    #[error("module not found: {module}")]
    #[diagnostic(code(ivy::types::module_not_found))]
    ModuleNotFound {
        module: String,
        #[label("module '{module}' not found")]
        span: Span,
    },

    #[error("circular import detected: {module}")]
    #[diagnostic(code(ivy::types::circular_import), help("import cycle: {} -> {module}", cycle.join(" -> ")))]
    CircularImport {
        module: String,
        cycle: Vec<String>,
        #[label("circular import")]
        span: Span,
    },

    #[error("error reading module {module}: {error}")]
    #[diagnostic(code(ivy::types::module_io_error))]
    ModuleIOError {
        module: String,
        error: String,
        #[label("could not read module")]
        span: Span,
    },

    #[error("parse error in module {module}: {error}")]
    #[diagnostic(code(ivy::types::module_parse_error))]
    ModuleParseError {
        module: String,
        error: String,
        #[label("parse error in module")]
        span: Span,
    },

    #[error("type error in module {module}: {inner}")]
    #[diagnostic(code(ivy::types::module_type_error))]
    ModuleTypeError {
        module: String,
        file_path: String,
        module_source: String,
        inner: Box<TypeError>,
    },
}

impl TypeError {
    /// Get the primary span associated with this error (for callers that need it).
    pub fn span(&self) -> Span {
        match self {
            TypeError::Mismatch { span, .. }
            | TypeError::InfiniteType { span, .. }
            | TypeError::UndefinedVariable { span, .. }
            | TypeError::UndefinedType { span, .. }
            | TypeError::UndefinedConstructor { span, .. }
            | TypeError::ArityMismatch { span, .. }
            | TypeError::NotCallable { span, .. }
            | TypeError::NotARecord { span, .. }
            | TypeError::UndefinedField { span, .. }
            | TypeError::NotIndexable { span, .. }
            | TypeError::PatternMismatch { span, .. }
            | TypeError::OrPatternBindingMismatch { span, .. }
            | TypeError::NonExhaustive { span, .. }
            | TypeError::DuplicateDefinition { span, .. }
            | TypeError::AnnotationMismatch { span, .. }
            | TypeError::RecordFieldCount { span, .. }
            | TypeError::MissingField { span, .. }
            | TypeError::ModuleNotFound { span, .. }
            | TypeError::CircularImport { span, .. }
            | TypeError::ModuleIOError { span, .. }
            | TypeError::ModuleParseError { span, .. } => *span,
            TypeError::ModuleTypeError { inner, .. } => inner.span(),
        }
    }

    pub fn mismatch(expected: Type, found: Type, span: Span) -> TypeError {
        TypeError::Mismatch {
            expected,
            found,
            span,
            expected_span: None,
        }
    }

    pub fn mismatch_at(expected: Type, found: Type, span: Span, expected_span: Span) -> TypeError {
        TypeError::Mismatch {
            expected,
            found,
            span,
            expected_span: Some(expected_span),
        }
    }

    pub fn infinite_type(var: TypeVar, ty: Type, span: Span) -> TypeError {
        TypeError::InfiniteType { var, ty, span }
    }

    pub fn undefined_variable(name: &str, span: Span) -> TypeError {
        TypeError::UndefinedVariable {
            name: name.to_string(),
            span,
        }
    }

    pub fn undefined_type(name: &str, span: Span) -> TypeError {
        TypeError::UndefinedType {
            name: name.to_string(),
            span,
        }
    }

    pub fn undefined_constructor(name: &str, span: Span) -> TypeError {
        TypeError::UndefinedConstructor {
            name: name.to_string(),
            span,
        }
    }

    pub fn arity_mismatch(name: &str, expected: usize, found: usize, span: Span) -> TypeError {
        TypeError::ArityMismatch {
            name: name.to_string(),
            expected,
            found,
            span,
        }
    }

    pub fn not_callable(ty: Type, span: Span) -> TypeError {
        TypeError::NotCallable { ty, span }
    }

    pub fn not_a_record(ty: Type, span: Span) -> TypeError {
        TypeError::NotARecord { ty, span }
    }

    pub fn undefined_field(record: &str, field: &str, span: Span) -> TypeError {
        TypeError::UndefinedField {
            record: record.to_string(),
            field: field.to_string(),
            span,
        }
    }

    pub fn not_indexable(ty: Type, span: Span) -> TypeError {
        TypeError::NotIndexable { ty, span }
    }

    pub fn duplicate_definition(name: &str, span: Span) -> TypeError {
        TypeError::DuplicateDefinition {
            name: name.to_string(),
            span,
        }
    }

    pub fn annotation_mismatch(annotated: Type, inferred: Type, span: Span) -> TypeError {
        TypeError::AnnotationMismatch {
            annotated,
            inferred,
            span,
        }
    }

    pub fn record_field_count(record: &str, expected: usize, found: usize, span: Span) -> TypeError {
        TypeError::RecordFieldCount {
            record: record.to_string(),
            expected,
            found,
            span,
        }
    }

    pub fn missing_field(record: &str, field: &str, span: Span) -> TypeError {
        TypeError::MissingField {
            record: record.to_string(),
            field: field.to_string(),
            span,
        }
    }

    pub fn module_not_found(module: &str, span: Span) -> TypeError {
        TypeError::ModuleNotFound {
            module: module.to_string(),
            span,
        }
    }

    pub fn circular_import(module: &str, cycle: Vec<String>, span: Span) -> TypeError {
        TypeError::CircularImport {
            module: module.to_string(),
            cycle,
            span,
        }
    }

    pub fn module_io_error(module: &str, error: &str, span: Span) -> TypeError {
        TypeError::ModuleIOError {
            module: module.to_string(),
            error: error.to_string(),
            span,
        }
    }

    pub fn module_parse_error(module: &str, error: &str, span: Span) -> TypeError {
        TypeError::ModuleParseError {
            module: module.to_string(),
            error: error.to_string(),
            span,
        }
    }

    pub fn module_type_error(module: &str, file_path: &str, module_source: &str, inner: TypeError) -> TypeError {
        TypeError::ModuleTypeError {
            module: module.to_string(),
            file_path: file_path.to_string(),
            module_source: module_source.to_string(),
            inner: Box::new(inner),
        }
    }
}

fn plural(n: usize) -> &'static str {
    if n == 1 {
        ""
    } else {
        "s"
    }
}

fn was_were(n: usize) -> &'static str {
    if n == 1 {
        "was"
    } else {
        "were"
    }
}
