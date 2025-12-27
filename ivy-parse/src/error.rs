//! Error types for parsing.

use crate::token::{Token, TokenKind};
use ivy_syntax::Span;
use miette::Diagnostic;
use thiserror::Error;

/// Result type for parsing operations.
pub type ParseResult<T> = Result<T, ParseError>;

/// Parse errors.
#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    #[diagnostic(code(ivy::parse::unexpected_token))]
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        #[label("found {} here", found)]
        span: Span,
    },

    #[error("Unexpected end of file")]
    #[diagnostic(code(ivy::parse::unexpected_eof))]
    UnexpectedEof {
        expected: String,
        #[label("expected {} here", expected)]
        span: Span,
    },

    #[error("Unterminated {kind}")]
    #[diagnostic(code(ivy::parse::unterminated))]
    Unterminated {
        kind: &'static str,
        #[label("started here")]
        start: Span,
    },

    #[error("Invalid escape sequence: {sequence}")]
    #[diagnostic(code(ivy::lex::invalid_escape))]
    InvalidEscape {
        sequence: String,
        #[label("invalid escape")]
        span: Span,
    },

    #[error("Invalid number literal")]
    #[diagnostic(code(ivy::lex::invalid_number))]
    InvalidNumber {
        #[label("invalid number")]
        span: Span,
    },

    #[error("Invalid character literal")]
    #[diagnostic(code(ivy::lex::invalid_char))]
    InvalidChar {
        #[label("invalid character")]
        span: Span,
    },

    #[error("Unexpected character: {ch}")]
    #[diagnostic(code(ivy::lex::unexpected_char))]
    UnexpectedChar {
        ch: char,
        #[label("unexpected character")]
        span: Span,
    },

    #[error("Invalid pattern in binding")]
    #[diagnostic(
        code(ivy::parse::invalid_pattern),
        help("Only identifiers, tuples, lists, and record patterns can appear in bindings")
    )]
    InvalidPattern {
        #[label("this is not a valid pattern")]
        span: Span,
    },
}

impl ParseError {
    /// Create an unexpected token error.
    pub fn unexpected(expected: impl Into<String>, token: &Token) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.into(),
            found: token.kind,
            span: token.span,
        }
    }

    /// Create an unexpected EOF error.
    pub fn eof(expected: impl Into<String>, span: Span) -> Self {
        ParseError::UnexpectedEof {
            expected: expected.into(),
            span,
        }
    }
}

/// Lex error (separate from parse errors for the two-phase approach).
#[derive(Error, Debug, Diagnostic)]
pub enum LexError {
    #[error("Unterminated string literal")]
    #[diagnostic(code(ivy::lex::unterminated_string))]
    UnterminatedString {
        #[label("string started here")]
        start: Span,
    },

    #[error("Unterminated character literal")]
    #[diagnostic(code(ivy::lex::unterminated_char))]
    UnterminatedChar {
        #[label("char started here")]
        start: Span,
    },

    #[error("Unterminated block comment")]
    #[diagnostic(code(ivy::lex::unterminated_comment))]
    UnterminatedComment {
        #[label("comment started here")]
        start: Span,
    },

    #[error("Invalid escape sequence: \\{ch}")]
    #[diagnostic(code(ivy::lex::invalid_escape))]
    InvalidEscape {
        ch: char,
        #[label("invalid escape sequence")]
        span: Span,
    },

    #[error("Unexpected character: '{ch}'")]
    #[diagnostic(code(ivy::lex::unexpected_char))]
    UnexpectedChar {
        ch: char,
        #[label("unexpected character")]
        span: Span,
    },
}

/// Result type for lexing operations.
pub type LexResult<T> = Result<T, LexError>;
