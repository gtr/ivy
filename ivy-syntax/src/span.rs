use std::ops::Deref;

/// A byte offset in the source code
pub type ByteOffset = usize;

/// Convert our Span to miette's SourceSpan for error reporting
impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        (span.start, span.end.saturating_sub(span.start)).into()
    }
}

/// Represents a contiguous region in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: ByteOffset,
    pub end: ByteOffset,
}

impl Span {
    pub fn new(start: ByteOffset, end: ByteOffset) -> Self {
        Self { start, end }
    }

    pub fn point(offset: ByteOffset) -> Self {
        Self {
            start: offset,
            end: offset,
        }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

/// A node wrapper that attaches a Span to any AST type.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            node: &self.node,
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}
