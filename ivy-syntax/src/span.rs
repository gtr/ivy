//! Source code spans and locations.

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
    /// Start byte offset (inclusive).
    pub start: ByteOffset,
    /// End byte offset (exclusive).
    pub end: ByteOffset,
}

impl Span {
    /// Create a new span from start to end
    pub fn new(start: ByteOffset, end: ByteOffset) -> Self {
        Self { start, end }
    }

    /// Create a zero-width span at a position.
    pub fn point(offset: ByteOffset) -> Self {
        Self {
            start: offset,
            end: offset,
        }
    }

    /// Merge two spans into one covering both.
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Get the length of this span in bytes.
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Check if this span is empty.
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

/// A node wrapper that attaches a Span to any AST type.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    /// The wrapped node.
    pub node: T,
    /// The source location.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned node.
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    /// Map the inner node to a new type.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Get a reference to the inner node.
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            node: &self.node,
            span: self.span,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}
