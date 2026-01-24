//! Token types for the lexer.

use ivy_syntax::Span;

/// A token with its kind, span, and lexeme.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// The source location.
    pub span: Span,
    /// The original text.
    pub lexeme: String,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span, lexeme: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            lexeme: lexeme.into(),
        }
    }

    /// Create an EOF token at the given position.
    pub fn eof(offset: usize) -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span::point(offset),
            lexeme: String::new(),
        }
    }
}

/// Token kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    /// Integer literal
    Int,
    /// Float literal
    Float,
    /// String literal
    String,
    /// Character literal
    Char,

    // Identifiers
    /// Lowercase identifier (variable, function)
    Ident,
    /// Uppercase identifier (type, constructor)
    TypeIdent,

    // Keywords
    Let,
    Mut,
    Fn,
    If,
    Then,
    Else,
    Match,
    With,
    End,
    Type,
    Trait,
    Impl,
    Import,
    Module,
    Pub,
    True,
    False,
    Where,
    Do,
    For,
    And,
    Or,

    // Operators
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,
    /// ==
    EqEq,
    /// !=
    BangEq,
    /// <
    Lt,
    /// <=
    Le,
    /// >
    Gt,
    /// >=
    Ge,
    /// !
    Bang,
    /// ::
    ColonColon,
    /// ++
    PlusPlus,

    // Syntax
    /// =
    Eq,
    /// ->
    Arrow,
    /// =>
    FatArrow,
    /// :
    Colon,
    /// .
    Dot,
    /// |
    Pipe,

    // Delimiters
    /// (
    LParen,
    /// )
    RParen,
    /// {
    LBrace,
    /// }
    RBrace,
    /// [
    LBracket,
    /// ]
    RBracket,
    /// ,
    Comma,
    /// ;
    Semi,
    /// _
    Underscore,

    // Special
    /// End of file
    Eof,
}

impl TokenKind {
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Let
                | TokenKind::Mut
                | TokenKind::Fn
                | TokenKind::If
                | TokenKind::Then
                | TokenKind::Else
                | TokenKind::Match
                | TokenKind::With
                | TokenKind::End
                | TokenKind::Type
                | TokenKind::Trait
                | TokenKind::Impl
                | TokenKind::Import
                | TokenKind::Module
                | TokenKind::Pub
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Where
                | TokenKind::Do
                | TokenKind::For
                | TokenKind::And
                | TokenKind::Or
        )
    }

    pub fn keyword(s: &str) -> Option<TokenKind> {
        match s {
            "let" => Some(TokenKind::Let),
            "mut" => Some(TokenKind::Mut),
            "fn" => Some(TokenKind::Fn),
            "if" => Some(TokenKind::If),
            "then" => Some(TokenKind::Then),
            "else" => Some(TokenKind::Else),
            "match" => Some(TokenKind::Match),
            "with" => Some(TokenKind::With),
            "end" => Some(TokenKind::End),
            "type" => Some(TokenKind::Type),
            "trait" => Some(TokenKind::Trait),
            "impl" => Some(TokenKind::Impl),
            "import" => Some(TokenKind::Import),
            "module" => Some(TokenKind::Module),
            "pub" => Some(TokenKind::Pub),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            "where" => Some(TokenKind::Where),
            "do" => Some(TokenKind::Do),
            "for" => Some(TokenKind::For),
            "and" => Some(TokenKind::And),
            "or" => Some(TokenKind::Or),
            _ => None,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::Int => "integer",
            TokenKind::Float => "float",
            TokenKind::String => "string",
            TokenKind::Char => "char",
            TokenKind::Ident => "identifier",
            TokenKind::TypeIdent => "type name",
            TokenKind::Let => "'let'",
            TokenKind::Mut => "'mut'",
            TokenKind::Fn => "'fn'",
            TokenKind::If => "'if'",
            TokenKind::Then => "'then'",
            TokenKind::Else => "'else'",
            TokenKind::Match => "'match'",
            TokenKind::With => "'with'",
            TokenKind::End => "'end'",
            TokenKind::Type => "'type'",
            TokenKind::Trait => "'trait'",
            TokenKind::Impl => "'impl'",
            TokenKind::Import => "'import'",
            TokenKind::Module => "'module'",
            TokenKind::Pub => "'pub'",
            TokenKind::True => "'true'",
            TokenKind::False => "'false'",
            TokenKind::Where => "'where'",
            TokenKind::Do => "'do'",
            TokenKind::For => "'for'",
            TokenKind::And => "'and'",
            TokenKind::Or => "'or'",
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Star => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::Percent => "'%'",
            TokenKind::EqEq => "'=='",
            TokenKind::BangEq => "'!='",
            TokenKind::Lt => "'<'",
            TokenKind::Le => "'<='",
            TokenKind::Gt => "'>'",
            TokenKind::Ge => "'>='",
            TokenKind::Bang => "'!'",
            TokenKind::ColonColon => "'::'",
            TokenKind::PlusPlus => "'++'",
            TokenKind::Eq => "'='",
            TokenKind::Arrow => "'->'",
            TokenKind::FatArrow => "'=>'",
            TokenKind::Colon => "':'",
            TokenKind::Dot => "'.'",
            TokenKind::Pipe => "'|'",
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::LBracket => "'['",
            TokenKind::RBracket => "']'",
            TokenKind::Comma => "','",
            TokenKind::Semi => "';'",
            TokenKind::Underscore => "'_'",
            TokenKind::Eof => "end of file",
        };
        write!(f, "{}", s)
    }
}
