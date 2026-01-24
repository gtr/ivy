//! Lexer for Ivy

use crate::error::{LexError, LexResult};
use crate::token::{Token, TokenKind};
use ivy_syntax::Span;

/// Lex the entire input into a vector of tokens.
pub fn lex(input: &str) -> LexResult<Vec<Token>> {
    let mut lexer = Lexer::new(input);
    lexer.tokenize()
}

/// The lexer state.
struct Lexer<'a> {
    /// The input source code.
    input: &'a str,
    /// Current byte offset.
    offset: usize,
    /// Collected tokens.
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer.
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            tokens: Vec::new(),
        }
    }

    /// Tokenize the entire input.
    fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        while !self.is_at_end() {
            self.skip_trivia()?;
            if self.is_at_end() {
                break;
            }
            let token = self.next_token()?;
            self.tokens.push(token);
        }

        self.tokens.push(Token::eof(self.offset));
        Ok(std::mem::take(&mut self.tokens))
    }

    /// Check if we're at the end of input.
    fn is_at_end(&self) -> bool {
        self.offset >= self.input.len()
    }

    /// Get the remaining input.
    fn remaining(&self) -> &str {
        &self.input[self.offset..]
    }

    /// Peek at the current character.
    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Peek at the next character.
    fn peek_next(&self) -> Option<char> {
        self.remaining().chars().nth(1)
    }

    /// Advance by one character and return it.
    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    /// Check if the remaining input starts with the given string.
    fn starts_with(&self, s: &str) -> bool {
        self.remaining().starts_with(s)
    }

    /// Advance by a fixed number of bytes
    fn advance_by(&mut self, n: usize) {
        self.offset += n;
    }

    /// Skip whitespace and comments
    fn skip_trivia(&mut self) -> LexResult<()> {
        loop {
            // Skip whitespace
            while let Some(ch) = self.peek() {
                if ch.is_whitespace() {
                    self.advance();
                } else {
                    break;
                }
            }

            // Skip line comments: --
            if self.starts_with("--") {
                self.advance_by(2);
                while let Some(ch) = self.peek() {
                    if ch == '\n' {
                        self.advance();
                        break;
                    }
                    self.advance();
                }
                continue;
            }

            // Skip block comments: {- ... -}
            if self.starts_with("{-") {
                let start = self.offset;
                self.advance_by(2);
                self.skip_block_comment(start)?;
                continue;
            }

            break;
        }
        Ok(())
    }

    /// Skip a block comment, handling nesting.
    fn skip_block_comment(&mut self, start: usize) -> LexResult<()> {
        let mut depth = 1;

        while depth > 0 {
            if self.is_at_end() {
                return Err(LexError::UnterminatedComment {
                    start: Span::new(start, start + 2),
                });
            }

            if self.starts_with("{-") {
                self.advance_by(2);
                depth += 1;
            } else if self.starts_with("-}") {
                self.advance_by(2);
                depth -= 1;
            } else {
                self.advance();
            }
        }

        Ok(())
    }

    /// Lex the next token.
    fn next_token(&mut self) -> LexResult<Token> {
        let start = self.offset;
        let ch = self.peek().unwrap();

        // Multi-character operators
        if self.starts_with("==") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::EqEq, start));
        }
        if self.starts_with("!=") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::BangEq, start));
        }
        if self.starts_with("<=") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::Le, start));
        }
        if self.starts_with(">=") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::Ge, start));
        }
        if self.starts_with("::") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::ColonColon, start));
        }
        if self.starts_with("++") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::PlusPlus, start));
        }
        if self.starts_with("->") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::Arrow, start));
        }
        if self.starts_with("=>") {
            self.advance_by(2);
            return Ok(self.make_token(TokenKind::FatArrow, start));
        }

        // Single-character tokens
        let kind = match ch {
            '+' => {
                self.advance();
                TokenKind::Plus
            }
            '-' => {
                self.advance();
                TokenKind::Minus
            }
            '*' => {
                self.advance();
                TokenKind::Star
            }
            '/' => {
                self.advance();
                TokenKind::Slash
            }
            '%' => {
                self.advance();
                TokenKind::Percent
            }
            '<' => {
                self.advance();
                TokenKind::Lt
            }
            '>' => {
                self.advance();
                TokenKind::Gt
            }
            '!' => {
                self.advance();
                TokenKind::Bang
            }
            '=' => {
                self.advance();
                TokenKind::Eq
            }
            ':' => {
                self.advance();
                TokenKind::Colon
            }
            '.' => {
                self.advance();
                TokenKind::Dot
            }
            '|' => {
                self.advance();
                TokenKind::Pipe
            }
            '(' => {
                self.advance();
                TokenKind::LParen
            }
            ')' => {
                self.advance();
                TokenKind::RParen
            }
            '{' => {
                self.advance();
                TokenKind::LBrace
            }
            '}' => {
                self.advance();
                TokenKind::RBrace
            }
            '[' => {
                self.advance();
                TokenKind::LBracket
            }
            ']' => {
                self.advance();
                TokenKind::RBracket
            }
            ',' => {
                self.advance();
                TokenKind::Comma
            }
            ';' => {
                self.advance();
                TokenKind::Semi
            }

            // String literal
            '"' => return self.lex_string(),

            // Character literal
            '\'' => return self.lex_char(),

            // Number or identifier
            _ if ch.is_ascii_digit() => return self.lex_number(),
            _ if ch == '_' || ch.is_alphabetic() => return self.lex_identifier(),

            _ => {
                return Err(LexError::UnexpectedChar {
                    ch,
                    span: Span::new(start, start + ch.len_utf8()),
                })
            }
        };

        Ok(self.make_token(kind, start))
    }

    /// Create a token from the current position.
    fn make_token(&self, kind: TokenKind, start: usize) -> Token {
        let lexeme = &self.input[start..self.offset];
        Token::new(kind, Span::new(start, self.offset), lexeme)
    }

    /// Lex a string literal
    fn lex_string(&mut self) -> LexResult<Token> {
        let start = self.offset;
        self.advance();

        let mut value = String::new();

        while let Some(ch) = self.peek() {
            if ch == '"' {
                self.advance();
                let mut token = self.make_token(TokenKind::String, start);
                token.lexeme = value;
                return Ok(token);
            }

            if ch == '\n' {
                return Err(LexError::UnterminatedString {
                    start: Span::new(start, start + 1),
                });
            }

            if ch == '\\' {
                self.advance();
                let escaped = self.lex_escape_sequence()?;
                value.push(escaped);
            } else {
                value.push(ch);
                self.advance();
            }
        }

        Err(LexError::UnterminatedString {
            start: Span::new(start, start + 1),
        })
    }

    /// Lex an escape sequence.
    fn lex_escape_sequence(&mut self) -> LexResult<char> {
        let pos = self.offset;
        match self.advance() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('"') => Ok('"'),
            Some('\'') => Ok('\''),
            Some('0') => Ok('\0'),
            Some(ch) => Err(LexError::InvalidEscape {
                ch,
                span: Span::new(pos - 1, pos + ch.len_utf8()),
            }),
            None => Err(LexError::InvalidEscape {
                ch: ' ',
                span: Span::new(pos - 1, pos),
            }),
        }
    }

    /// Lex a character literal.
    fn lex_char(&mut self) -> LexResult<Token> {
        let start = self.offset;
        self.advance();

        let ch = if self.peek() == Some('\\') {
            self.advance();
            self.lex_escape_sequence()?
        } else {
            match self.advance() {
                Some(c) => c,
                None => {
                    return Err(LexError::UnterminatedChar {
                        start: Span::new(start, start + 1),
                    })
                }
            }
        };

        if self.peek() != Some('\'') {
            return Err(LexError::UnterminatedChar {
                start: Span::new(start, start + 1),
            });
        }
        self.advance();

        let mut token = self.make_token(TokenKind::Char, start);
        token.lexeme = ch.to_string();
        Ok(token)
    }

    /// Lex a number literal.
    fn lex_number(&mut self) -> LexResult<Token> {
        let start = self.offset;

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') && self.peek_next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.advance(); // consume .
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            return Ok(self.make_token(TokenKind::Float, start));
        }

        Ok(self.make_token(TokenKind::Int, start))
    }

    /// Lex an identifier or keyword.
    fn lex_identifier(&mut self) -> LexResult<Token> {
        let start = self.offset;
        let first_char = self.peek().unwrap();

        self.advance();

        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' || ch == '?' {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = &self.input[start..self.offset];

        // Check for underscore (wildcard)
        if lexeme == "_" {
            return Ok(self.make_token(TokenKind::Underscore, start));
        }

        // Check for keyword
        if let Some(kind) = TokenKind::keyword(lexeme) {
            return Ok(self.make_token(kind, start));
        }

        // Determine identifier type based on first character
        let kind = if first_char.is_uppercase() {
            TokenKind::TypeIdent
        } else {
            TokenKind::Ident
        };

        Ok(self.make_token(kind, start))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = lex("let x = 42;").unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].kind, TokenKind::Let);
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[2].kind, TokenKind::Eq);
        assert_eq!(tokens[3].kind, TokenKind::Int);
        assert_eq!(tokens[4].kind, TokenKind::Semi);
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_operators() {
        let tokens = lex("+ - * / % == != < <= > >= and or :: ++").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::Percent);
        assert_eq!(tokens[5].kind, TokenKind::EqEq);
        assert_eq!(tokens[6].kind, TokenKind::BangEq);
        assert_eq!(tokens[7].kind, TokenKind::Lt);
        assert_eq!(tokens[8].kind, TokenKind::Le);
        assert_eq!(tokens[9].kind, TokenKind::Gt);
        assert_eq!(tokens[10].kind, TokenKind::Ge);
        assert_eq!(tokens[11].kind, TokenKind::And);
        assert_eq!(tokens[12].kind, TokenKind::Or);
        assert_eq!(tokens[13].kind, TokenKind::ColonColon);
        assert_eq!(tokens[14].kind, TokenKind::PlusPlus);
    }

    #[test]
    fn test_string_literal() {
        let tokens = lex(r#""hello\nworld""#).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(tokens[0].lexeme, "hello\nworld");
    }

    #[test]
    fn test_comments() {
        let tokens = lex("x -- this is a comment\ny").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[1].lexeme, "y");
    }

    #[test]
    fn test_block_comments() {
        let tokens = lex("x {- nested {- comment -} here -} y").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[1].lexeme, "y");
    }

    #[test]
    fn test_type_ident() {
        let tokens = lex("Option Some None").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::TypeIdent);
        assert_eq!(tokens[1].kind, TokenKind::TypeIdent);
        assert_eq!(tokens[2].kind, TokenKind::TypeIdent);
    }

    #[test]
    fn test_predicate_identifier() {
        let tokens = lex("empty? valid?").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident);
        assert_eq!(tokens[0].lexeme, "empty?");
        assert_eq!(tokens[1].kind, TokenKind::Ident);
        assert_eq!(tokens[1].lexeme, "valid?");
    }
}
