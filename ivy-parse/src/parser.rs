//! Parser for Ivy.
//!
//! This parser consumes tokens produced by the lexer and builds an AST.

use crate::error::{ParseError, ParseResult};
use crate::lexer;
use crate::token::{Token, TokenKind};
use ivy_syntax::ast::{Ident, Program};
use ivy_syntax::decl::{Constraint, Decl, FnBody, FnDecl, GuardedExpr, RecordField, TraitItem, TypeBody, Variant};
use ivy_syntax::expr::{Expr, FieldInit, MatchArm, Param};
use ivy_syntax::lit::Literal;
use ivy_syntax::op::{BinOp, UnaryOp};
use ivy_syntax::pattern::{FieldPattern, Pattern};
use ivy_syntax::span::{Span, Spanned};
use ivy_syntax::types::TypeExpr;

/// Parse source code into an AST.
pub fn parse(source: &str) -> ParseResult<Program> {
    let tokens = lexer::lex(source).map_err(|e| match e {
        crate::error::LexError::UnexpectedChar { ch, span } => ParseError::UnexpectedChar { ch, span },
        crate::error::LexError::UnterminatedString { start } => ParseError::Unterminated { kind: "string", start },
        crate::error::LexError::UnterminatedChar { start } => ParseError::Unterminated {
            kind: "character",
            start,
        },
        crate::error::LexError::UnterminatedComment { start } => ParseError::Unterminated { kind: "comment", start },
        crate::error::LexError::InvalidEscape { ch, span } => ParseError::InvalidEscape {
            sequence: format!("\\{}", ch),
            span,
        },
    })?;

    let mut parser = Parser::new(&tokens);
    parser.parse_program()
}

/// The parser state.
struct Parser<'a> {
    /// The token stream.
    tokens: &'a [Token],
    /// Current position in the token stream.
    pos: usize,
}

impl<'a> Parser<'a> {
    /// Create a new parser.
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Get the current token.
    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    /// Peek at the current token kind.
    fn peek(&self) -> TokenKind {
        self.current().kind
    }

    /// Check if we're at the end.
    fn is_at_end(&self) -> bool {
        self.peek() == TokenKind::Eof
    }

    /// Advance to the next token.
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1).unwrap()
    }

    /// Check if the current token matches a kind.
    fn check(&self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Consume a token if it matches, otherwise return an error.
    fn expect(&mut self, kind: TokenKind) -> ParseResult<&Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::unexpected(kind.to_string(), self.current()))
        }
    }

    /// Try to consume a token if it matches.
    fn match_token(&mut self, kind: TokenKind) -> Option<&Token> {
        if self.check(kind) {
            Some(self.advance())
        } else {
            None
        }
    }

    /// Create a span from start to current position.
    fn span_from(&self, start: usize) -> Span {
        let end = if self.pos > 0 {
            self.tokens[self.pos - 1].span.end
        } else {
            start
        };
        Span::new(start, end)
    }

    /// Parse a complete program.
    fn parse_program(&mut self) -> ParseResult<Program> {
        let start = self.current().span.start;
        let mut declarations = Vec::new();

        while !self.is_at_end() {
            let decl = self.parse_declaration()?;
            declarations.push(decl);
        }

        let span = self.span_from(start);
        Ok(Program::new(declarations, span))
    }

    /// Parse a top-level declaration or expression.
    fn parse_declaration(&mut self) -> ParseResult<Spanned<Decl>> {
        let start = self.current().span.start;

        // Check for pub modifier
        let is_pub = self.match_token(TokenKind::Pub).is_some();

        let decl = match self.peek() {
            TokenKind::Module => self.parse_module_decl()?,
            TokenKind::Import => self.parse_import_decl()?,
            TokenKind::Type => self.parse_type_decl(is_pub)?,
            TokenKind::Trait => self.parse_trait_decl()?,
            TokenKind::Impl => self.parse_impl_decl()?,
            TokenKind::Fn => self.parse_fn_decl(is_pub)?,
            TokenKind::Let => self.parse_let_decl(is_pub)?,
            _ if is_pub => {
                return Err(ParseError::unexpected("declaration after 'pub'", self.current()));
            }
            // Top-level expression (e.g., function call like println(...))
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semi)?;
                let span = self.span_from(start);
                return Ok(Spanned::new(
                    Decl::Let {
                        is_pub: false,
                        is_mut: false,
                        pattern: Spanned::new(Pattern::Wildcard, span),
                        ty: None,
                        value: expr,
                    },
                    span,
                ));
            }
        };

        let span = self.span_from(start);
        Ok(Spanned::new(decl, span))
    }

    /// Parse module declaration: module Name;
    fn parse_module_decl(&mut self) -> ParseResult<Decl> {
        self.expect(TokenKind::Module)?;
        let name = self.parse_type_ident()?;
        self.expect(TokenKind::Semi)?;
        Ok(Decl::Module { name })
    }

    /// Parse import declaration: import Path.To.Module;
    fn parse_import_decl(&mut self) -> ParseResult<Decl> {
        self.expect(TokenKind::Import)?;

        let mut path = vec![self.parse_ident_any()?];

        while self.match_token(TokenKind::Dot).is_some() {
            // Check for selective import: import Foo.{bar, baz}
            if self.check(TokenKind::LBrace) {
                self.advance();
                let mut items = vec![self.parse_ident_any()?];
                while self.match_token(TokenKind::Comma).is_some() {
                    items.push(self.parse_ident_any()?);
                }
                self.expect(TokenKind::RBrace)?;
                self.expect(TokenKind::Semi)?;
                return Ok(Decl::Import {
                    path,
                    items: Some(items),
                });
            }

            path.push(self.parse_ident_any()?);
        }

        self.expect(TokenKind::Semi)?;
        Ok(Decl::Import { path, items: None })
    }

    /// Parse type declaration: type Name<a> = ...;
    fn parse_type_decl(&mut self, is_pub: bool) -> ParseResult<Decl> {
        self.expect(TokenKind::Type)?;
        let name = self.parse_type_ident()?;

        // Optional type parameters (can be lowercase a, b or uppercase T, E)
        let params = if self.match_token(TokenKind::Lt).is_some() {
            let mut params = vec![self.parse_ident_any()?];
            while self.match_token(TokenKind::Comma).is_some() {
                params.push(self.parse_ident_any()?);
            }
            self.expect(TokenKind::Gt)?;
            params
        } else {
            Vec::new()
        };

        self.expect(TokenKind::Eq)?;

        let body = if self.check(TokenKind::Pipe) {
            // Sum type
            self.parse_sum_type()?
        } else if self.check(TokenKind::LBrace) {
            // Record type
            self.parse_record_type()?
        } else {
            return Err(ParseError::unexpected("'|' or '{'", self.current()));
        };

        self.expect(TokenKind::Semi)?;

        Ok(Decl::Type {
            is_pub,
            name,
            params,
            body,
        })
    }

    /// Parse sum type: | Variant1 | Variant2(T)
    fn parse_sum_type(&mut self) -> ParseResult<TypeBody> {
        let mut variants = Vec::new();

        while self.match_token(TokenKind::Pipe).is_some() {
            let start = self.current().span.start;
            let name = self.parse_type_ident()?;

            let fields = if self.match_token(TokenKind::LParen).is_some() {
                let mut fields = vec![self.parse_type_expr()?];
                while self.match_token(TokenKind::Comma).is_some() {
                    fields.push(self.parse_type_expr()?);
                }
                self.expect(TokenKind::RParen)?;
                fields
            } else {
                Vec::new()
            };

            let span = self.span_from(start);
            variants.push(Variant::new(name, fields, span));
        }

        Ok(TypeBody::Sum(variants))
    }

    /// Parse record type: { field1: Type1, field2: Type2 }
    fn parse_record_type(&mut self) -> ParseResult<TypeBody> {
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();

        if !self.check(TokenKind::RBrace) {
            loop {
                let start = self.current().span.start;
                let name = self.parse_ident()?;
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type_expr()?;
                let span = self.span_from(start);
                fields.push(RecordField::new(name, ty, span));

                if !self.match_token(TokenKind::Comma).is_some() {
                    break;
                }
                // Allow trailing comma
                if self.check(TokenKind::RBrace) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RBrace)?;
        Ok(TypeBody::Record(fields))
    }

    /// Parse trait declaration: trait Show<a> { ... }
    fn parse_trait_decl(&mut self) -> ParseResult<Decl> {
        self.expect(TokenKind::Trait)?;
        let name = self.parse_type_ident()?;
        self.expect(TokenKind::Lt)?;
        let param = self.parse_ident()?;
        self.expect(TokenKind::Gt)?;
        self.expect(TokenKind::LBrace)?;

        let mut items = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let item = self.parse_trait_item()?;
            items.push(item);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Decl::Trait { name, param, items })
    }

    /// Parse trait item.
    fn parse_trait_item(&mut self) -> ParseResult<TraitItem> {
        let start = self.current().span.start;
        self.expect(TokenKind::Fn)?;
        let name = self.parse_ident()?;

        // Check for signature (with params) vs full function vs type signature
        if self.check(TokenKind::LParen) {
            // fn name(params): ReturnType; or fn name(params) => body;
            self.expect(TokenKind::LParen)?;

            let mut params = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    let param = self.parse_param()?;
                    params.push(param);
                    if !self.match_token(TokenKind::Comma).is_some() {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RParen)?;

            let return_ty = if self.match_token(TokenKind::Colon).is_some() {
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            // Check if this is just a signature (;) or has a body (=>)
            if self.match_token(TokenKind::Semi).is_some() {
                let span = self.span_from(start);
                let ty = self.build_fn_type_from_params(&params, return_ty, span)?;
                Ok(TraitItem::Signature { name, ty, span })
            } else if self.match_token(TokenKind::FatArrow).is_some() {
                let body_expr = self.parse_expr()?;
                self.expect(TokenKind::Semi)?;
                let span = self.span_from(start);
                Ok(TraitItem::DefaultImpl(FnDecl::new(
                    false,
                    name,
                    params,
                    return_ty,
                    FnBody::Expr(body_expr),
                    span,
                )))
            } else if self.check(TokenKind::Pipe) {
                // Guarded default implementation
                let mut guards = Vec::new();
                while self.match_token(TokenKind::Pipe).is_some() {
                    let guard_start = self.current().span.start;
                    let guard = self.parse_expr()?;
                    self.expect(TokenKind::FatArrow)?;
                    let body = self.parse_expr()?;
                    let guard_span = self.span_from(guard_start);
                    guards.push(GuardedExpr::new(guard, body, guard_span));
                }
                self.expect(TokenKind::Semi)?;
                let span = self.span_from(start);
                Ok(TraitItem::DefaultImpl(FnDecl::new(
                    false,
                    name,
                    params,
                    return_ty,
                    FnBody::Guards(guards),
                    span,
                )))
            } else {
                Err(ParseError::unexpected("';' or '=>'", self.current()))
            }
        } else if self.match_token(TokenKind::Colon).is_some() {
            // fn name: Type; (type signature without params)
            let ty = self.parse_type_expr()?;
            self.expect(TokenKind::Semi)?;
            let span = self.span_from(start);
            Ok(TraitItem::Signature { name, ty, span })
        } else {
            Err(ParseError::unexpected("'(' or ':'", self.current()))
        }
    }

    /// Build a function type from parameters.
    fn build_fn_type_from_params(
        &self,
        params: &[Param],
        return_ty: Option<Spanned<TypeExpr>>,
        span: Span,
    ) -> ParseResult<Spanned<TypeExpr>> {
        // TODO(gtr): For now, just return the return type or Unit
        // In the future, we'd build: param1 -> param2 -> ... -> return
        let result = return_ty.unwrap_or_else(|| Spanned::new(TypeExpr::Unit, span));

        if params.is_empty() {
            Ok(result)
        } else {
            // Build function type from params
            let mut ty = result;
            for param in params.iter().rev() {
                if let Some(param_ty) = &param.ty {
                    ty = Spanned::new(
                        TypeExpr::Function {
                            param: Box::new(param_ty.clone()),
                            result: Box::new(ty),
                        },
                        span,
                    );
                }
            }
            Ok(ty)
        }
    }

    /// Parse impl declaration: impl Trait for Type { ... }
    fn parse_impl_decl(&mut self) -> ParseResult<Decl> {
        self.expect(TokenKind::Impl)?;
        let trait_name = self.parse_type_ident()?;
        self.expect(TokenKind::For)?;
        let for_type = self.parse_type_expr()?;

        // Optional where clause
        let where_clause = if self.match_token(TokenKind::Where).is_some() {
            let mut constraints = Vec::new();
            loop {
                let start = self.current().span.start;
                let trait_name = self.parse_type_ident()?;
                self.expect(TokenKind::Lt)?;
                let type_arg = self.parse_ident_any()?;
                self.expect(TokenKind::Gt)?;
                let span = self.span_from(start);
                constraints.push(Constraint::new(trait_name, type_arg, span));

                if !self.match_token(TokenKind::Comma).is_some() {
                    break;
                }
            }
            constraints
        } else {
            Vec::new()
        };

        self.expect(TokenKind::LBrace)?;

        let mut methods = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let method = self.parse_fn_in_impl()?;
            methods.push(method);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Decl::Impl {
            trait_name,
            for_type,
            where_clause,
            methods,
        })
    }

    /// Parse function declaration: fn name(params) => body;
    fn parse_fn_decl(&mut self, is_pub: bool) -> ParseResult<Decl> {
        let start = self.current().span.start;
        self.expect(TokenKind::Fn)?;
        let name = self.parse_ident()?;
        let (params, return_ty, body) = self.parse_fn_body()?;
        self.expect(TokenKind::Semi)?;
        let span = self.span_from(start);

        Ok(Decl::Fn(FnDecl::new(is_pub, name, params, return_ty, body, span)))
    }

    /// Parse function inside impl block (with semicolon).
    fn parse_fn_in_impl(&mut self) -> ParseResult<Spanned<FnDecl>> {
        let start = self.current().span.start;
        self.expect(TokenKind::Fn)?;
        let name = self.parse_ident()?;
        let (params, return_ty, body) = self.parse_fn_body()?;
        self.expect(TokenKind::Semi)?;
        let span = self.span_from(start);

        Ok(Spanned::new(
            FnDecl::new(false, name, params, return_ty, body, span),
            span,
        ))
    }

    /// Parse function body (params and body).
    fn parse_fn_body(&mut self) -> ParseResult<(Vec<Param>, Option<Spanned<TypeExpr>>, FnBody)> {
        self.expect(TokenKind::LParen)?;

        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                let param = self.parse_param()?;
                params.push(param);
                if !self.match_token(TokenKind::Comma).is_some() {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.match_token(TokenKind::Colon).is_some() {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let body = if self.match_token(TokenKind::FatArrow).is_some() {
            let expr = self.parse_expr()?;
            FnBody::Expr(expr)
        } else if self.check(TokenKind::Pipe) {
            let mut guards = Vec::new();
            while self.match_token(TokenKind::Pipe).is_some() {
                let start = self.current().span.start;
                let guard = self.parse_expr()?;
                self.expect(TokenKind::FatArrow)?;
                let body = self.parse_expr()?;
                let span = self.span_from(start);
                guards.push(GuardedExpr::new(guard, body, span));
            }
            FnBody::Guards(guards)
        } else {
            return Err(ParseError::unexpected("'=>' or '|'", self.current()));
        };

        Ok((params, return_ty, body))
    }

    /// Parse a function parameter.
    fn parse_param(&mut self) -> ParseResult<Param> {
        let start = self.current().span.start;
        let pattern = self.parse_pattern()?;

        let ty = if self.match_token(TokenKind::Colon).is_some() {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Param::new(pattern, ty, span))
    }

    /// Parse let declaration: let x = expr;
    fn parse_let_decl(&mut self, is_pub: bool) -> ParseResult<Decl> {
        self.expect(TokenKind::Let)?;
        let is_mut = self.match_token(TokenKind::Mut).is_some();
        let pattern = self.parse_pattern()?;

        let ty = if self.match_token(TokenKind::Colon).is_some() {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semi)?;

        Ok(Decl::Let {
            is_pub,
            is_mut,
            pattern,
            ty,
            value,
        })
    }

    /// Parse a type expression.
    fn parse_type_expr(&mut self) -> ParseResult<Spanned<TypeExpr>> {
        self.parse_type_fn()
    }

    /// Parse function type: A -> B
    fn parse_type_fn(&mut self) -> ParseResult<Spanned<TypeExpr>> {
        let start = self.current().span.start;
        let mut ty = self.parse_type_app()?;

        while self.match_token(TokenKind::Arrow).is_some() {
            let result = self.parse_type_fn()?;
            let span = self.span_from(start);
            ty = Spanned::new(
                TypeExpr::Function {
                    param: Box::new(ty),
                    result: Box::new(result),
                },
                span,
            );
        }

        Ok(ty)
    }

    /// Parse type application: Option<Int>
    fn parse_type_app(&mut self) -> ParseResult<Spanned<TypeExpr>> {
        let start = self.current().span.start;
        let base = self.parse_type_primary()?;

        if self.match_token(TokenKind::Lt).is_some() {
            if let TypeExpr::Named(name) = base.node.clone() {
                let mut args = vec![self.parse_type_expr()?];
                while self.match_token(TokenKind::Comma).is_some() {
                    args.push(self.parse_type_expr()?);
                }
                self.expect(TokenKind::Gt)?;
                let span = self.span_from(start);
                return Ok(Spanned::new(TypeExpr::Apply { base: name, args }, span));
            }
        }

        Ok(base)
    }

    /// Parse primary type expression.
    fn parse_type_primary(&mut self) -> ParseResult<Spanned<TypeExpr>> {
        let start = self.current().span.start;

        match self.peek() {
            // Unit type: ()
            TokenKind::LParen if self.tokens.get(self.pos + 1).map(|t| t.kind) == Some(TokenKind::RParen) => {
                self.advance();
                self.advance();
                let span = self.span_from(start);
                Ok(Spanned::new(TypeExpr::Unit, span))
            }

            // Tuple or grouped type: (A, B) or (A)
            TokenKind::LParen => {
                self.advance();
                let first = self.parse_type_expr()?;

                if self.match_token(TokenKind::Comma).is_some() {
                    // Tuple type
                    let mut elements = vec![first];
                    elements.push(self.parse_type_expr()?);
                    while self.match_token(TokenKind::Comma).is_some() {
                        elements.push(self.parse_type_expr()?);
                    }
                    self.expect(TokenKind::RParen)?;
                    let span = self.span_from(start);
                    Ok(Spanned::new(TypeExpr::Tuple { elements }, span))
                } else {
                    // Grouped type
                    self.expect(TokenKind::RParen)?;
                    Ok(first)
                }
            }

            // List type: [A]
            TokenKind::LBracket => {
                self.advance();
                let element = self.parse_type_expr()?;
                self.expect(TokenKind::RBracket)?;
                let span = self.span_from(start);
                Ok(Spanned::new(
                    TypeExpr::List {
                        element: Box::new(element),
                    },
                    span,
                ))
            }

            // Type name or type variable
            TokenKind::TypeIdent => {
                let name = self.parse_type_ident()?;
                let span = self.span_from(start);
                Ok(Spanned::new(TypeExpr::Named(name), span))
            }

            TokenKind::Ident => {
                let name = self.parse_ident()?;
                let span = self.span_from(start);
                Ok(Spanned::new(TypeExpr::Var(name), span))
            }

            _ => Err(ParseError::unexpected("type", self.current())),
        }
    }

    /// Parse an expression.
    fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        match self.peek() {
            TokenKind::Let => self.parse_let_expr(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::Match => self.parse_match_expr(),
            TokenKind::Fn => self.parse_lambda_expr(),
            TokenKind::Do => self.parse_do_expr(),
            _ => self.parse_assign_expr(),
        }
    }

    /// Parse let expression: let x = e
    fn parse_let_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        self.expect(TokenKind::Let)?;
        let is_mut = self.match_token(TokenKind::Mut).is_some();
        let pattern = self.parse_pattern()?;

        let ty = if self.match_token(TokenKind::Colon).is_some() {
            Some(Box::new(self.parse_type_expr()?))
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;

        let span = self.span_from(start);
        Ok(Spanned::new(
            Expr::Let {
                is_mut,
                pattern: Box::new(pattern),
                ty,
                value: Box::new(value),
            },
            span,
        ))
    }

    /// Parse if expression: if cond then a else b
    fn parse_if_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        self.expect(TokenKind::If)?;
        let condition = self.parse_expr()?;
        self.expect(TokenKind::Then)?;
        let then_branch = self.parse_expr()?;
        self.expect(TokenKind::Else)?;
        let else_branch = self.parse_expr()?;

        let span = self.span_from(start);
        Ok(Spanned::new(
            Expr::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            },
            span,
        ))
    }

    /// Parse match expression: match e with | p1 -> e1 | p2 -> e2 end
    fn parse_match_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        self.expect(TokenKind::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect(TokenKind::With)?;

        let mut arms = Vec::new();
        while self.match_token(TokenKind::Pipe).is_some() {
            let arm_start = self.current().span.start;
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Arrow)?;
            let body = self.parse_expr()?;
            let arm_span = self.span_from(arm_start);
            arms.push(MatchArm::new(pattern, body, arm_span));
        }

        self.expect(TokenKind::End)?;

        let span = self.span_from(start);
        Ok(Spanned::new(
            Expr::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            span,
        ))
    }

    /// Parse lambda expression: fn (x, y) => x + y
    fn parse_lambda_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        self.expect(TokenKind::Fn)?;
        self.expect(TokenKind::LParen)?;

        let mut params = Vec::new();
        if !self.check(TokenKind::RParen) {
            loop {
                let param = self.parse_param()?;
                params.push(param);
                if !self.match_token(TokenKind::Comma).is_some() {
                    break;
                }
            }
        }
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.match_token(TokenKind::Colon).is_some() {
            Some(Box::new(self.parse_type_expr()?))
        } else {
            None
        };

        self.expect(TokenKind::FatArrow)?;
        let body = self.parse_expr()?;

        let span = self.span_from(start);
        Ok(Spanned::new(
            Expr::Lambda {
                params,
                return_ty,
                body: Box::new(body),
            },
            span,
        ))
    }

    /// Parse do expression: do { stmt; stmt; expr }
    fn parse_do_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        self.expect(TokenKind::Do)?;
        self.expect(TokenKind::LBrace)?;

        let mut body = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            let expr = self.parse_expr()?;
            body.push(expr);
            // Optional semicolon between expressions
            self.match_token(TokenKind::Semi);
        }

        self.expect(TokenKind::RBrace)?;

        let span = self.span_from(start);
        Ok(Spanned::new(Expr::Do { body }, span))
    }

    /// Parse assignment expression: target = value
    fn parse_assign_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let left = self.parse_or_expr()?;

        if self.match_token(TokenKind::Eq).is_some() {
            let right = self.parse_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Assign {
                    target: Box::new(left),
                    value: Box::new(right),
                },
                span,
            ));
        }

        Ok(left)
    }

    /// Parse or expression: a || b
    fn parse_or_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let mut left = self.parse_and_expr()?;

        while self.match_token(TokenKind::OrOr).is_some() {
            let op_span = self.tokens[self.pos - 1].span;
            let right = self.parse_and_expr()?;
            let span = self.span_from(start);
            left = Spanned::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: Spanned::new(BinOp::Or, op_span),
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    /// Parse and expression: a && b
    fn parse_and_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let mut left = self.parse_eq_expr()?;

        while self.match_token(TokenKind::AndAnd).is_some() {
            let op_span = self.tokens[self.pos - 1].span;
            let right = self.parse_eq_expr()?;
            let span = self.span_from(start);
            left = Spanned::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: Spanned::new(BinOp::And, op_span),
                    right: Box::new(right),
                },
                span,
            );
        }

        Ok(left)
    }

    /// Parse equality expression: a == b, a != b
    fn parse_eq_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let left = self.parse_cmp_expr()?;

        let op = if self.match_token(TokenKind::EqEq).is_some() {
            Some(BinOp::Eq)
        } else if self.match_token(TokenKind::BangEq).is_some() {
            Some(BinOp::Ne)
        } else {
            None
        };

        if let Some(op) = op {
            let op_span = self.tokens[self.pos - 1].span;
            let right = self.parse_cmp_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: Spanned::new(op, op_span),
                    right: Box::new(right),
                },
                span,
            ));
        }

        Ok(left)
    }

    /// Parse comparison expression: a < b, a <= b, etc.
    fn parse_cmp_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let left = self.parse_cons_expr()?;

        let op = if self.match_token(TokenKind::Lt).is_some() {
            Some(BinOp::Lt)
        } else if self.match_token(TokenKind::Le).is_some() {
            Some(BinOp::Le)
        } else if self.match_token(TokenKind::Gt).is_some() {
            Some(BinOp::Gt)
        } else if self.match_token(TokenKind::Ge).is_some() {
            Some(BinOp::Ge)
        } else {
            None
        };

        if let Some(op) = op {
            let op_span = self.tokens[self.pos - 1].span;
            let right = self.parse_cons_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: Spanned::new(op, op_span),
                    right: Box::new(right),
                },
                span,
            ));
        }

        Ok(left)
    }

    /// Parse cons/concat expression: a :: b, a ++ b (right-associative)
    fn parse_cons_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let left = self.parse_add_expr()?;

        let op = if self.match_token(TokenKind::ColonColon).is_some() {
            Some(BinOp::Cons)
        } else if self.match_token(TokenKind::PlusPlus).is_some() {
            Some(BinOp::Concat)
        } else {
            None
        };

        if let Some(op) = op {
            let op_span = self.tokens[self.pos - 1].span;
            // Right-associative: recurse
            let right = self.parse_cons_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: Spanned::new(op, op_span),
                    right: Box::new(right),
                },
                span,
            ));
        }

        Ok(left)
    }

    /// Parse additive expression: a + b, a - b
    fn parse_add_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let mut left = self.parse_mul_expr()?;

        loop {
            let op = if self.match_token(TokenKind::Plus).is_some() {
                Some(BinOp::Add)
            } else if self.match_token(TokenKind::Minus).is_some() {
                Some(BinOp::Sub)
            } else {
                None
            };

            if let Some(op) = op {
                let op_span = self.tokens[self.pos - 1].span;
                let right = self.parse_mul_expr()?;
                let span = self.span_from(start);
                left = Spanned::new(
                    Expr::Binary {
                        left: Box::new(left),
                        op: Spanned::new(op, op_span),
                        right: Box::new(right),
                    },
                    span,
                );
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse multiplicative expression: a * b, a / b, a % b
    fn parse_mul_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = if self.match_token(TokenKind::Star).is_some() {
                Some(BinOp::Mul)
            } else if self.match_token(TokenKind::Slash).is_some() {
                Some(BinOp::Div)
            } else if self.match_token(TokenKind::Percent).is_some() {
                Some(BinOp::Mod)
            } else {
                None
            };

            if let Some(op) = op {
                let op_span = self.tokens[self.pos - 1].span;
                let right = self.parse_unary_expr()?;
                let span = self.span_from(start);
                left = Spanned::new(
                    Expr::Binary {
                        left: Box::new(left),
                        op: Spanned::new(op, op_span),
                        right: Box::new(right),
                    },
                    span,
                );
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse unary expression: -x, !x
    fn parse_unary_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;

        if self.match_token(TokenKind::Minus).is_some() {
            let op_span = self.tokens[self.pos - 1].span;
            let operand = self.parse_unary_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Unary {
                    op: Spanned::new(UnaryOp::Neg, op_span),
                    operand: Box::new(operand),
                },
                span,
            ));
        }

        if self.match_token(TokenKind::Bang).is_some() {
            let op_span = self.tokens[self.pos - 1].span;
            let operand = self.parse_unary_expr()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Expr::Unary {
                    op: Spanned::new(UnaryOp::Not, op_span),
                    operand: Box::new(operand),
                },
                span,
            ));
        }

        self.parse_postfix_expr()
    }

    /// Parse postfix expression: f(x), obj.field, arr[i]
    fn parse_postfix_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;
        let mut expr = self.parse_primary_expr()?;

        loop {
            if self.match_token(TokenKind::LParen).is_some() {
                // Function call
                let mut args = Vec::new();
                if !self.check(TokenKind::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.match_token(TokenKind::Comma).is_some() {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RParen)?;
                let span = self.span_from(start);
                expr = Spanned::new(
                    Expr::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                );
            } else if self.match_token(TokenKind::Dot).is_some() {
                // Field access
                let field = self.parse_ident()?;
                let span = self.span_from(start);
                expr = Spanned::new(
                    Expr::Field {
                        object: Box::new(expr),
                        field,
                    },
                    span,
                );
            } else if self.match_token(TokenKind::LBracket).is_some() {
                // Index access
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                let span = self.span_from(start);
                expr = Spanned::new(
                    Expr::Index {
                        object: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse primary expression.
    fn parse_primary_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        let start = self.current().span.start;

        match self.peek() {
            // Literals
            TokenKind::Int => {
                let token = self.advance();
                let value: i64 = token.lexeme.parse().unwrap();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::Int(value)), span))
            }
            TokenKind::Float => {
                let token = self.advance();
                let value: f64 = token.lexeme.parse().unwrap();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::Float(value)), span))
            }
            TokenKind::String => {
                let token = self.advance();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::String(token.lexeme.clone())), span))
            }
            TokenKind::Char => {
                let token = self.advance();
                let ch = token.lexeme.chars().next().unwrap();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::Char(ch)), span))
            }
            TokenKind::True => {
                let token = self.advance();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::Bool(true)), span))
            }
            TokenKind::False => {
                let token = self.advance();
                let span = token.span;
                Ok(Spanned::new(Expr::Lit(Literal::Bool(false)), span))
            }

            // Identifier
            TokenKind::Ident => {
                let name = self.parse_ident()?;
                let span = name.span;
                Ok(Spanned::new(Expr::Var(name), span))
            }

            // Type identifier (constructor)
            TokenKind::TypeIdent => {
                let name = self.parse_type_ident()?;

                // Check for record construction: Type { field: value }
                if self.check(TokenKind::LBrace) {
                    self.advance();
                    let mut fields = Vec::new();

                    if !self.check(TokenKind::RBrace) {
                        loop {
                            let field_start = self.current().span.start;
                            let field_name = self.parse_ident()?;
                            self.expect(TokenKind::Colon)?;
                            let value = self.parse_expr()?;
                            let field_span = self.span_from(field_start);
                            fields.push(FieldInit::new(field_name, value, field_span));

                            if !self.match_token(TokenKind::Comma).is_some() {
                                break;
                            }
                            if self.check(TokenKind::RBrace) {
                                break;
                            }
                        }
                    }

                    self.expect(TokenKind::RBrace)?;
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Expr::Record { name, fields }, span));
                }

                let span = name.span;
                Ok(Spanned::new(Expr::Var(name), span))
            }

            // Parenthesized expression, unit, or tuple
            TokenKind::LParen => {
                self.advance();

                // Unit: ()
                if self.match_token(TokenKind::RParen).is_some() {
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Expr::Lit(Literal::Unit), span));
                }

                let first = self.parse_expr()?;

                // Tuple: (a, b, ...)
                if self.match_token(TokenKind::Comma).is_some() {
                    let mut elements = vec![first];
                    elements.push(self.parse_expr()?);
                    while self.match_token(TokenKind::Comma).is_some() {
                        elements.push(self.parse_expr()?);
                    }
                    self.expect(TokenKind::RParen)?;
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Expr::Tuple { elements }, span));
                }

                // Grouped expression: (e)
                self.expect(TokenKind::RParen)?;
                let span = self.span_from(start);
                Ok(Spanned::new(Expr::Paren { inner: Box::new(first) }, span))
            }

            // List: [a, b, c]
            TokenKind::LBracket => {
                self.advance();

                let mut elements = Vec::new();
                if !self.check(TokenKind::RBracket) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.match_token(TokenKind::Comma).is_some() {
                            break;
                        }
                    }
                }

                self.expect(TokenKind::RBracket)?;
                let span = self.span_from(start);
                Ok(Spanned::new(Expr::List { elements }, span))
            }

            // Record update: { base | field: value }
            TokenKind::LBrace => {
                self.advance();
                let base = self.parse_expr()?;
                self.expect(TokenKind::Pipe)?;

                let mut updates = Vec::new();
                loop {
                    let field_start = self.current().span.start;
                    let name = self.parse_ident()?;
                    self.expect(TokenKind::Colon)?;
                    let value = self.parse_expr()?;
                    let field_span = self.span_from(field_start);
                    updates.push(FieldInit::new(name, value, field_span));

                    if !self.match_token(TokenKind::Comma).is_some() {
                        break;
                    }
                }

                self.expect(TokenKind::RBrace)?;
                let span = self.span_from(start);
                Ok(Spanned::new(
                    Expr::RecordUpdate {
                        base: Box::new(base),
                        updates,
                    },
                    span,
                ))
            }

            _ => Err(ParseError::unexpected("expression", self.current())),
        }
    }

    /// Parse a pattern.
    fn parse_pattern(&mut self) -> ParseResult<Spanned<Pattern>> {
        self.parse_pattern_or()
    }

    /// Parse or-pattern: p1 | p2
    fn parse_pattern_or(&mut self) -> ParseResult<Spanned<Pattern>> {
        let start = self.current().span.start;
        let left = self.parse_pattern_cons()?;

        if self.match_token(TokenKind::Pipe).is_some() {
            let right = self.parse_pattern_or()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Pattern::Or {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            ));
        }

        Ok(left)
    }

    /// Parse cons pattern: h :: t
    fn parse_pattern_cons(&mut self) -> ParseResult<Spanned<Pattern>> {
        let start = self.current().span.start;
        let head = self.parse_pattern_primary()?;

        if self.match_token(TokenKind::ColonColon).is_some() {
            let tail = self.parse_pattern_cons()?;
            let span = self.span_from(start);
            return Ok(Spanned::new(
                Pattern::Cons {
                    head: Box::new(head),
                    tail: Box::new(tail),
                },
                span,
            ));
        }

        Ok(head)
    }

    /// Parse primary pattern.
    fn parse_pattern_primary(&mut self) -> ParseResult<Spanned<Pattern>> {
        let start = self.current().span.start;

        match self.peek() {
            TokenKind::Underscore => {
                self.advance();
                let span = self.span_from(start);
                Ok(Spanned::new(Pattern::Wildcard, span))
            }
            TokenKind::Int => {
                let token = self.advance();
                let value: i64 = token.lexeme.parse().unwrap();
                let span = token.span;
                Ok(Spanned::new(Pattern::Lit(Literal::Int(value)), span))
            }
            TokenKind::Float => {
                let token = self.advance();
                let value: f64 = token.lexeme.parse().unwrap();
                let span = token.span;
                Ok(Spanned::new(Pattern::Lit(Literal::Float(value)), span))
            }
            TokenKind::String => {
                let token = self.advance();
                let span = token.span;
                Ok(Spanned::new(Pattern::Lit(Literal::String(token.lexeme.clone())), span))
            }
            TokenKind::Char => {
                let token = self.advance();
                let ch = token.lexeme.chars().next().unwrap();
                let span = token.span;
                Ok(Spanned::new(Pattern::Lit(Literal::Char(ch)), span))
            }
            TokenKind::True => {
                self.advance();
                let span = self.span_from(start);
                Ok(Spanned::new(Pattern::Lit(Literal::Bool(true)), span))
            }
            TokenKind::False => {
                self.advance();
                let span = self.span_from(start);
                Ok(Spanned::new(Pattern::Lit(Literal::Bool(false)), span))
            }
            TokenKind::Ident => {
                let name = self.parse_ident()?;
                let span = name.span;
                Ok(Spanned::new(Pattern::Var(name), span))
            }
            TokenKind::TypeIdent => {
                let name = self.parse_type_ident()?;

                if self.match_token(TokenKind::LParen).is_some() {
                    let mut args = Vec::new();
                    if !self.check(TokenKind::RParen) {
                        loop {
                            args.push(self.parse_pattern()?);
                            if !self.match_token(TokenKind::Comma).is_some() {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Pattern::Constructor { name, args }, span));
                }

                if self.check(TokenKind::LBrace) {
                    self.advance();
                    let mut fields = Vec::new();

                    if !self.check(TokenKind::RBrace) {
                        loop {
                            let field_start = self.current().span.start;
                            let field_name = self.parse_ident()?;

                            let pattern = if self.match_token(TokenKind::Colon).is_some() {
                                Some(self.parse_pattern()?)
                            } else {
                                None
                            };

                            let field_span = self.span_from(field_start);
                            fields.push(FieldPattern::new(field_name, pattern, field_span));

                            if !self.match_token(TokenKind::Comma).is_some() {
                                break;
                            }
                            if self.check(TokenKind::RBrace) {
                                break;
                            }
                        }
                    }

                    self.expect(TokenKind::RBrace)?;
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Pattern::Record { name, fields }, span));
                }

                let span = self.span_from(start);
                Ok(Spanned::new(Pattern::Constructor { name, args: Vec::new() }, span))
            }
            TokenKind::LParen => {
                self.advance();

                // Unit: ()
                if self.match_token(TokenKind::RParen).is_some() {
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Pattern::Lit(Literal::Unit), span));
                }

                let first = self.parse_pattern()?;

                // Tuple: (a, b, ...)
                if self.match_token(TokenKind::Comma).is_some() {
                    let mut elements = vec![first];
                    elements.push(self.parse_pattern()?);
                    while self.match_token(TokenKind::Comma).is_some() {
                        elements.push(self.parse_pattern()?);
                    }
                    self.expect(TokenKind::RParen)?;
                    let span = self.span_from(start);
                    return Ok(Spanned::new(Pattern::Tuple { elements }, span));
                }

                // Grouped pattern
                self.expect(TokenKind::RParen)?;
                Ok(first)
            }
            TokenKind::LBracket => {
                self.advance();

                let mut elements = Vec::new();
                if !self.check(TokenKind::RBracket) {
                    loop {
                        elements.push(self.parse_pattern()?);
                        if !self.match_token(TokenKind::Comma).is_some() {
                            break;
                        }
                    }
                }

                self.expect(TokenKind::RBracket)?;
                let span = self.span_from(start);
                Ok(Spanned::new(Pattern::List { elements }, span))
            }

            _ => Err(ParseError::unexpected("pattern", self.current())),
        }
    }

    /// Parse a lowercase identifier.
    fn parse_ident(&mut self) -> ParseResult<Ident> {
        if self.check(TokenKind::Ident) {
            let token = self.advance();
            Ok(Ident::new(&token.lexeme, token.span))
        } else {
            Err(ParseError::unexpected("identifier", self.current()))
        }
    }

    /// Parse an uppercase identifier.
    fn parse_type_ident(&mut self) -> ParseResult<Ident> {
        if self.check(TokenKind::TypeIdent) {
            let token = self.advance();
            Ok(Ident::new(&token.lexeme, token.span))
        } else {
            Err(ParseError::unexpected("type name", self.current()))
        }
    }

    /// Parse any identifier.
    fn parse_ident_any(&mut self) -> ParseResult<Ident> {
        if self.check(TokenKind::Ident) || self.check(TokenKind::TypeIdent) {
            let token = self.advance();
            Ok(Ident::new(&token.lexeme, token.span))
        } else {
            Err(ParseError::unexpected("identifier", self.current()))
        }
    }
}
