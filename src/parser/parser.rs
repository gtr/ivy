use crate::lexer::tokens::*;
use crate::parser::ast::*;
use crate::consume_token;
use crate::errors::errors::*;

/// Parser encapsulates the recursive-descent parser for ivy.
#[derive(Default)]
pub struct Parser {
  tokens: Vec<Token>,
  last:   Option<(usize, usize)>,
  cursor: usize,
}

pub fn parse(tokens: Vec<Token>) -> Result<Node, IvyError>{
  let mut p = Parser::new(tokens);
  p.parse()
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Parser {
    Parser { tokens, cursor: 0 , last: None}
  }

  fn parse(&mut self) -> Result<Node, IvyError> {
    let mut nodes = Vec::new();

    while !self.is_done() {
      nodes.push( self.parse_statement()? );
    }

    Ok(NewRootNode(nodes))
  }

  /// Parses a statement based on the following rule:
  /// 
  /// <statement> ::= <expression> ';' ;
  /// 
  fn parse_statement(&mut self) -> Result<Node, IvyError> {
    let expr = self.parse_expression()?;
    consume_token!(self, TokenType::Semicolon)?;

    Ok(expr)
  }

  /// Parses an expression based on the following rule:
  /// 
  /// <expression> ::= <letExpr>      
  ///                | <mutExpr>      
  ///                | <fnExpr>       
  ///                | <ifExpr>       
  ///                | <pubExpr>      
  ///                | <dataExpr>     
  ///                | <structStmt>   
  ///                | <packageStmt>  
  ///                | <importStmt>   
  ///                | <matchExpr>    
  ///                | <whileExpr>    
  ///                | <doExpr>       
  ///                | <returnExpr>   
  ///                | <or>           
  ///                | <tupleAny> ;   
  ///
  fn parse_expression(&mut self) -> Result<Node, IvyError> {
    match self.peek() {
      Some(tok) => {
        match tok.typ {
          TokenType::Let      => self.parse_let(),
          TokenType::Mut      => self.parse_mut(),
          TokenType::Fn       => self.parse_function(),
          TokenType::If       => self.parse_if(),
          TokenType::Pub      => self.parse_pub(),
          TokenType::Data     => self.parse_data(),
          TokenType::Struct   => self.parse_struct(),
          TokenType::Package  => self.parse_package(),
          TokenType::Import   => self.parse_import(),
          TokenType::Match    => self.parse_match(),
          TokenType::While    => self.parse_while(),
          TokenType::Do       => self.parse_do(),
          TokenType::Return   => self.parse_return(),
          TokenType::Trait    => self.parse_trait(),
          TokenType::Impl     => self.parse_impl(),
          _                   => self.parse_or(),
        }
      }
      _ => {
        let (r, c) = self.get_last_token_location();
        Err(new_parser_expected(r, c + 1, format!("<expression>")))
      }
    }
  }

  /// Parses a let expression based on the following pattern:
  /// 
  /// <letExpr> ::= 'let' [ 'mut' ]? [ <symbol> | <tupleSymbols> ] 
  ///                                [ '::' <typeFn> ]? '=' <expression>;
  /// 
  fn parse_let(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Let)?;
    let is_mut = if let Some(tok) = self.peek() {
      if tok.typ == TokenType::Mut {
        consume_token!(self, TokenType::Mut)?;
        true
      } else { false }
    } else { false };
    let mut symbols = Vec::new();
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::LParen => {
          let mut first = true;
          consume_token!(self, TokenType::LParen)?;
          while let Some(tok2) = self.peek() {
            match tok2.typ {
              TokenType::RParen => { break },
              _ => {
                if !first { consume_token!(self, TokenType::Comma)?; }
                symbols.push(self.parse_symbol()?);
                first = false;
              }
            };
          };
          consume_token!(self, TokenType::RParen)?;
        },
        TokenType::Symbol(_) => {
          symbols.push(self.parse_symbol()?);
        },
        _ => return Err(new_parser_expected_one_of(tok.row, tok.col, vec![
          format!("<symbol>"), format!("`(`")
        ]))
      };
    };

    let ttype = if let Some(tok2) = self.peek() {
      if tok2.typ == TokenType::DoubleColon {
        consume_token!(self, TokenType::DoubleColon)?;
        Some(self.parse_type_fn()?)
      } else {
        None
      }
    } else { None };

    consume_token!(self, TokenType::Bind)?;

    let rhs = self.parse_expression()?;

    if is_mut {
      Ok(NewLetMutExpr(token, symbols, rhs, ttype))
    } else {
      Ok(NewLetExpr(token, symbols, rhs, ttype))
    }
  }

  /// Parses a mut expression based on the following pattern:
  /// 
  /// <mutExpr> ::= 'mut' [ <symbol> | <access> ] '=' <expression> ;
  /// 
  fn parse_mut(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Mut)?;
    let lhs = self.parse_access_attr()?;
    consume_token!(self, TokenType::Bind)?;
    let rhs = self.parse_expression()?;
    
    Ok(NewMutExpr(token, lhs, rhs))
  }

  /// Parses a function expression/declaration based on the folllwing pattern:
  /// 
  /// <fnExpr> ::= <fnAnon> | <fnSignature> | <fnDeclaration> ;
  /// 
  fn parse_function(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek_twice() {
      match tok.typ {
        TokenType::LParen => self.parse_fn_anon(),
        TokenType::Symbol(_) => {
          if let Some(tok2) = self.peek_thrice() {
            match tok2.typ {
              TokenType::DoubleColon => self.parse_fn_signature(),
              TokenType::LParen => self.parse_fn_declaration(),
              _ => {
                self.next();
                self.next();
                Err(new_parser_expected_one_of(tok2.row, tok2.col, vec!(
                  format!("`::`"), format!("`(`"))
                ))
              }
            }
          } else {
            self.next();
            self.next();
            let (r, c) = self.get_last_token_location();
            Err(new_parser_expected_one_of(r, c + 1, vec!(
              format!("`::`"), format!("`(`"))
            ))
          }
        },
        _ => Err(new_parser_expected_one_of(tok.row, tok.col, vec![
          format!("<symbol>"), format!("`(`")
        ]))
      }
    } else { 
      self.next();
      let (r, c) = self.get_last_token_location();
      Err(new_parser_expected_one_of(r, c + 2, vec!(
        format!("<symbol>"), format!("`(`"))
      ))
    }
  }


  /// Parses a tuple of function arguments based on the following rule:
  /// 
  /// <fnAnon> ::= 'fn' <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;
  /// 
  fn parse_fn_anon(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Fn)?;
    let arguments = self.parse_fn_args()?;
    let type_out = if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Colon => {
          consume_token!(self, TokenType::Colon)?;
          Some(self.parse_type_fn()?)
        },
        _ => { None }
      }
    } else { None };
    
    consume_token!(self, TokenType::EqArrow)?;
    let rhs = self.parse_expression()?;

    Ok(NewFnAnon(token, arguments, type_out, rhs))
  }


  /// Parses a tuple of function arguments based on the following rule:
  /// 
  /// <fnSignature> ::= 'fn' <symbol> '::' <typeFn> ;
  /// 
  fn parse_fn_signature(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Fn)?;
    let symbol = self.parse_symbol()?;
    consume_token!(self, TokenType::DoubleColon)?;
    let ttype = self.parse_type_fn()?;

    Ok(NewFnSignature(token, symbol, ttype))
  }

  /// Parses a tuple of function arguments based on the following rule:
  /// 
  /// <fnDeclaration> ::= 'fn' <symbol> <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;
  /// 
  fn parse_fn_declaration(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Fn)?;
    let symbol = self.parse_symbol()?;
    let arguments = self.parse_fn_args()?;
    let type_out = if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Colon => {
          consume_token!(self, TokenType::Colon)?;
          Some(self.parse_type_fn()?)
        },
        _ => { None }
      }
    } else { None };
    
    consume_token!(self, TokenType::EqArrow)?;
    let rhs = self.parse_expression()?;

    Ok(NewFnDeclaration(token, symbol, rhs, arguments, type_out))
  }

  /// Parses a tuple of function arguments based on the following rule:
  /// 
  /// <fnArgs>  ::= '(' [ <fnArgsTyped> [ ',' <fnArgsTyped> ]* ]? ')' ;
  /// 
  /// Returns a vector of nodes rather than a single node.
  fn parse_fn_args(&mut self) -> Result<Vec<Node>, IvyError> {
    let mut args = Vec::new();
    let mut first = true;

    consume_token!(self, TokenType::LParen)?;
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break },
        _ => {
          if !first {
            consume_token!(self, TokenType::Comma)?;
          }
          args.push(self.parse_fn_arg()?);
          first = false;
        }
      };
    };
    consume_token!(self, TokenType::RParen)?;

    Ok(args)
  }

  /// Parses an if expression based on the following rule:
  /// 
  /// <fnArgsTyped>   ::= <symbol> [ ':' <typeFn> ] ? 
  ///                              | <listExpr> 
  ///                              | <integer>
  ///                              | <string> ;
  /// 
  fn parse_fn_arg(&mut self) -> Result <Node, IvyError> {
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::LBracket   => { return self.parse_list(); },
        TokenType::String(_)  => { return self.parse_string(); },
        TokenType::Integer(_) => { return self.parse_integer(); },
        TokenType::Symbol(_)  => {
          if let Some(tok2) = self.peek_twice() {
            if tok2.typ == TokenType::LParen {
              return self.parse_call();
            }
          }
        },
        _ => {},
      }
    };

    let symbol = self.parse_symbol()?;
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Colon => {
          self.next();
          Ok(NewFnArgTyped(symbol, Some(self.parse_type_fn()?)))
        },
        _ => { Ok(symbol) }
      }
    } else { Ok(symbol) }
  }

  /// Parses an if expression based on the following rule:
  /// 
  /// <ifExpr> ::= 'if' <or> 'then' <expression> [ 'else' <expression> ]? ;
  /// 
  fn parse_if(&mut self) -> Result <Node, IvyError> {
    let token = consume_token!(self, TokenType::If)?;
    let cond = self.parse_expression()?;
    consume_token!(self, TokenType::Then)?;
    let true_branch = self.parse_expression()?;
    let false_branch = if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Else => {
          consume_token!(self, TokenType::Else)?;
          Some(self.parse_expression()?)
        },
        _ => {None},
      }
    } else { None };
    Ok(NewIfExpr(token, cond, true_branch, false_branch))
  }

  fn parse_pub(&mut self) -> Result<Node, IvyError> {
    Ok (NewPubExpr(
      consume_token!(self, TokenType::Pub)?,
      self.parse_expression()?,
    ))
  }

  /// Parses a data declaration based on the following rule:
  /// 
  /// <dataExpr>      ::= 'data' <symbol> [ <dataGenerics> ]? '(' <dataVariants> ')' ;
  /// <dataVariants>  ::= [ '|' ]? <dataItem> [ '|' <dataItem> ]* ;
  /// 
  fn parse_data(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Data)?;
    let symbol = self.parse_symbol()?;
    let mut generics = Vec::new();
    let mut variants = Vec::new();
    
    if let Some(tok) = self.peek() {
      match tok.typ {
          TokenType::Less => {
            generics = self.parse_data_generics()?;
          },
          _ => {},
      };
    };
    
    let mut first = true;
    consume_token!(self, TokenType::LParen)?;
    while let Some(tok2) = self.peek() {
      if tok2.typ == TokenType::Bar {
        if first { consume_token!(self, TokenType::Bar)?; }
      };
      match tok2.typ {
        TokenType::RParen => { break },
        _ => { 
          if !first {  consume_token!(self, TokenType::Bar)?; }
          variants.push(self.parse_data_item()?);
          first = false;
        }
      }
    }

    consume_token!(self, TokenType::RParen)?;
    Ok(NewDataDelcaration(token, symbol, generics, variants))
  }

  /// Parses a generic data pattern based on the following rule:
  /// 
  /// <dataGenerics>  ::= '<' <symbol> [',' <symbol> ]* '>' ;
  /// 
  fn parse_data_generics(&mut self) -> Result<Vec<Node>, IvyError> {
    let mut first = true;
    let mut generics = Vec::new();
    consume_token!(self, TokenType::Less)?;

    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Greater => { break },
        _ => {
          if !first { 
            consume_token!(self, TokenType::Comma)?; 
          }
          generics.push(self.parse_symbol()?);
          first = false;
        }
      };
    };

    consume_token!(self, TokenType::Greater)?;
    Ok(generics)
  }

  /// Parses a data item pattern based on the following rule:
  /// 
  /// <dataItem>      ::= <symbol> [ '::' ( <typeFn> )]? ;
  /// 
  fn parse_data_item(&mut self) -> Result<Node, IvyError> {
    let symbol = self.parse_symbol()?;
    if let Some(tok) = self.peek() {
      if tok.typ == TokenType::DoubleColon {
        consume_token!(self, TokenType::DoubleColon)?;
        if let Some(tok2) = self.peek() {
          if tok2.typ == TokenType::Struct {
            Ok(NewDataItem(symbol, self.parse_struct_anon()?))
          } else {
            Ok(NewDataItem(symbol, self.parse_type_fn()?))
          }
        } else {
          Ok(NewDataItem(symbol, self.parse_type_fn()?))
        }
      } else {
        Ok(symbol)
      }
    } else {
      let (r, c) = self.get_last_token_location();
      Err(new_parser_expected(r, c, "`}`".to_string()))
    }
  }



  fn parse_struct(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek_twice() {
      match tok.typ {
        TokenType::Symbol(_) => {
          self.parse_struct_declaration()
        }
        _ => { self.parse_struct_anon()}
      }
    } else {
      self.parse_struct_declaration()
    }
  }

  /// Parses an anonymous struct expression based on the following rule:
  /// 
  /// <structAnon> ::= 'struct' '(' <structFields> ')' ;
  /// 
  fn parse_struct_anon(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Struct)?;
    consume_token!(self, TokenType::LParen)?;
    let mut fields = Vec::new();
    
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break },
        _ => {
          consume_token!(self, TokenType::At)?;
          fields.push(self.parse_struct_field()?);
        }
      }
    }

    consume_token!(self, TokenType::RParen)?;
    Ok(NewStructAnon(token, fields))
  }
  
  /// Parses a struct declaration based on the following rules:
  /// 
  /// <structDeclaration> ::= 'struct' <symbol> '(' <structFields> ')' ;
  /// <structFields>      ::= <structField> [ ',' <structField> ]* [ ',' ]? ;
  /// 
  fn parse_struct_declaration(&mut self) -> Result<Node, IvyError> {
    let struct_tok = consume_token!(self, TokenType::Struct)?;
    let name = self.parse_symbol()?;
    consume_token!(self, TokenType::LParen)?;
    let mut fields = Vec::new();
    
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break; },
        _ => {
          consume_token!(self, TokenType::At)?;
          fields.push(self.parse_struct_field()?);
        }
      }
    }
    
    consume_token!(self, TokenType::RParen)?;
    Ok(NewStructDeclaration(struct_tok, name, fields))
  }

  /// Parses a struct field pattern based on the following rule:
  /// 
  /// <structField>       ::= <symbol> '::' <typeFn> ;
  /// 
  fn parse_struct_field(&mut self) -> Result<Node, IvyError> {
    let symbol = self.parse_symbol()?;
    consume_token!(self, TokenType::DoubleColon)?;
    let ttype = self.parse_type_fn()?;
    
    Ok(NewStructField(symbol, ttype))
  }

  /// Parses a package statement based on the following rule:
  /// 
  /// <packageStmt>   ::= 'package' <symbol> ;
  /// 
  fn parse_package(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Package)?;

    Ok(NewPackage(self.parse_symbol()?, token))
  }

  /// Parses an import statement based on the following rule:
  /// 
  /// <importStmt>        ::= 'import' [ <string> | <tupleStrings> ] ;
  /// 
  fn parse_import(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Import)?;

    if let Some(tok) = self.peek() {
      if tok.typ == TokenType::LParen {
        consume_token!(self, TokenType::LParen)?;
        let mut imports = Vec::new();
        let mut first = true;
        while let Some(tok2) = self.peek() {
          match tok2.typ {
            TokenType::RParen => { break },
            _ => {
              if !first { consume_token!(self, TokenType::Comma)?; }
              imports.push(self.parse_string()?);
              first = false;
            }
          };
        };

        consume_token!(self, TokenType::RParen)?;
        return Ok(NewImport(imports, token))
      } else {
        Ok(NewImport(vec![self.parse_string()?], token))
      }
    } else {
      let (r, c) = self.get_last_token_location();
      return Err(new_parser_expected_one_of(r,c, vec![
        "(".to_string(), "<string>".to_string()
      ]));
    }
  }

  /// Parses a match expression based on the following rule:
  /// 
  /// <matchExpr> ::= 'match' <expression> 'with' '(' [ <matchBranch> ]* ')' ;
  /// 
  fn parse_match(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Match)?;
    let lhs = self.parse_expression()?;
    consume_token!(self, TokenType::With)?;
    let mut branches = Vec::new();
    consume_token!(self, TokenType::LParen)?;
    
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break },
        _ => { branches.push(self.parse_match_branch()?); }
      }   
    }
    
    consume_token!(self, TokenType::RParen)?;
    Ok(NewMatchExpression(token, lhs, branches))
  }


  /// Parses a match branch pattern based on the following rule:
  /// 
  /// <matchBranch>   ::= '|' <expression> '->' <expression>
  /// 
  fn parse_match_branch(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Bar)?;
    let lhs = self.parse_expression()?;
    consume_token!(self, TokenType::Arrow)?;
    let rhs = self.parse_expression()?;
    
    Ok(NewMatchBranch(token, lhs, rhs))
  }

  /// Parses a list pattern based on the following rule:
  /// 
  /// <listExpr>      ::= <listSplit> | <listLiteral> ;
  /// 
  fn parse_list(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek_twice() {
      match tok.typ {
        TokenType::RBracket => self.parse_list_literal(),
        TokenType::Symbol(_) => {
          if let Some(tok2) = self.peek_thrice() {
            match tok2.typ {
              TokenType::Bar => self.parse_list_split(),
              TokenType::RBracket | _ => self.parse_list_literal(),
            }
          } else { self.parse_list_literal() }
        },
        _ => self.parse_list_literal()
      }
    } else { self.parse_list_literal() }
  }

  /// Parses a list split pattern based on the following rule:
  /// 
  /// <listSplit>     ::= '[' <symbol> '|' <symbol> ']' ;
  /// 
  fn parse_list_split(&mut self) -> Result<Node, IvyError> {
    consume_token!(self, TokenType::LBracket)?;
    let h = self.parse_symbol()?;
    consume_token!(self, TokenType::Bar)?;
    let t = self.parse_symbol()?;
    consume_token!(self, TokenType::RBracket)?;
    
    Ok(NewListSplit(h, t))
  }

  /// Parses a list literal based on the following rule:
  /// 
  /// <listLiteral>   ::= '[' [ <listItems> ]? ']' ;
  /// <listItems>     ::= <expression> [ ',' <expression> ]* ;
  /// 
  fn parse_list_literal(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::LBracket)?;
    let mut items = Vec::new();
    let mut first = true;
    
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RBracket => { break }
        _ => {
          if !first {
            consume_token!(self, TokenType::Comma)?;
          }
          items.push(self.parse_expression()?);
          first = false;
        }
      }
    }
    
    consume_token!(self, TokenType::RBracket)?;
    Ok(NewListExpression(token, items))
  }

  /// Parses a while expression pattern based on the following rule:
  /// 
  /// <whileExpr>     ::= 'while' <or> '{' [ <statement> ]* '}' ;
  /// 
  fn parse_while(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::While)?;
    let cond = self.parse_expression()?;
    let mut stmts = Vec::new();
    consume_token!(self, TokenType::LCurly)?;
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RCurly => {
          // self.next();
          break;
        }
        _ => {}
      };
      stmts.push(self.parse_statement()?);
    }
    consume_token!(self, TokenType::RCurly)?;
    Ok(NewWhileExpression(token, cond, stmts))
  }

  /// Parses a do block based on the following rule:
  /// 
  /// <doExpr>        ::= 'do' '{' [ <statement> ]* '}' ;
  /// 
  fn parse_do(&mut self) -> Result<Node, IvyError> {
    let tok = consume_token!(self, TokenType::Do)?;
    consume_token!(self, TokenType::LCurly)?;
    let mut stmts = vec![];
    
    loop {
      match self.peek() {
        Some(tok) => match tok.typ {
          TokenType::RCurly => {
            self.next();
            break;
          },
          _ => {stmts.push(self.parse_statement()?)}
        }
        None => {
          let (r, c) = self.get_last_token_location();
          return Err(new_parser_expected(r, c, "`}`".to_string()))
        }
      }
    }
    
    Ok(NewDoExpression(tok, stmts))
  }

  /// Parses a return statement based on the following rule:
  /// 
  /// <returnExpr>    ::= 'return' <expression> ;
  /// 
  fn parse_return(&mut self) -> Result<Node, IvyError> {
    Ok(NewReturnExpression(consume_token!(
      self, TokenType::Return)?, 
      self.parse_expression()?
    ))
  }

  /// Parses a trait statement based on the following rule:
  /// 
  /// <traitStmt> ::= 'trait' <symbol> '(' [ <fnExpr> ';' ]* ')' ;
  /// 
  fn parse_trait(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Trait)?;
    let symbol = self.parse_symbol()?;
    consume_token!(self, TokenType::LParen)?;
    let mut funcs = Vec::new();
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Fn => {
          funcs.push(self.parse_function()?);
          consume_token!(self, TokenType::Semicolon)?;
        },
        _ => {
          break;
        }
      }
    }
    consume_token!(self, TokenType::RParen)?;
    Ok(NewTraitStmt(token, symbol, funcs))
  }

  /// <implExpr>  ::= 'impl' <symbol> 'for' <typeFn> '(' [ <fnExpr> ';' ]* ')' ;
  fn parse_impl(&mut self) -> Result<Node, IvyError> {
    let token = consume_token!(self, TokenType::Impl)?;
    let symbol = self.parse_symbol()?;
    consume_token!(self, TokenType::For)?;
    let datatype = self.parse_type_fn()?;
    consume_token!(self, TokenType::LParen)?;
    let mut funcs = Vec::new();
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Fn => {
          funcs.push(self.parse_function()?);
          consume_token!(self, TokenType::Semicolon)?;
        },
        _ => {
          break;
        }
      }
    }
    consume_token!(self, TokenType::RParen)?;
    Ok(NewImplStmt(token, datatype, symbol, funcs))
  }

  /// Parses a function type pattern based on the following rule:
  /// 
  /// <typeFn>    ::= <typeCmpst> [ '->' <typeCmpst> ]? ;
  /// 
  fn parse_type_fn(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_type_cmpst()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Arrow => {
          self.next();
          NewTypeFn(expr, self.parse_type_cmpst()?, tok)
        }
        _ => break,
      };
      expr = op;
    };

    Ok(expr)
  }

  /// Parses a tuple type pattern based on the following rule:
  /// 
  /// <typeCmpst> ::= <typeLst> | <symbol> '<' [ <typeLst> [ ',' <typeLst> ]* ] '>' ;
  /// 
  fn parse_type_cmpst(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek_twice() {
      if tok.typ == TokenType::Less {

        let name = self.parse_symbol()?;
        consume_token!(self, TokenType::Less)?;
        let mut types = Vec::new();
        let mut first = true;
        while let Some(tok2) = self.peek() {
          match tok2.typ {
            TokenType::Greater => { break; }
            _ => {
              if !first {
                consume_token!(self, TokenType::Comma)?;
              }
              types.push(self.parse_type_fn()?);
              first = false;
            }
          };
        };

        consume_token!(self, TokenType::Greater)?;
        return Ok(NewTypeCmpst(name, types))
      }
    };

    self.parse_type_lst()
  }

  /// Parses a tuple type pattern based on the following rule:
  /// 
  /// <typeLst>   ::= <typeTuple> | '[' <typeTuple> ']' ;
  ///  
  fn parse_type_lst(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      if tok.typ == TokenType::LBracket {
        let token = consume_token!(self, TokenType::LBracket)?;
        let ttype = self.parse_type_fn()?;
        consume_token!(self, TokenType::RBracket)?;
        return Ok(NewTypeLst(ttype, token));
      }
    }
    self.parse_type_tuple()
  }

  /// Parses a tuple type pattern based on the following rule:
  /// 
  /// <typeTuple> ::= <typeFn> | '(' <typeFn> [ ',' <typeFn> ]* ')' ;
  ///  
  fn parse_type_tuple(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      if tok.typ == TokenType::LParen {
        consume_token!(self, TokenType::LParen)?;

        let mut types = Vec::new();
        let mut first = true;
        while let Some(tok2) = self.peek() {
          match tok2.typ {
            TokenType::RParen => { break },
            _ => {
              if !first { consume_token!(self, TokenType::Comma)?; }
              types.push(self.parse_type_fn()?);
              first = false;
            }
          };
        };

        consume_token!(self, TokenType::RParen)?;
        return Ok(NewTypeTuple(types))
      }
    };

    self.parse_type()
  }

  /// Parses a tuple type pattern based on the following rul  e:
  /// 
  /// <type>      ::= [ 'mut' ]?  [ <symbol> | <typeFn> ] ;
  /// 
  fn parse_type(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Mut => {
          consume_token!(self, TokenType::Mut)?;
          Ok(NewTtype(self.parse_symbol()?, true))
        },
        TokenType::Symbol(_) => {
          Ok(NewTtype(self.parse_symbol()?, false))
        },
        _ => {
          Err(new_parser_expected_one_of(tok.row, tok.col, vec![
            "mut".to_string(), "<".to_string(),
          ]))
        }
      }
    } else {
      let (r, c) = self.get_last_token_location();
      return Err(new_parser_expected(r, c, "`}`".to_string()))
    }
  }

  /// Parses an and expression based on the following rule:
  /// 
  /// <or> ::= <and> [ '||' <and> ]* ;
  /// 
  fn parse_or(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_and()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Or => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_and()?)
        }
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses an and expression based on the following rule:
  /// 
  /// <and> ::= <equality> [ '&&' <equality> ]* ;
  /// 
  fn parse_and(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_equality()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::And => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_equality()?)
        }
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses an equality expression based on the following rule:
  /// 
  /// <equality>      ::= <comparison> [ [ '==' | '!=' ] <comparison> ]* ;
  /// 
  fn parse_equality(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_comparison()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Eq | TokenType::NotEq => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_comparison()?)
        } 
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses a comparison expression based on the following rule:
  /// 
  /// <comparison> ::= <addition> [ [ '>' 
  ///                              | '>=' 
  ///                              | '<' 
  ///                              | '<=' ] <addition> ]* ;
  /// 
  fn parse_comparison(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_addition()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Greater | TokenType::GreaterEqual
        | TokenType::Less |TokenType::LessEqual  => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_addition()?)
        } 
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses an addition expression based on the following rule:
  /// 
  /// <addition> ::= <mult> [ ( '+' | '-' | '++' ) <mult> ]* ;
  /// 
  fn parse_addition(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_mult()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Plus | TokenType::Minus | TokenType::PlusPlus => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_mult()?)
        }
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses a multiplication expression based on the following rule:
  /// 
  /// <mult> ::= <unary> [ ( '*' | '/' ) <unary> ]* ;
  /// 
  fn parse_mult(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_unary()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Star | TokenType::Slash => {
          self.next();
          NewBinaryExpression(tok, expr, self.parse_unary()?)
        } 
        _ => break,
      };
      expr = op;
    }

    Ok(expr)
  }

  /// Parses a unary expression based on the following rule:
  /// 
  /// <unary> ::= [ '!' | '-' ] <callExpr> | <callExpr> ;
  /// 
  fn parse_unary(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Not | TokenType::Minus  => {
          let token = consume_token!(self, TokenType::Not)?;
          Ok(NewUnaryExpression(token, self.parse_call()?))
        },
        _ => self.parse_call()
      }
    } else { self.parse_call() }
  }

  /// Parses an expression based on the following pattern:
  /// 
  /// <callExpr> ::= <accessAttr> | <accessAttr> <tuple> ;
  /// 
  fn parse_call(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::Symbol(_) => {
          if let Some(tok2) = self.peek_twice() {
            if tok2.typ == TokenType::LParen {
              let symbol = self.parse_symbol()?;
              let args = self.parse_tuple_vec()?;
              return Ok(NewCall(symbol, args))
            }
          }
        }
        _ => {}
      }
    };

    self.parse_access_attr()
  }

  /// Parses an access attribute expression based on the following pattern:
  /// 
  /// <accessAttr>    ::= <accessIndx> [ '.' <callExpr> ]* ;
  /// 
  fn parse_access_attr(&mut self) -> Result<Node, IvyError> {
    let mut expr = self.parse_access_indx()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::Dot => {
          self.next();
          NewAccess(expr, self.parse_call()?)
        }
        _ => break,
      };
      expr = op;
    };

    Ok(expr)
  }

  /// Parses an access index expression based on the following pattern:
  /// 
  /// <accessIndx>    ::= <factor> [ '[' <or> ']' ]* ;
  /// 
  fn parse_access_indx(&mut self) -> Result<Node, IvyError> {
    let mut lhs = self.parse_factor()?;

    while let Some(tok) = self.peek() {
      let op = match tok.typ {
        TokenType::LBracket => {
          self.next();
          let r = NewAccessIndex(lhs, self.parse_or()?);
          consume_token!(self, TokenType::RBracket)?;
          r
        }
        _ => break,
      };
      lhs = op;
    };

    Ok(lhs)
  }

  /// Parses a factor expression based on the following rule:
  /// 
  /// <factor>    ::= '(' [ <or> ]? ')' 
  ///               | <tuple> 
  ///               | <listExpr> 
  ///               | <atom> ;
  /// 
  fn parse_factor(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::LParen => {
          if self.is_tuple() { return self.parse_tuple() }
          consume_token!(self, TokenType::LParen)?;
          let expr = self.parse_expression();
          consume_token!(self, TokenType::RParen)?;
          expr
        },
        TokenType::Integer(_) | TokenType::Symbol(_)  | TokenType::String(_) => {
          self.next();
          Ok(NewAtom(tok))
        },
        TokenType::LBracket => {
          self.parse_list()
        }
        _ => Err(new_parser_expected(tok.row, tok.col, "<factor>".to_string()))
      }
    } else {
      let (r, c) = self.get_last_token_location();
      Err( new_parser_expected(r, c + 1, "<factor>".to_string()))
    }
  }

  /// Determines if the current `(` `)` pattern is a tuple (containing more 
  /// than one expression) or just a grouped expression.
  fn is_tuple(&mut self) -> bool {
    let mut cursor_temp = self.cursor + 1;
    let mut first = true;
    while cursor_temp < self.tokens.len() {
      match self.tokens[cursor_temp].typ {
        TokenType::RParen => { break },
        TokenType::Comma => { return true; }
        _ => { first = false; }
      };
      cursor_temp += 1;
    };

    false || first
  }

  /// Parses a tuple expression based on the following pattern:
  /// 
  /// <tuple> ::= '(' <expression> [ ',' <expression> ]* ')' ;
  /// 
  fn parse_tuple(&mut self) -> Result<Node, IvyError> {
    let mut exprs = Vec::new();
    let mut first = true;

    consume_token!(self, TokenType::LParen)?;
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break },
        _ => {
          if !first { consume_token!(self, TokenType::Comma)?; }
          exprs.push(self.parse_expression()?);
          first = false;
        }
      };
    };
    consume_token!(self, TokenType::RParen)?;
    
    Ok(NewTupleAny(exprs))
  }
  /// Same as `parse_tuple` but returns a vector of nodes rather than a 
  /// single node.
  fn parse_tuple_vec(&mut self) -> Result<Vec<Node>, IvyError> {
    let mut exprs = Vec::new();
    let mut first = true;

    consume_token!(self, TokenType::LParen)?;
    while let Some(tok) = self.peek() {
      match tok.typ {
        TokenType::RParen => { break },
        _ => {
          if !first { consume_token!(self, TokenType::Comma)?; }
          exprs.push(self.parse_expression()?);
          first = false;
        }
      };
    };
    consume_token!(self, TokenType::RParen)?;
    
    Ok(exprs)
  }

  fn parse_symbol(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.next() {
      match tok.typ {
        TokenType::Symbol(_) => Ok(NewAtom(tok)),
        _ => Err(new_parser_expected(tok.row, tok.col, "<symbol>".to_string()))
      }
    } else {
      let (r, c) = self.get_last_token_location();
      return Err(new_parser_expected(r, c, "<symbol>".to_string()))
    }
  }

  fn parse_string(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.next() {
      match tok.typ {
        TokenType::String(_) => Ok(NewAtom(tok)),
        _ => Err(new_parser_expected(tok.row, tok.col, "<string>".to_string()))
      }
    } else {
      let (r, c) = self.get_last_token_location();
      return Err(new_parser_expected(r, c, "<string>".to_string()))
    }
  }

  fn parse_integer(&mut self) -> Result<Node, IvyError> {
    if let Some(tok) = self.next() {
      match tok.typ {
        TokenType::Integer(_) => Ok(NewAtom(tok)),
        _ => Err(new_parser_expected(tok.row, tok.col, "<integer>".to_string()))
      }
    } else {
      let (r, c) = self.get_last_token_location();
      return Err(new_parser_expected(r, c, "<integer>".to_string()))
    }
  }

  /// Next returns an optional token and advances the cursor if there is
  /// Some() next token.
  fn next(&mut self) -> Option<Token> {
    if !self.is_done() {
      self.cursor += 1;
      let tok = &self.tokens[self.cursor - 1];
      Some(copy_token(tok))
    } else { None }
  }

  /// Peek returns an optional token without advancing the cursor.
  pub fn peek(&self) -> Option<Token> {
    if !self.is_done() {
      let tok = &self.tokens[self.cursor];
      Some(copy_token(tok))
    } else { None }
  }

  pub fn peek_twice(&mut self) -> Option<Token> {
    if !(self.cursor + 1 >= self.tokens.len()) {
      Some(copy_token(&self.tokens[self.cursor + 1]))
    } else { None }
  }

  // Wait is thrice a word? lol
  pub fn peek_thrice(&mut self) -> Option<Token> {
    if !(self.cursor + 2 >= self.tokens.len()) {
      Some(copy_token(&self.tokens[self.cursor + 2]))
    } else { None }
  }

  fn get_last_token_location(&self) -> (usize, usize) {
    (self.tokens[self.cursor - 1].row, self.tokens[self.cursor - 1].col)
  }

  //// Returns whether we are at the end of the token stream.
  pub fn is_done(&self) -> bool {
    self.cursor >= self.tokens.len()
  }
}

#[macro_export]
/// expands into code that consumes a TokenType.
macro_rules! consume_token {
  ($self:ident, $token:path) => {
    match $self.next() {
      Some(tok) => {
        match tok.typ {
          $token => {
            $self.last = Some((tok.row, tok.col));
            Ok(tok)
          },
          _ => Err( new_parser_expected(tok.row, tok.col, format!("{}", $token)) )
        }
      }
      _ => {
        let (r, c) = $self.get_last_token_location();
        return Err(new_parser_expected(r, c + 1, format!("{}", $token)))
      }
    }
  };
}


mod tests {
  use crate::parser::parser::*;

  #[test]
  fn test_parse_let_expression() {
    // Given a list of tokens representing `let a = 5;`
    let tokens = vec![
      new_test_token(TokenType::Let),
      new_test_token(TokenType::Symbol("a".to_string())),
      new_test_token(TokenType::Bind),
      new_test_token(TokenType::Integer(5)),
      new_test_token(TokenType::Semicolon),
    ];

    // Construct the expected AST
    let symbol_a = NewAtom(new_test_token(TokenType::Symbol("a".to_string())));
    let value_5 = NewAtom(new_test_token(TokenType::Integer(5)));
    let let_expr = NewLetExpr(
      new_test_token(TokenType::Let),
      vec![symbol_a],
      value_5,
      None
    );

    let expected = NewRootNode(vec![let_expr]);

    // Parse the tokens
    let result = parse(tokens);
    assert!(result.is_ok());

    // Compare the parsed AST to the expected AST
    let parsed = result.unwrap();
    assert_eq!(parsed, expected, "Parsed AST does not match expected.");
  }

  #[test]
  fn test_parse_match_expression() {
    // Given a list of tokens representing `Gender (Male | Female | NonBinary);`
    let tokens = vec![
      new_test_token(TokenType::Data),
      new_test_token(TokenType::Symbol("Gender".to_string())),
      new_test_token(TokenType::LParen),
      new_test_token(TokenType::Symbol("Male".to_string())),
      new_test_token(TokenType::Bar),
      new_test_token(TokenType::Symbol("Female".to_string())),
      new_test_token(TokenType::Bar),
      new_test_token(TokenType::Symbol("NonBinary".to_string())),
      new_test_token(TokenType::RParen),
      new_test_token(TokenType::Semicolon),
    ];

    // Construct the expected AST
    let symbol_gender = NewAtom(new_test_token(TokenType::Symbol("Gender".to_string())));
    let variants = vec![
      NewAtom(new_test_token(TokenType::Symbol("Male".to_string()))),
      NewAtom(new_test_token(TokenType::Symbol("Female".to_string()))),
      NewAtom(new_test_token(TokenType::Symbol("NonBinary".to_string()))),
    ];
    let data_decl = NewDataDelcaration(
      new_test_token(TokenType::Data),
      symbol_gender,
      vec![],
      variants,
    );

    let expected = NewRootNode(vec![data_decl]);

    // Parse the tokens
    let result = parse(tokens);
    assert!(result.is_ok());

    // Compare parsed AST to the expected AST
    let parsed = result.unwrap();
    assert_eq!(parsed, expected, "Parsed AST does not match expected");
  }

  #[test]
  fn test_function_definition() {
    // Given a list of tokens representing `fn square :: Int -> Int; fn square (x) => x * x;`
    let tokens = vec![
      new_test_token(TokenType::Fn),
      new_test_symbol_token("square"),
      new_test_token(TokenType::DoubleColon),
      new_test_symbol_token("Int"),
      new_test_token(TokenType::Arrow),
      new_test_symbol_token("Int"),
      new_test_token(TokenType::Semicolon),
      new_test_token(TokenType::Fn),
      new_test_symbol_token("square"),
      new_test_token(TokenType::LParen),
      new_test_symbol_token("x"),
      new_test_token(TokenType::RParen),
      new_test_token(TokenType::EqArrow),
      new_test_symbol_token("x"),
      new_test_token(TokenType::Star),
      new_test_symbol_token("x"),
      new_test_token(TokenType::Semicolon),
    ];

    // Construct the expected AST
    let fn_signature = NewFnSignature(
      new_test_token(TokenType::Fn),
      new_test_symbol_node("square"),
      NewTypeFn(
        NewTtype(new_test_symbol_node("Int"), false),
        NewTtype(new_test_symbol_node("Int"), false),
        new_test_token(TokenType::Arrow),
      ),
    );

    let fn_definition = NewFnDeclaration(
      new_test_token(TokenType::Fn),
      new_test_symbol_node("square"),
      NewBinaryExpression(
        new_test_token(TokenType::Star),
        new_test_symbol_node("x"),
        new_test_symbol_node("x"),
      ),
      vec![new_test_symbol_node("x")],
      None,
    );

    let expected = NewRootNode(vec![fn_signature, fn_definition]);
    
    // Parse the tokens
    let result = parse(tokens);
    assert!(result.is_ok());

    // Compare parsed AST to the expected AST
    let parsed = result.unwrap();
    assert_eq!(parsed, expected, "Parsed AST does not match expected");
  }

  #[test]
  fn test_struct() {
    let tokens = vec![
      new_test_token(TokenType::Struct),
      new_test_symbol_token("Person"),
      new_test_token(TokenType::LParen),
      new_test_token(TokenType::At),
      new_test_symbol_token("name"),
      new_test_token(TokenType::DoubleColon),
      new_test_symbol_token("String"),
      new_test_token(TokenType::At),
      new_test_symbol_token("age"),
      new_test_token(TokenType::DoubleColon),
      new_test_symbol_token("Int"),
      new_test_token(TokenType::At),
      new_test_symbol_token("gender"),
      new_test_token(TokenType::DoubleColon),
      new_test_symbol_token("Gender"),
      new_test_token(TokenType::RParen),
      new_test_token(TokenType::Semicolon),
    ];

    let struct_declaration = NewStructDeclaration(
      new_test_token(TokenType::Struct),
      new_test_symbol_node("Person"),
      vec![
        NewStructField(
          new_test_symbol_node("name"),
          NewTtype(new_test_symbol_node("String"), false)),
        NewStructField(
          new_test_symbol_node("age"),
          NewTtype(new_test_symbol_node("Int"), false)),
        NewStructField(
          new_test_symbol_node("gender"), 
          NewTtype(new_test_symbol_node("Gender"), false)),
      ],
    );

    let expected = NewRootNode(vec![struct_declaration]);

    // Parse the tokens
    let result = parse(tokens);
    assert!(result.is_ok());

    // Compare parsed AST to the expected AST
    let parsed = result.unwrap();
    assert_eq!(parsed, expected, "Parsed AST does not match expected");
  }

  // fn test 
}
