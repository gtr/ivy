use crate::lexer::tokens::*;
use crate::errors::errors::*;

fn match_single_token(ch: char) -> Option<TokenType> {
  match ch {
    '*' => Some(TokenType::Star),
    '+' => Some(TokenType::Plus),
    '.' => Some(TokenType::Dot),
    '/' => Some(TokenType::Slash),
    ')' => Some(TokenType::RParen),
    '[' => Some(TokenType::LBracket),
    ']' => Some(TokenType::RBracket),
    ',' => Some(TokenType::Comma),
    ';' => Some(TokenType::Semicolon),
    '{' => Some(TokenType::LCurly),
    '}' => Some(TokenType::RCurly),
    '@' => Some(TokenType::At),
    _   => None,
  }
}

fn match_keyword(word: String) -> TokenType {
  match word.as_str() {
    "let"       => TokenType::Let,
    "mut"       => TokenType::Mut,
    "fn"        => TokenType::Fn,
    "if"        => TokenType::If,
    "then"      => TokenType::Then,
    "else"      => TokenType::Else,
    "pub"       => TokenType::Pub,
    "data"      => TokenType::Data,
    "struct"    => TokenType::Struct,
    "package"   => TokenType::Package,
    "import"    => TokenType::Import,
    "match"     => TokenType::Match,
    "with"      => TokenType::With,
    "while"     => TokenType::While,
    "do"        => TokenType::Do,
    "return"    => TokenType::Return,
    "trait"     => TokenType::Trait,
    "impl"      => TokenType::Impl,
    "for"       => TokenType::For,
    _           => TokenType::Symbol(word),
  }
}

/// Lexically analyzes the given `src` and produces a list of tokens.
/// 
/// # Arguments
/// 
/// * `src` - A string slice that holds the source code.
/// * `filename` - The name of the file.
/// 
/// # Returns
/// 
/// An `Option` containing a vector of tokens if the lexing succeeds, or 
/// `None` if an error occurs.

pub fn lex(src: &str, filename: &str) -> Option<Vec<Token>> {
  match lex_(src) {
    Ok(tokens) => Some(tokens),
    Err(err) => {
      err.show_error(src, filename);
      None
    }
  }
}

fn lex_(src: &str) -> Result<Vec<Token>, LexerError> {
  let (mut row, mut col, mut peek_ptr) = (1, 1, 1);
  let mut tokens = Vec::new();
  let mut chars = src.chars().peekable();

  loop {
    peek_ptr += 1;

    let iter = chars.next();
    if iter.is_none() {
      break;
    }


    let curr = iter.unwrap();
    if let Some(token_type) = match_single_token(curr) {
      tokens.push(Token {row, col, typ: token_type});
      col = peek_ptr;
      continue;
    }
    
    match curr {
      '-' => {
        match chars.peek() {
          Some('-') => {
            loop {
              match chars.next() {
                Some('\n') => {
                  peek_ptr = 1;
                  row += 1;
                  col = 1;
                  break;
                },
                None => { return Ok(tokens); },
                _    => peek_ptr += 1,
              }
            }
          },
          Some('>') => {
            chars.next();
            peek_ptr += 1;
            tokens.push(Token {row, col, typ: TokenType::Arrow});
            col = peek_ptr;
          },
          Some(')') => {
            chars.next();
            peek_ptr += 1;
            tokens.push(Token {row, col, typ: TokenType::CommentRight});
            col = peek_ptr;
          }
          _ => {
            tokens.push(Token {row, col, typ: TokenType::Minus});
            col = peek_ptr;
          }
        }
      },

      '+' => tokens.push(match chars.peek() {
          Some('+') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::PlusPlus};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Plus};
              col = peek_ptr;
              tok
          }
      }),

      '=' => tokens.push(match chars.peek() {
          Some('=') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::Eq};
              col = peek_ptr;
              tok
          },
          Some('>') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::EqArrow};
              col = peek_ptr;
              tok
          }
          _ => {
              let tok = Token {row, col, typ: TokenType::Bind};
              col = peek_ptr;
              tok
          },
      }),

      '(' => {
          match chars.peek() {
              Some('-') => {
                  chars.next();
                  loop {
                      match chars.next() {
                          Some('\n') => {
                              peek_ptr = 1;
                              row += 1;
                              col = 1;
                              break;
                          },
                          Some('-') => {
                              peek_ptr += 2;
                              match chars.next() {
                                  Some(')') => break,
                                  _ => {}
                              }
                          }
                          Some(_) =>  peek_ptr += 1,
                          None    => { return Ok(tokens); },
                      }
                  }
              },
              _ => {
                  tokens.push(Token {row, col, typ: TokenType::LParen});
                  col = peek_ptr;
              }
          }
      },

      ':' => tokens.push(match chars.peek() {
          Some(':') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::DoubleColon};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Colon};
              col = peek_ptr;
              tok
          },
      }),

      '!' => tokens.push(match chars.peek() {
          Some('=') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::NotEq};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Not};
              col = peek_ptr;
              tok
          },
      }),

      '>' => tokens.push(match chars.peek() {
          Some('=') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::GreaterEqual};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Greater};
              col = peek_ptr;
              tok
          },
      }),

      '<' => tokens.push(match chars.peek() {
          Some('=') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::LessEqual};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Less};
              col = peek_ptr;
              tok
          },
      }),

      '&' => tokens.push(match chars.peek() {
          Some('&') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::And};
              col = peek_ptr;
              tok
          },
          _ => { return Err(new_invalid_token('&', row, col)) }
      }),

      '|' => tokens.push(match chars.peek() {
          Some('|') => {
              chars.next();
              peek_ptr += 1;
              let tok = Token {row, col, typ: TokenType::Or};
              col = peek_ptr;
              tok
          },
          _ => {
              let tok = Token {row, col, typ: TokenType::Bar};
              col = peek_ptr;
              tok
          }
      }),

      // Pattern matching on a keyword or an identifier.
      s @ 'a' ..= 'z' | s @ 'A'..='Z' => {
          let mut word = String::new();
          word.push(s);
          while let Some('a'..='z') | Some('A'..='Z')
                  | Some('_') | Some('?') | Some('0'..='9') = chars.peek() {
              word.push(chars.next().unwrap());
              peek_ptr += 1;
          }

          let tok = Token {row, col, typ: match_keyword(word)};
          col = peek_ptr;
          tokens.push(tok);
      },

      s @ '_' =>  {
          let mut word = String::new();
          word.push(s);
          while let Some('a'..='z') | Some('A'..='Z') 
              | Some('0'..= '9') | Some('=') | Some('>') 
              | Some('<') | Some('_') | Some('!') = chars.peek() {
              word.push(chars.next().unwrap());
              peek_ptr += 1;
          }

          let tok = Token {row, col, typ: match_keyword(word)};
          col = peek_ptr;
          tokens.push(tok);
      },

      // Pattern matching on an integer.
      n @ '0'..='9' => {
          let mut word = String::new();
          word.push(n);

          while let Some('0'..='9') = chars.peek() {
              word.push(chars.next().unwrap());
              peek_ptr +=  1;
          }

          let num: i32 = word.parse().unwrap();
          
          let tok = Token {row, col, typ: TokenType::Integer(num)};
          col = peek_ptr;
          tokens.push(tok);
      }

      // Lexing a string.
      '"' => {
          let mut word = String::new();
          let mut finished = false;

          loop {
              match chars.peek() {
                  Some('"') => {
                      finished = true;
                      chars.next();
                      peek_ptr += 1;
                      break;
                  }
                  Some(_) => {
                      word.push(chars.next().unwrap());
                      peek_ptr += 1;
                  },
                  None => {break},
              }
          }

          if finished {
              let tok = Token {row, col, typ:TokenType::String(word)};
              col = peek_ptr;
              tokens.push(tok);
          } else {
              return Err(new_unterminated_str(row, col));
          }
      },
      ' '  => {
          col = peek_ptr;
      },
      '\t' => peek_ptr += 4, // i gotta fix this lol
      '\n' => {
          peek_ptr = 1;
          col = 1;
          row += 1;
      },
      u => {
          return Err(new_invalid_token(u, row, col));
      }
    }
  }

  Ok(tokens)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokens::TokenType;

    #[test]
    fn test_basic_tokens() {
        let input = "+-*/";
        let result = lex(input, "");
        assert!(result.is_some());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].typ, TokenType::Plus);
        assert_eq!(tokens[1].typ, TokenType::Minus);
        assert_eq!(tokens[2].typ, TokenType::Star);
        assert_eq!(tokens[3].typ, TokenType::Slash);
    }

    #[test]
    fn test_keywords() {
        let input = "let mut fn if";
        let result = lex(input, "");
        assert!(result.is_some());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].typ, TokenType::Let);
        assert_eq!(tokens[1].typ, TokenType::Mut);
        assert_eq!(tokens[2].typ, TokenType::Fn);
        assert_eq!(tokens[3].typ, TokenType::If);
    }

    #[test]
    fn test_ignoring_whitespace() {
        let input = "   +   -  ==  =>  ;  @";
        let result = lex(input, "");
        assert!(result.is_some());
        let tokens = result.unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].typ, TokenType::Plus);
        assert_eq!(tokens[1].typ, TokenType::Minus);
        assert_eq!(tokens[2].typ, TokenType::Eq);
        assert_eq!(tokens[3].typ, TokenType::EqArrow);
        assert_eq!(tokens[4].typ, TokenType::Semicolon);
        assert_eq!(tokens[5].typ, TokenType::At);
    }

    #[test]
    fn test_comments() {
        let input = "-- this is a comment.";
        let result = lex(input, "");
        assert!(result.is_some());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_multiline_comments() {
      let input = "begin (- this is a comment -) end";
      let result = lex(input, "");
      assert!(result.is_some());
      let tokens = result.unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].typ, TokenType::Symbol("begin".to_string()));
        assert_eq!(tokens[1].typ, TokenType::Symbol("end".to_string()));
    }

    #[test]
    fn test_number() {
      let input = "456";
      let result = lex(input, "");
      assert!(result.is_some());
      let tokens = result.unwrap();
      assert_eq!(tokens.len(), 1);
      assert_eq!(tokens[0].typ, TokenType::Integer(456));
    }

    #[test]
    fn test_string() {
      let input = "\"hello world\"";
      let result = lex(input, "");
      assert!(result.is_some());
      let tokens = result.unwrap();
      assert_eq!(tokens.len(), 1);
      assert_eq!(tokens[0].typ, TokenType::String("hello world".to_string()));
    }

    #[test]
    fn test_unterminated_string() {
      let input = "\"hello world";
      let result = lex(input, "");
      assert!(result.is_none());
    }

    #[test]
    fn test_unexpected_character() {
        let input = "#";
        let result = lex(input, "");
        assert!(result.is_none());
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let result = lex(input, "");
        assert!(result.is_some());
        assert!(result.unwrap().is_empty());
    }
}
