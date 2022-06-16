use std::error::Error;
use std::fmt;
use crate::lexer::tokens::*;

fn new_token(kind: TokenType, value: String) -> Token{
    Token{kind: kind, value: value}
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(
            (match &self.kind {
                _ => format!("({}, {})", self.kind, self.value),
            })
            .as_str(),
        )
    }
}

// TokenError represents a token error, used for error handling.
#[derive(Debug)]
pub struct TokenError {
    err: String,
}

impl Error for TokenError {}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tokenization error: {}", self.err)
    }
}

pub fn lex(program: &str) -> Result<Vec<Token>, TokenError> {
    let mut tokens = Vec::new();
    let mut chars = program.chars().collect::<Vec<char>>();
    
    if chars.is_empty() { return Ok(tokens) };

    use TokenType::*;
    while chars.len() > 0 {
        let mut curr = chars.remove(0);
        match curr {
            '+' | '*' | '/' => {
                tokens.push(new_token(Operator, curr.to_string()))
            }
            '-' => {
                let peek = chars[0];
                if peek == '>' {
                    tokens.push(new_token(Arrow, "->".to_string())) 
                } else {
                    tokens.push(new_token(Operator, curr.to_string())) 
                }
            }
            '!' => {
                let peek = chars[0];
                if peek == '=' {
                    tokens.push(new_token(Operator, "!=".to_string())) 
                } else {
                    tokens.push(new_token(Operator, curr.to_string())) 
                }
            }
            '<' => {
                let peek = chars[0];
                if peek == '=' {
                    tokens.push(new_token(Operator, "<=".to_string())) 
                } else {
                    tokens.push(new_token(Operator, curr.to_string())) 
                }
            }
            '>' => {
                let peek = chars[0];
                if peek == '=' {
                    tokens.push(new_token(Operator, ">=".to_string())) 
                } else {
                    tokens.push(new_token(Operator, curr.to_string())) 
                }
            }
            '(' => tokens.push(new_token(LParen, curr.to_string())),
            ')' => tokens.push(new_token(RParen, curr.to_string())),
            '[' => tokens.push(new_token(LBrack, curr.to_string())),
            ']' => tokens.push(new_token(RBrack, curr.to_string())),
            '{' => tokens.push(new_token(LCurly, curr.to_string())),
            '}' => tokens.push(new_token(RCurly, curr.to_string())),
            '"' => {
                let mut word = std::string::String::new();
                // While we don't encounter a closing quote character, we 
                // should continue pushing the chars into the word string.
                while chars.len() > 0 && chars[0] != '"' {
                    word.push(chars.remove(0));
                }

                // We should expect there to be an existing closing quote
                // character. if there is, we just pop it from the vector and
                // if there isn't, we return an error.
                if chars.len() > 0 && chars[0] == '"' {
                    chars.remove(0);
                } else {
                    return Err(TokenError {
                        err: format!("Unterminated string: {}", word),
                    });
                }

                // At this point, we can now push the word into the current 
                // tokens vector.
                tokens.push(new_token(String, word));
            }
            _ => {
                let mut word = std::string::String::new();

                if curr.is_digit(10) {
                    while chars.len() > 0 && (curr.is_digit(10) || curr == '.') {
                        word.push(curr);
                        curr = chars.remove(0);
                    }
                    tokens.push(new_token(Number, word));
                } else {
                    // Collect a word until we get to a whitespace or parentheses.
                    while chars.len() > 0 && !curr.is_whitespace() && curr != '(' && curr != ')' {
                        word.push(curr);
                        
                        let peek = chars[0];
                        if peek == '(' || peek == ')' {
                            break;
                        }

                        curr = chars.remove(0);
                    }
                
                
                    if !word.is_empty() {
                        match word.as_str() {
                            "program" | "println" | "print" | "defn" | "def" | "lambda"
                            | "match" | "list" | "map" | "if" | "true" | "false" | "and"
                            | "or" => {
                                tokens.push(new_token(Keyword, word))
                            }
                            _ => {
                                tokens.push(new_token(Symbol, word))
                            }
                        }   
                    }
                }
            }
        }
    }

    Ok(tokens)
}

pub fn print_tokens(token_result: Result<Vec<Token>, TokenError>) {
    if token_result.is_err() {
        println!("token error!");
    }

    let tokens = token_result.unwrap().into_iter().collect::<Vec<_>>();
    for token in tokens {
        println!("{}", token);
    }
}