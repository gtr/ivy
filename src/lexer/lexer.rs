use crate::lexer::tokens::*;
use crate::errors::errors;

fn match_single_token(ch: char) -> Option<Token> {
    match ch {
        '*' => Some(Token::Star),
        '(' => Some(Token::LeftParen),
        ')' => Some(Token::RightParen),
        '[' => Some(Token::LeftBracket),
        ']' => Some(Token::RightBracket),
        ',' => Some(Token::Comma),
        ';' => Some(Token::Semicolon),
        '/' => Some(Token::Slash),
        _ => None,
    }
}

fn match_keyword(word: String) -> Token {
    match word.as_str() {
        "let" => Token::Let,
        "fn" => Token::Fn,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "true" => Token::True,
        "false" => Token::False,
        "print" => Token::Print,
        "println" => Token::PrintLn,
        _ => Token::Symbol(word),
    }
}

pub fn print_tokens(token_result: Result<Vec<Token>, String>) {
    match token_result {
        Err(s) => {
            let error = errors::IvyError::LexerError(s);
            println!("{}", error);
            return;
        }
        Ok(_) => {
            let tokens = token_result.unwrap().into_iter().collect::<Vec<_>>();
            for token in tokens {
                println!("{}", token);
            }
        }
    }
}

pub fn lex(src: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = src.chars().peekable();

    loop {
        let iter = chars.next();
        if iter.is_none() {
            break;
        }

        let curr = iter.unwrap();
        if let Some(token) = match_single_token(curr) {
            tokens.push(token);
            continue;
        }

        match curr {
            '+' => tokens.push(match chars.peek() {
                Some('+') => {
                    chars.next();
                    Token::PlusPlus
                },
                _ => Token::Plus,
            }),

            '-' => tokens.push(match chars.peek() {
                Some('-') => {
                    chars.next();
                    Token::MinusMimus
                },
                Some('>') => {
                    chars.next();
                    Token::Arrow
                },
                _ => Token::Minus,
            }),

            '=' => tokens.push(match chars.peek() {
                Some('=') => {
                    chars.next();
                    Token::Eq
                },
                _ => Token::Bind,
            }),

            '!' => tokens.push(match chars.peek() {
                Some('=') => {
                    chars.next();
                    Token::NotEq
                },
                _ => Token::Not,
            }),

            '>' => tokens.push(match chars.peek() {
                Some('=') => {
                    chars.next();
                    Token::GreaterEqual
                },
                _ => Token::Greater,
            }),

            '<' => tokens.push(match chars.peek() {
                Some('=') => {
                    chars.next();
                    Token::LessEqual
                },
                _ => Token::Less,
            }),

            // Pattern matching on a keyword or an identifier.
            s @ 'a' ..= 'z' | s @ 'A'..='Z' => {
                let mut word = String::new();
                word.push(s);
                loop {
                    match chars.peek() {
                        Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                            word.push(chars.next().unwrap());
                        }
                        _ => break,
                    }
                }

                tokens.push(match_keyword(word));
            }

            // Pattern matching on an integer.
            n @ '0'..='9' => {
                let mut word = String::new();
                word.push(n);

                loop {
                    match chars.peek() {
                        Some('0'..='9') => {
                            word.push(chars.next().unwrap());
                        },
                        _ => break,
                    };
                }

                let num: i32 = word.parse().unwrap();
                tokens.push(Token::Integer(num));
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
                            break;
                        }
                        Some(_) => {
                            word.push(chars.next().unwrap());
                        },
                        None => {break},
                    }
                }

                if finished {
                    tokens.push(Token::String(word));
                } else {
                    return Err(format!("Unterminated string: \"{}", word));
                }
            }

            // Ignoring whitespaces; they don't matter in ivy.
            ' ' | '\t' | '\n' => {
                continue;
            }

            u @ _ => {
                return Err(format!("Unexpected token: {}", u));
            }
        }
    }

    Ok(tokens)
}