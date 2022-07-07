use std::fmt;

/// Token represents a token in the ivy programming language.
#[allow(dead_code)]
#[derive(Debug)]
pub enum Token {

    // Atoms
    Integer(i32),   // 32
    String(String), // "hello"
    Symbol(String), // user-defined
    True,           // true
    False,          // false
    None,           // None

    // Operators
    Plus,           // +            
    PlusPlus,       // ++
    Minus,          // -
    MinusMimus,     // --
    Star,           // *
    Slash,          // /
    Bind,           // =
    Eq,             // ==
    Not,            // !
    NotEq,          // !=
    Greater,        // >
    GreaterEqual,   // >=
    Less,           // <
    LessEqual,      // <=
    And,            // &&
    Or,             // ||

    // Delimeters
    Pipe,           // |
    Arrow,          // ->
    Comma,          // ,
    Semicolon,      // ;
    LParen,         // (
    RParen,         // )
    LBracket,       // [
    RBracket,       // ]
    LCurly,         // {
    RCurly,         // }

    // Keywords
    Let,            // let
    Fn,             // fn
    If,             // if
    Data,           // data
    Enum,           // enum
    Then,           // then
    Else,           // else
    Tuple,          // tuple 
    Match,          // match
    Print,          // print
    PrintLn,        // println
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Integer(i) => write!(f, "Integer: {}", i),
            Token::String(s) => write!(f, "String: {}", s),
            Token::Symbol(s) => write!(f, "Symbol: {}", s),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
            Token::None => write!(f, "None"),
            
            Token::Plus => write!(f, "Plus"),
            Token::PlusPlus => write!(f, "PlusPlus"),
            Token::Minus => write!(f, "Minus"),
            Token::MinusMimus => write!(f, "MinusMinus"),
            Token::Star => write!(f, "Star"),
            Token::Slash => write!(f, "Slash"),
            Token::Bind => write!(f, "Bind"),
            Token::Eq => write!(f, "Eq"),
            Token::Not => write!(f, "Not"),
            Token::NotEq => write!(f, "NotEq"),
            Token::Greater => write!(f, "Greater"),
            Token::GreaterEqual => write!(f, "GreaterEqual"),
            Token::Less => write!(f, "Less"),
            Token::LessEqual => write!(f, "LessEqual"),
            Token::And => write!(f, "And"),
            Token::Or => write!(f, "Or"),
            
            Token::Pipe => write!(f, "Pipe"),
            Token::Arrow => write!(f, "Arrow"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::LParen => write!(f, "LParen"),
            Token::RParen => write!(f, "RParen"),
            Token::LBracket => write!(f, "LBracket"),
            Token::RBracket => write!(f, "RBracket"),
            Token::LCurly => write!(f, "LCurly"),
            Token::RCurly => write!(f, "RCurly"),
            
            Token::Let => write!(f, "Let"),
            Token::Fn => write!(f, "Fn"),
            Token::If => write!(f, "If"),
            Token::Then => write!(f, "Then"),
            Token::Else => write!(f, "Else"),
            Token::Tuple => write!(f, "Tuple"),
            Token::Data => write!(f, "Data"),
            Token::Enum => write!(f, "Enum"),
            Token::Match => write!(f, "Match"),
            Token::Print => write!(f, "Print"),
            Token::PrintLn => write!(f, "Println"),
        }
    }
}