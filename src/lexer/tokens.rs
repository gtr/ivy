use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Number,
    Symbol,
    String,
    Operator,
    Keyword,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LCurly,
    RCurly,
    Arrow,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenType::*;
        f.write_str(
            (match self {
                Number => format!("NUMBER"),
                Symbol => format!("SYMBOL"),
                String => format!("STRING"),
                Operator => format!("OPERATOR"),
                Keyword => format!("KEYWORD"),
                LParen => format!("LPAREN"),
                RParen => format!("RPAREN"),
                LBrack => format!("LBRACK"),
                RBrack => format!("RBRACK"),
                LCurly => format!("LCURLY"),
                RCurly => format!("RCURLY"),
                Arrow => format!("ARROW"),
            })
            .as_str(),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub value: String,
}