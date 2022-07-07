use std::fmt;
use crate::lexer::tokens::*;

#[derive(Debug, Clone)]
pub struct Node {
    pub children: Vec<Node>,
    pub entry: GrammarItem,
}

impl Node {
    pub fn new(entry: GrammarItem) -> Node {
        Node {
            children: Vec::new(),
            entry: entry,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum GrammarItem {
    Integer(i32),
    Symbol(String),
    String(String),
    True, False,
    Root,
    
    
    Plus, PlusPlus,
    Minus, MinusMimus,
    Multiply, Divide,
    Eq, Not, NotEq,
    Greater, GreaterEqual,
    Less, LessEqual,
    And, Or,

    Match, Branch, Enum,
    
    
    Let,
    Fn,
    If,
    Tuple,
}

impl fmt::Display for GrammarItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            GrammarItem::Integer(i) => write!(f, "Integer: {}", i),
            GrammarItem::Symbol(s) => write!(f, "Symbol: {}", s),
            GrammarItem::String(s) => write!(f, "String: {}", s),
            GrammarItem::True => write!(f, "true"),
            GrammarItem::False => write!(f, "false"),
            GrammarItem::Match => write!(f, "match"),
            GrammarItem::Branch => write!(f, "branch"),
            GrammarItem::Enum => write!(f, "enum"),

            GrammarItem::Plus => write!(f, "+"),
            GrammarItem::PlusPlus => write!(f, "++"),
            GrammarItem::Minus => write!(f, "-"),
            GrammarItem::MinusMimus => write!(f, "--"),
            GrammarItem::Multiply => write!(f, "*"),
            GrammarItem::Divide => write!(f, "/"),
            GrammarItem::Greater => write!(f, ">"),
            GrammarItem::GreaterEqual => write!(f, ">="),
            GrammarItem::Less => write!(f, "<"),
            GrammarItem::LessEqual => write!(f, "<="),
            GrammarItem::Eq => write!(f, "=="),
            GrammarItem::NotEq => write!(f, "!="),
            GrammarItem::Not => write!(f, "!"),
            GrammarItem::Root => write!(f, "."),
            GrammarItem::Tuple => write!(f, "tuple"),
            
            GrammarItem::And => write!(f, "and"),
            GrammarItem::Or => write!(f, "or"),
            
            GrammarItem::Let => write!(f, "let"),
            GrammarItem::Fn => write!(f, "fn"),
            GrammarItem::If => write!(f, "if"),
        }
    }
}

pub fn token_to_grammar_item(token: Token) -> GrammarItem {
    match token {
        Token::Let => GrammarItem::Let,
        Token::Match => GrammarItem::Match,
        Token::Enum => GrammarItem::Enum,
        
        Token::Integer(i) => GrammarItem::Integer(i),
        Token::Symbol(s) => GrammarItem::Symbol(s),
        Token::String(s) => GrammarItem::String(s),
        
        Token::True => GrammarItem::True,
        Token::False => GrammarItem::False,

        Token::Greater => GrammarItem::Greater,
        Token::GreaterEqual => GrammarItem::GreaterEqual,
        Token::Less => GrammarItem::Less,
        Token::LessEqual => GrammarItem::LessEqual,
        Token::Eq => GrammarItem::Eq,
        Token::NotEq => GrammarItem::NotEq,
        
        Token::Not => GrammarItem::Not,
        Token::Minus => GrammarItem::Minus,
        Token::MinusMimus => GrammarItem::MinusMimus,
        Token::PlusPlus => GrammarItem::PlusPlus,
        Token::Plus => GrammarItem::Plus,
        Token::Star => GrammarItem::Multiply,
        Token::Slash => GrammarItem::Divide,
        
        Token::And => GrammarItem::And,
        Token::Or => GrammarItem::Or,
        Token::If => GrammarItem::If,    

        _ => GrammarItem::Root,
    }
}

pub fn print_tree(root: &Node) {
    print_tree_helper(root, 0);
}

pub fn print_tree_helper(root: &Node, level: usize) {
    match root.entry {
        GrammarItem::Root => {},
        _ => {
            let indent = "  ".repeat(level - 1);
            println!("{}: {}{}", level, indent, root.entry);
        }
    }

    for ch in &root.children {
        print_tree_helper(&ch, level + 1);
    }
}
