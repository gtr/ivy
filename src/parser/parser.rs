use crate::lexer::tokens::*;
use crate::parser::ast::*;
use crate::errors::errors;
use crate::token_to_node;
use crate::consume_token;

#[derive(Default)]
pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

pub fn parse(tokens: Vec<Token>) {
    let mut p = Parser::new(tokens);
    match p.parse() {
        Ok(node) => print_tree(&node),
        Err(err) => {
            let error = errors::IvyError::ParserError(err);
            println!("{}", error);
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser{tokens: tokens, cursor: 0}
    }

    pub fn parse(&mut self) -> Result<Node, String> {
        let mut root = Node::new(GrammarItem::Root);

        while !self.is_done() {
            root.children.push(self.parse_statement()?);
        }

        Ok(root)
    }

    /// Parses a statement based on the following rule:
    /// 
    /// <statement> ::= <expression> ';' ;
    /// 
    fn parse_statement(&mut self) -> Result<Node, String> {
        let expr = self.parse_expression()?;
        consume_token!(self, Token::Semicolon)?;

        Ok(expr)
    }

    /// Parses an expression based on the following rule:
    /// 
    /// <expresion> ::= <letExpr> 
    ///               | <funcExpr>
    ///               | <ifExpr>
    ///               | <matchExpr>
    ///               | <tupleExpr>
    ///               | <enumExpr>
    ///               | <or> ;
    /// 
    fn parse_expression(&mut self) -> Result<Node, String> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::Fn) => self.parse_function(),
            Some(Token::If) => self.parse_if(),
            Some(Token::Match) => self.parse_match(),
            Some(Token::Tuple) => self.parse_tuple_expression(),
            Some(Token::Enum) => self.parse_enum(),
            _ => self.parse_or(),
        }
    }


    /// Parses a let expression based on the following rule:
    /// 
    /// <letExpr> ::= 'let' [ <symbol> | <tupleExpr> ] '=' <expression> ;
    /// 
    fn parse_let(&mut self) -> Result<Node, String> {
        let mut let_node = token_to_node!(self, Token::Let)?;

        match self.peek() {
            Some(Token::LParen) => let_node.children.push(self.parse_tuple()?),
            Some(Token::Symbol(_)) => let_node.children.push(self.parse_symbol()?),
            _ => {
                return Err(format!("expected symbol or tuple"))
            },
        }
        
        consume_token!(self, Token::Bind)?;
        let_node.children.push(self.parse_expression()?);

        Ok(let_node)
    }

    /// Parses a function expression based on the following rule:
    /// 
    /// <funcExpr>  ::= 'fn' <tupleExpr> '->' <expression> ;
    /// 
    fn parse_function(&mut self) -> Result<Node, String> {
        let mut func = Node::new(GrammarItem::Fn);

        consume_token!(self, Token::Fn)?;
        func.children.push(self.parse_tuple()?);
        consume_token!(self, Token::Arrow)?;
        func.children.push(self.parse_expression()?);

        Ok(func)
    }

    /// Parses an if expression based on the following rule:
    /// 
    /// <ifExpr> ::= 'if' '(' <or> ')' 
    ///             'then' <expression> 
    ///             [ 'else' <expression> ]? ;
    /// 
    fn parse_if(&mut self) -> Result<Node, String> {
        let mut if_node = token_to_node!(self, Token::If)?;

        consume_token!(self, Token::LParen)?;
        if_node.children.push(token_to_node!(self, Token::If)?);
        consume_token!(self, Token::RParen)?;

        consume_token!(self, Token::Then)?;
        if_node.children.push(self.parse_expression()?);
        
        match self.peek() {
            Some(Token::Else) => {
                consume_token!(self, Token::Else)?;
                if_node.children.push(self.parse_expression()?);
            },
            _ => {return Ok(if_node)}
        }

        Ok(if_node)
    }

    /// Parses a match expression based on the following rule:
    ///   
    /// <matchExpr> ::= 'match' '(' <expression> ')' '(' <matchBranches> ')' ;
    /// 
    fn parse_match(&mut self) -> Result<Node, String> {
        let mut match_node = token_to_node!(self, Token::Match)?;

        consume_token!(self, Token::LParen)?;
        match_node.children.push(self.parse_expression()?);
        consume_token!(self, Token::RParen)?;

        consume_token!(self, Token::LParen)?;
        let branches = self.parse_match_branches()?;
        consume_token!(self, Token::RParen)?;

        for branch in branches {
            match_node.children.push(branch);
        };

        Ok(match_node)
    }

    /// Parses a match branches pattern based on the following rule:
    /// 
    /// <matchBranches> ::= '|' <expresion> '->' <expresion> 
    ///                     [ <matchBranches> ]* ;
    /// 
    fn parse_match_branches(&mut self) -> Result<Vec<Node>, String> {
        let mut branches = Vec::new();

        loop {
            match self.peek() {
                Some(Token::Pipe) => {
                    consume_token!(self, Token::Pipe)?;
                    let mut branch = Node::new(GrammarItem::Branch);
                    
                    branch.children.push(self.parse_expression()?);
                    consume_token!(self, Token::Arrow)?;
                    branch.children.push(self.parse_expression()?);

                    branches.push(branch);
                }
                _ => {break}
            };
        };

        Ok(branches)
    }

    /// Parses a tuple expression based on the following rule:
    /// 
    /// <tupleExpr> ::= 'tuple' <tuple> ;
    /// 
    fn parse_tuple_expression(&mut self) -> Result<Node, String> {
        consume_token!(self, Token::Tuple)?;

        self.parse_tuple()
    }

    /// Parses a tuple pattern based on the following rule:
    /// 
    /// <tuple> ::= '(' [ <commas> ]? ')' ;
    /// 
    fn parse_tuple(&mut self) -> Result<Node, String> {
        let mut tuple = Node::new(GrammarItem::Tuple);
        
        consume_token!(self, Token::LParen)?;

        match self.peek() {
            Some(Token::RParen) => consume_token!(self, Token::RParen)?,
            _ => {
                tuple.children.append(&mut self.parse_commas()?);
                consume_token!(self, Token::RParen)?;
            },
        };

        Ok(tuple)
    }

    /// Parses a comma expression based on the following rule:
    /// 
    /// <commas> ::= <atom> [ ',' <atom> ]* ;
    /// 
    /// Note: this function returns a vector of tokens rather than just a 
    /// single token.
    fn parse_commas(&mut self) -> Result<Vec<Node>, String> {
        let mut atoms = Vec::new();

        loop {
            atoms.push(self.parse_atom()?);
            match self.peek() {
                Some(Token::Comma) => consume_token!(self, Token::Comma)?,
                _ => break,
            }
        }

        Ok(atoms)
    }

    /// Parses an enum expression based on the following rule:
    /// 
    /// <enumExpr> ::= 'enum' '(' <enumPipes> ')' ;
    /// 
    fn parse_enum(&mut self) -> Result<Node, String> {
        let mut enum_node = token_to_node!(self, Token::Enum)?;

        consume_token!(self, Token::LParen)?;
        enum_node.children.append(&mut self.parse_enum_pipes()?);
        consume_token!(self, Token::RParen)?;

        Ok(enum_node)
    }

    /// Parses an enum expression based on the following rule:
    /// 
    /// <enumPipes> ::= [ '|' ]? <symbol> [ '|' <symbol> ]* ;
    /// 
    fn parse_enum_pipes(&mut self) -> Result<Vec<Node>, String> {
        let mut variants = Vec::new();

        match self.peek() {
            Some(Token::Pipe) => consume_token!(self, Token::Pipe)?,
            _ => {},
        }

        loop {
            variants.push(self.parse_symbol()?);
            match self.peek() {
                Some(Token::Pipe) => consume_token!(self, Token::Pipe)?,
                _ => break,
            };
        };

        Ok(variants)
    }

    /// Parses an and expression based on the following rule:
    /// 
    /// <or> ::= <and> [ '||' <and> ]* ;
    /// 
    fn parse_or(&mut self) -> Result<Node, String> {
        let lhs = self.parse_and()?;

        let mut and = match self.peek() {
            Some(Token::Or) => token_to_node!(self, Token::Or)?,
            _ => return Ok(lhs),
        };

        let rhs = self.parse_and()?;

        and.children.push(lhs);
        and.children.push(rhs);

        Ok(and)
    }


    /// Parses an and expression based on the following rule:
    /// 
    /// <and> ::= <equality> [ '&&' <equality> ]* ;
    /// 
    fn parse_and(&mut self) -> Result<Node, String> {
        let lhs = self.parse_equality()?;

        let mut and = match self.peek() {
            Some(Token::And) => token_to_node!(self, Token::And)?,
            _ => return Ok(lhs),
        };

        let rhs = self.parse_equality()?;

        and.children.push(lhs);
        and.children.push(rhs);

        Ok(and)
    }

    /// Parses an equality expression based on the following rule:
    /// 
    /// <equality>      ::= <comparison> [ [ '==' | '!=' ] <comparison> ]* ;
    /// 
    fn parse_equality(&mut self) -> Result<Node, String> {
        let lhs = self.parse_comparison()?;

        let mut equality = match self.peek() {
            Some(Token::Eq) => token_to_node!(self, Token::Eq)?,
            Some(Token::NotEq) => token_to_node!(self, Token::NotEq)?,
            _ => return Ok(lhs),
        };

        let rhs = self.parse_comparison()?;

        equality.children.push(lhs);
        equality.children.push(rhs);

        Ok(equality)
    }

    /// Parses an addition expression based on the following rule:
    /// 
    /// <comparison> ::= <addition> [ [ '>' 
    ///                              | '>=' 
    ///                              | '<' 
    ///                              | '<=' ] <addition> ]* ;
    /// 
    fn parse_comparison(&mut self) -> Result<Node, String> {
        let lhs = self.parse_addition()?;

        let mut comparison = match self.peek() {
            Some(Token::Greater) => token_to_node!(self, Token::Greater)?,
            Some(Token::GreaterEqual) => token_to_node!(self, Token::GreaterEqual)?,
            Some(Token::Less) => token_to_node!(self, Token::Less)?,
            Some(Token::LessEqual) => token_to_node!(self, Token::LessEqual)?,
            _ => return Ok(lhs),
        };

        let rhs = self.parse_addition()?;

        comparison.children.push(lhs);
        comparison.children.push(rhs);

        Ok(comparison)
    }

    /// Parses an addition expression based on the following rule:
    /// 
    /// <addition> ::= <mult> [ ( '+' | '-' ) <mult> ]* ;
    /// 
    fn parse_addition(&mut self) -> Result<Node, String> {
        let lhs = self.parse_mult()?;

        let mut addition = match self.peek() {
            Some(Token::Plus) => token_to_node!(self, Token::Plus)?,
            Some(Token::Minus) => token_to_node!(self, Token::Minus)?,
            _ => return Ok(lhs),
        };

        let rhs = self.parse_mult()?;

        addition.children.push(lhs);
        addition.children.push(rhs);

        Ok(addition)
    }

    /// Parses a multiplication expression based on the following rule:
    /// 
    /// <mult> ::= <unary> [ ( '*' | '/' ) <unary> ]* ;
    /// 
    fn parse_mult(&mut self) -> Result<Node, String> {
        let lhs = self.parse_unary()?;

        let mut addition = match self.peek() {
            Some(Token::Star) => token_to_node!(self, Token::Star)?,
            Some(Token::Slash) => token_to_node!(self, Token::Slash)?,
            _ => return Ok(lhs),
        };

        addition.children.push(lhs);
        addition.children.push(self.parse_unary()?);

        Ok(addition)
    }

    /// Parses a unary expression based on the following rule:
    /// 
    /// <unary> ::= [ '!' | '-' | '++' | '--' ] <call> | <call> ;
    /// 
    fn parse_unary(&mut self) -> Result<Node, String> {
        match self.peek() {
            Some(Token::Not) | Some(Token::Minus) 
            | Some(Token::PlusPlus) | Some(Token::MinusMimus) => {
                let mut op = self.expect_unary()?;
                op.children.push(self.parse_call()?);
                Ok(op)
            }
            _ => self.parse_call(),
        }
    }

    /// Parses a call expression based on the following rule:
    /// 
    /// <call> ::= <factor> '(' [ <commas> ]? ')' | <factor> ;
    /// 
    fn parse_call(&mut self) -> Result<Node, String> {
        let mut lhs = self.parse_factor()?;

        match self.peek() {
            Some(Token::LParen) => {
                lhs.children.push(self.parse_tuple()?);
                Ok(lhs)
            }
            _ => Ok(lhs),
        }
    }

    /// Parses a factor expression based on the following rule:
    /// 
    /// <factor> ::= '(' <or> ')' | <atom> ;
    /// 
    fn parse_factor(&mut self) -> Result<Node, String> {
        match self.peek() {
            Some(Token::LParen) => {
                consume_token!(self, Token::LParen)?;
                let expr = self.parse_or();
                consume_token!(self, Token::RParen)?;
                expr
            },
            Some(Token::Integer(_)) | Some(Token::Symbol(_))  | Some(Token::String(_)) 
            | Some(Token::True) | Some(Token::False) => self.parse_atom(),
            _ => Err(format!("error parsing factor")),
        }
    }

    /// Parses an atom expression based on the following rule:
    /// 
    /// <atom> ::= <integer>
    ///          | <symbol> 
    ///          | <string>
    ///          | <boolean> ;
    /// 
    fn parse_atom(&mut self) -> Result<Node, String> {
        match self.peek() {
            Some(Token::Integer(_)) => self.parse_integer(),
            Some(Token::Symbol(_)) => self.parse_symbol(),
            Some(Token::String(_)) => self.parse_string(),
            Some(Token::True) | Some(Token::False) => self.parse_boolean(),
            _ => Err(format!("expected an atom")),
        }
    }
    
    fn parse_integer(&mut self) -> Result<Node, String> {
        match self.next() {
            Some(Token::Integer(i)) => Ok(Node::new(GrammarItem::Integer(*i))),
            _ => Err(format!("expected integer")),
        }
    }

    fn parse_symbol(&mut self) -> Result<Node, String> {
        match self.next() {
            Some(Token::Symbol(s)) => Ok(Node::new(GrammarItem::Symbol(s.to_string()))),
            _ => Err(format!("expected symbol")),
        }
    }

    fn parse_string(&mut self) -> Result<Node, String> {
        match self.next() {
            Some(Token::String(s)) => Ok(Node::new(GrammarItem::String(s.to_string()))),
            _ => Err(format!("expected symbol")),
        }
    }

    fn parse_boolean(&mut self) -> Result<Node, String> {
        match self.next() {
            Some(Token::True) => Ok(Node::new(GrammarItem::True)),
            Some(Token::False) => Ok(Node::new(GrammarItem::False)),
            _ => Err(format!("expected boolean")),
        }
    }

    fn expect_unary(&mut self) -> Result<Node, String> {
        match self.next() {
            Some(Token::Not) => Ok(Node::new(GrammarItem::Not)),
            Some(Token::Minus) => Ok(Node::new(GrammarItem::Minus)),
            Some(Token::MinusMimus) => Ok(Node::new(GrammarItem::MinusMimus)),
            Some(Token::PlusPlus) => Ok(Node::new(GrammarItem::PlusPlus)),
            _ => Err(format!("expected unary")),
        }
    }

    /// Next returns an optional token and advances the cursor if there is
    /// Some() next token.
    fn next(&mut self) -> Option<&Token> {
        if !self.is_done() {
            self.cursor += 1;
            return Some(&self.tokens[self.cursor - 1]);
        }
        None
    }

    /// Peek returns an optional token without advancing the cursor.
    pub fn peek(&self) -> Option<&Token> {
        if !self.is_done() {
            return Some(&self.tokens[self.cursor]);
        }
        None
    }

    //// Returns whether we are at the end of the token stream.
    pub fn is_done(&self) -> bool {
        self.cursor >= self.tokens.len()
    }
}

#[macro_export]
/// expands into code that consumes a token.
macro_rules! consume_token {
    ($self:ident, $token:path) => {
        match $self.next() {
            Some($token) => Ok(()),
            _ => Err(format!("expected {}", $token))
        }
    };
}

#[macro_export]
/// expands into code that matches a Token to a GrammarItem.
macro_rules! token_to_node {
    ($self:ident, $token:path) => {
        match $self.next() {
            Some($token) => Ok(Node::new(token_to_grammar_item($token))),
            _ => Err(format!("expected {}", token_to_grammar_item($token)))
        }
    };
}
