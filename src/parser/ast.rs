#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(non_snake_case)]

use crate::lexer::tokens::*;

// ====================================
// Let Node
// ====================================

// <letExpr>  ::= 'let' [ 'mut' ]? [ <symbol> | <tupleSymbols> ] [ '::' <typeFn> ]? '=' <expression> ;
#[derive(Debug, PartialEq)]
pub struct LetExpr {
  pub symbols: Vec<Node>,
  pub rhs: Box<Node>,
  pub token: Box<Token>,
  pub is_mut: bool,
  pub ttype: Box<Option<Node>>
}

pub fn NewLetExpr(tok: Token, symbols: Vec<Node>, rhs: Node, ttype: Option<Node>) -> Node {
  Node::LetExpr(LetExpr { 
    symbols: symbols, 
    rhs: Box::new(rhs), 
    token: Box::new(tok), 
    is_mut: false, 
    ttype: Box::new(ttype)
  })
}

pub fn NewLetMutExpr(tok: Token, symbols: Vec<Node>, rhs: Node, ttype: Option<Node>) -> Node {
  Node::LetExpr(LetExpr { 
    symbols: symbols, 
    rhs: Box::new(rhs), 
    token: Box::new(tok),
    is_mut: true, 
    ttype: Box::new(ttype)
  })
}

// ====================================
// Mut Node
// ====================================

// <mutExpr> ::= 'mut' [ <symbol> | <access> ] '=' <expression> ;
#[derive(Debug, PartialEq)]
pub struct MutExpr {
  pub lhs: Box<Node>,
  pub rhs: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewMutExpr(tok: Token, lhs: Node, rhs: Node) -> Node {
  Node::MutExpr(MutExpr { 
      lhs: Box::new(lhs), rhs: Box::new(rhs), token: Box::new(tok) 
  })
}

// ====================================
// Function Expressions
// ====================================

// <fnAnon>  ::= 'fn' <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;
#[derive(Debug, PartialEq)]
pub struct FnAnon {
  pub arguments: Vec<Node>,
  pub type_out: Option<Box<Node>>,
  pub rhs: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewFnAnon(tok: Token, arguments: Vec<Node>, type_out: Option<Node>, rhs: Node) -> Node {
  Node::FnAnon(FnAnon { 
    arguments: arguments, 
    type_out: type_out.map(Box::new),
    rhs: Box::new(rhs), 
    token: Box::new(tok), 
  })
}

// <fnSignature> ::= 'fn' <symbol> '::' <typeFn> ;
#[derive(Debug, PartialEq)]
pub struct FnSignature {
  pub symbol: Box<Node>,
  pub ttype: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewFnSignature(tok: Token, symbol: Node, ttype: Node) -> Node {
  Node::FnSignature(FnSignature { 
      token: Box::new(tok), symbol: Box::new(symbol), ttype: Box::new(ttype),
  })
}

// <fnDeclaration> ::= 'fn' <symbol> <fnArgs> [ ':' <typeFn> ]? '=>' <expression> ;
#[derive(Debug, PartialEq)]
pub struct FnDeclaration {
  pub symbol: Box<Node>,
  pub arguments: Vec<Node>,
  pub type_out:Option<Box<Node>>,
  pub rhs: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewFnDeclaration(
  tok: Token, symbol: Node, rhs: Node,
  arguments: Vec<Node>, type_out: Option<Node>, 
) -> Node {
  Node::FnDeclaration(FnDeclaration { 
    symbol: Box::new(symbol), 
    arguments: arguments, 
    type_out: type_out.map(Box::new),
    rhs: Box::new(rhs), 
    token: Box::new(tok),
  })
}

// ====================================
// Function Arguments
// ====================================


// <fnArgsTyped> ::= <symbol> [ ':' <typeFn> ] ? ;
#[derive(Debug, PartialEq)]
pub struct FnArgTyped {
  pub symbol: Box<Node>,
  pub ttype: Option<Box<Node>>,
}

pub fn NewFnArgTyped(symbol: Node, ttype: Option<Node>) -> Node {
  Node::FnArgTyped ( FnArgTyped{
    symbol: Box::new(symbol), ttype: ttype.map(Box::new),
  })
}

// ====================================
// If Node
// ====================================

// <ifExpr> ::= 'if' <or> 'then' <expression> [ 'else' <expression> ]? ;
#[derive(Debug, PartialEq)]
pub struct IfExpr {
  pub cond: Box<Node>,
  pub true_branch: Box<Node>,
  pub false_branch: Option<Box<Node>>,
  pub token: Box<Token>,
}

pub fn NewIfExpr(
  tok: Token, cond: Node,  true_branch: Node, false_branch: Option<Node>
) -> Node {
  Node::IfExpr(IfExpr {
    cond: Box::new(cond),
    true_branch: Box::new(true_branch),
    false_branch: false_branch.map(Box::new),
    token: Box::new(tok),
  })
}

// ====================================
// Pub Expression
// ===================================

// <pubExpr> ::= 'pub' [ <fnSignature> | <fnDeclaration> | <typeExpr> | <structStmt> ] ;
#[derive(Debug, PartialEq)]
pub struct PubExpr {
  pub rhs: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewPubExpr (tok: Token, rhs: Node) -> Node {
  Node::PubExpr( PubExpr{ token: Box::new(tok), rhs: Box::new(rhs) })
}

// ====================================
// Data Types 
// ====================================

// <dataDeclaration> ::= 'data' <symbol> '(' <typeVariants> ')' ;
#[derive(Debug, PartialEq)]
pub struct DataDeclaration {
  pub symbol: Box<Node>,
  pub generics: Vec<Node>,
  pub variants: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewDataDelcaration(tok: Token, symbol: Node,  generics: Vec<Node>, variants: Vec<Node>) -> Node {
  Node::DataDeclaration(DataDeclaration {
    token: Box::new(tok), symbol: Box::new(symbol), generics, variants
  })
}

// <dataVariants>  ::= [ '|' ]? <dataItem> [ '|' <dataItem> ]* ;
// <dataItem>      ::= <symbol> [ '::' <typeFn> ]? ;
#[derive(Debug, PartialEq)]
pub struct DataItem {
  pub symbol: Box<Node>,
  pub ttype: Box<Node>,
}

pub fn NewDataItem(symbol: Node, ttype: Node) -> Node {
  Node::DataItem(DataItem { 
    symbol: Box::new(symbol), ttype: Box::new(ttype)
  })
}


// ====================================
// Structs
// ====================================

// <structAnon> ::= 'struct' '(' <stuctFields> ')' ;
#[derive(Debug, PartialEq)]
pub struct StructAnon {
  pub fields: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewStructAnon(tok: Token, fields: Vec<Node>) -> Node {
  Node::StructAnon(StructAnon { token: Box::new(tok), fields })
}

// <structDeclaration> ::= 'struct' <symbol> '(' <stuctFields> ')' ;
#[derive(Debug, PartialEq)]
pub struct StructDeclaration {
  pub symbol: Box<Node>,
  pub fields: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewStructDeclaration(tok: Token, symbol: Node, fields: Vec<Node>) -> Node {
  Node::StructDeclaration(StructDeclaration { 
    symbol: Box::new(symbol), token: Box::new(tok), fields 
  })
}

// <structFields>      ::= <strictField> [ ',' <strictField> ]* [ ',' ]? ;
// <structField>       ::= <symbol> '::' <typeFn> ;
#[derive(Debug, PartialEq)]
pub struct StructField {
  pub symbol: Box<Node>,
  pub ttype: Box<Node>,
}

pub fn NewStructField(symbol: Node, ttype: Node) -> Node {
  Node::StructField(
    StructField{ symbol: Box::new(symbol), ttype: Box::new(ttype)} 
  )
}

// ====================================
// Package Statement
// ====================================

// <packageStmt>   ::= 'package' <symbol> ;
#[derive(Debug, PartialEq)]
pub struct Package {
  pub token: Box<Token>,
  pub rhs: Box<Node>,
}

pub fn NewPackage (rhs: Node, token: Token) -> Node {
  Node::Package(Package { token: Box::new(token), rhs: Box::new(rhs) })
}

// ====================================
// Import Statement
// ====================================

// <importStmt>    ::= 'import' [ <string> | <tupleStrings> ] ;
#[derive(Debug, PartialEq)]
pub struct Import {
  pub token: Box<Token>,
  pub rhs: Vec<Node>,
}

pub fn NewImport (rhs: Vec<Node>, token: Token) -> Node {
  Node::Import(Import { token: Box::new(token), rhs })
}

// ====================================
// Match Expressions
// ====================================

// <matchExpr>     ::= 'match' <or> '(' [ <matchBranch> ]* ')' ;
#[derive(Debug, PartialEq)]
pub struct MatchExpression {
  pub lhs: Box<Node>,
  pub branches: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewMatchExpression(tok: Token, lhs: Node, branches: Vec<Node>) -> Node {
  Node::MatchExpression(
    MatchExpression { token: Box::new(tok), lhs: Box::new(lhs), branches 
  })
}

// <matchBranch>   ::= '|' <expression> '->' <expression>
#[derive(Debug, PartialEq)]
pub struct MatchBranch {
  pub lhs: Box<Node>,
  pub rhs: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewMatchBranch(tok: Token, lhs: Node, rhs: Node) -> Node {
  Node::MatchBranch(MatchBranch { 
    token: Box::new(tok), lhs: Box::new(lhs), rhs: Box::new(rhs) 
  })
}

// ====================================
// List Expressions
// ====================================

// <listLiteral>   ::= '[' [ <listItems> ]? ']' ;
// <listItems>     ::= <expression> [ ',' <expression> ]* ;
#[derive(Debug, PartialEq)]
pub struct ListLiteral {
  pub items: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewListExpression(tok: Token, items: Vec<Node>) -> Node {
  Node::ListLiteral(ListLiteral { token: Box::new(tok), items })
}

// <listSplit>     ::= '[ <symbol> '|' <symbol> ']' ;
#[derive(Debug, PartialEq)]
pub struct ListSplit {
  pub head: Box<Node>,
  pub tail: Box<Node>,
}

pub fn NewListSplit(head: Node, tail: Node) -> Node {
  Node::ListSplit(ListSplit {head: Box::new(head), tail: Box::new(tail)})
}

// ====================================
// Type Literals
// ====================================

// <typeFn>    ::= <typeLst>   | <typeLst> '->' <typeLst> ;
#[derive(Debug, PartialEq)]
pub struct TypeFn {
  pub lhs: Box<Node>,
  pub rhs: Box<Node>,
  pub token: Box<Token>
}

pub fn NewTypeFn(lhs: Node, rhs: Node, token: Token) -> Node {
  Node::TypeFn(TypeFn{ 
    lhs: Box::new(lhs), 
    rhs: Box::new(rhs), 
    token: Box::new(token)
  })
}

// <typeLst>   ::= <typeTuple> | '[' <typeFn> ']' ;
#[derive(Debug, PartialEq)]
pub struct TypeLst {
  pub ttype: Box<Node>,
  pub token: Box<Token>
}

pub fn NewTypeLst(ttype: Node, token: Token) -> Node { 
  Node::TypeLst(TypeLst { ttype: Box::new(ttype), token: Box::new(token) }) 
}

// <typeTuple> ::= <typeCmpst> | '(' <typeFn> [ ',' <typeFn> ]* ')' ;
#[derive(Debug, PartialEq)]
pub struct TypeTuple {
  pub ttypes: Vec<Node>,
}

pub fn NewTypeTuple(ttypes: Vec<Node>) -> Node { Node::TypeTuple(TypeTuple { ttypes }) }

// <typeCmpst> ::= <type>      | <symbol> '<' [ <typeFn> [ ',' <typeFn> ]* ] '>' ;
#[derive(Debug, PartialEq)]
pub struct TypeCmpst {
  pub ttype: Box<Node>,
  pub items: Vec<Node>,
}

pub fn NewTypeCmpst(ttype: Node, items: Vec<Node>) -> Node {
  Node::TypeCmpst(TypeCmpst{ ttype: Box::new(ttype), items })
}

// <type>      ::= [ 'mut' ]? <symbol> | <typeFn> ;
#[derive(Debug, PartialEq)]
pub struct Ttype {
  pub symbol: Box<Node>,
  pub is_mut: bool,
}

pub fn NewTtype(symbol: Node, is_mut: bool) -> Node {
  Node::Ttype(Ttype { symbol: Box::new(symbol), is_mut })
}


// ====================================
// While Expressions
// ====================================

// <whileExpr>     ::= 'while' <or> '{' [ <statement> ]* '}' ;
#[derive(Debug, PartialEq)]
pub struct WhileExpression {
  pub cond: Box<Node>,
  pub statements: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewWhileExpression(tok: Token, cond: Node, statements: Vec<Node>) -> Node {
  Node::WhileExpression(WhileExpression { 
    token: Box::new(tok), cond: Box::new(cond), statements 
  })
}

// ====================================
// Do Expressions
// ====================================

// <doExpr>        ::= 'do' '{' [ <statement> ]* '}' ;
#[derive(Debug, PartialEq)]
pub struct DoExpression {
  pub token: Box<Token>,
  pub statements: Vec<Node>,
}

pub fn NewDoExpression(tok: Token, statements: Vec<Node> ) -> Node {
  Node::DoExpression(DoExpression { token: Box::new(tok), statements })
}

// ====================================
// Return Expressions
// ====================================

// <returnExpr>    ::= 'return' <expression> ;
#[derive(Debug, PartialEq)]
pub struct ReturnExpression {
  pub value: Box<Node>,
  pub token: Box<Token>,
}

pub fn NewReturnExpression(tok: Token, value: Node) -> Node {
  Node::ReturnExpression(ReturnExpression {
    token: Box::new(tok), value: Box::new(value)
  })
}

// ====================================
// Trait Statements
// ====================================

#[derive(Debug, PartialEq)]
pub struct TraitStmt {
  pub symbol: Box<Node>,
  pub funcs: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewTraitStmt(tok: Token, symbol: Node, funcs: Vec<Node>) -> Node {
  Node::TraitStmt(TraitStmt {
    token: Box::new(tok), symbol: Box::new(symbol), funcs
  })
}

// ====================================
// Implementation Statements
// ====================================

#[derive(Debug, PartialEq)]
pub struct ImplStmt {
  pub method: Box<Node>,
  pub symbol: Box<Node>,
  pub funcs: Vec<Node>,
  pub token: Box<Token>,
}

pub fn NewImplStmt(tok: Token, method: Node, symbol: Node, funcs: Vec<Node>) -> Node {
  Node::ImplStmt(ImplStmt {
    method: Box::new(method), 
    token: Box::new(tok), 
    symbol: Box::new(symbol), 
    funcs
  })
}

// ====================================
// Binary & Unary Expressions
// ====================================

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
  pub lhs: Box<Node>,
  pub rhs: Box<Node>,
  pub token: Token,
}

pub fn NewBinaryExpression(tok: Token, lhs: Node, rhs: Node) -> Node {
  Node::BinaryExpression(BinaryExpression {
    token: tok, lhs: Box::new(lhs), rhs: Box::new(rhs)
  })
}

pub fn copy_token(tok: &Token) -> Token {
  Token {
    col: tok.col, row: tok.row, typ: tok.typ.clone()
  }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
  pub rhs: Box<Node>,
  pub token: Token,
}

pub fn NewUnaryExpression(tok: Token, rhs: Node) -> Node {
  Node::UnaryExpression(UnaryExpression { token: tok, rhs: Box::new(rhs) })
}

// ====================================
// Calls
// ====================================

// <call> ::= <access> | <symbol> <tupleAny> ;
#[derive(Debug, PartialEq)]
pub struct Call {
  pub lhs: Box<Node>,
  pub args: Vec<Node>,
}

pub fn NewCall(lhs: Node, args: Vec<Node>) -> Node { 
    Node::Call(Call{ lhs: Box::new(lhs), args }) 
}

// ====================================
// Access
// ====================================

// <access> ::= <symbol> '.' <symbol>
//            | <symbol> '.' <access>
//            | <symbol> '.' <call>
#[derive(Debug, PartialEq)]
pub struct Access {
  pub lhs: Box<Node>,
  pub rhs: Box<Node>,
}

pub fn NewAccess(lhs: Node, rhs: Node) -> Node {
  Node::Access(Access{ lhs: Box::new(lhs), rhs: Box::new(rhs) })
}

// <access> ::= <symbol> '[' <expression> ']' 
#[derive(Debug, PartialEq)]
pub struct AccessIndex {
  pub symbol: Box<Node>,
  pub index: Box<Node>,
}

pub fn NewAccessIndex(symbol: Node, index: Node) -> Node {
  Node::AccessIndex(AccessIndex {
    symbol: Box::new(symbol),
    index: Box::new(index)
  })
}

// ====================================
// Tuples
// ====================================

// <tupleAny>      ::= '(' <expression> [ ','  <expression> ]* ')' ;
#[derive(Debug, PartialEq)]
pub struct TupleAny {
  pub items: Vec<Node>,
}

pub fn NewTupleAny(items: Vec<Node>) -> Node {
  Node::TupleAny( TupleAny{ items })
}

// <tupleSymbols>  ::= '(' <symbol> [ ',' <symbol> ]* [ ',' ]? ')' ;
#[derive(Debug, PartialEq)]
pub struct TupleSymbols {
  pub items: Vec<Node>,
}

pub fn NewTupleSymbols(items: Vec<Node>) -> Node {
  Node::TupleSymbols( TupleSymbols{ items })
}

// <tupleStrings>  ::= '(' <string> [ ',' <string> ]* [ ',' ]? ')' ;
#[derive(Debug, PartialEq)]
pub struct TupleString {
  pub items: Vec<Node>,
}

pub fn NewTupleString(items: Vec<Node>) -> Node {
  Node::TupleString( TupleString{ items })
}

// ====================================
// Atoms
// ====================================

#[derive(Debug, PartialEq)]
pub struct Atom {
  pub token: Token,
}

pub fn NewAtom(tok: Token) -> Node { 
  Node::Atom(Atom {token: tok }) 
}

#[derive(Debug, PartialEq)]
pub struct Root {
  pub children: Vec<Node>,
}

pub fn NewRootNode(children: Vec<Node>) -> Node {
  Node::Root( Root { children })
}

#[derive(Debug, PartialEq)]
pub enum Node {
  Root(Root),

  // [x] Let Expressions
  LetExpr(LetExpr),

  // [x] Mut Expressions
  MutExpr(MutExpr),
  
  // [x] Function Expressions
  FnAnon(FnAnon),
  FnSignature(FnSignature),
  FnDeclaration(FnDeclaration),
  
  // [x] If Expressions
  IfExpr(IfExpr),

  // [x] Public Expression
  PubExpr(PubExpr),
  
  // [ ] Data Type Expression
  DataDeclaration(DataDeclaration),
  DataItem(DataItem),
  
  // [ ] Structs
  StructAnon(StructAnon),
  StructDeclaration(StructDeclaration),
  StructField(StructField),

  // [ ] Import & Package
  Package(Package),
  Import(Import),

  // [ ] Match Expressions
  MatchExpression(MatchExpression),
  MatchBranch(MatchBranch),
  
  // [ ] List Expressions
  ListLiteral(ListLiteral),
  ListSplit(ListSplit),

  // [ ] While Expressions 
  WhileExpression(WhileExpression),

  // [ ] Do Expressions
  DoExpression(DoExpression),

  // [ ] Return Expressions
  ReturnExpression(ReturnExpression),

  // Trait and Impl:
  TraitStmt(TraitStmt),
  ImplStmt(ImplStmt),

  // [ ] Type Literals
  TypeFn(TypeFn),
  TypeLst(TypeLst),
  TypeTuple(TypeTuple),
  TypeCmpst(TypeCmpst),
  Ttype(Ttype),
  FnArgTyped(FnArgTyped),
  
  // [ ] Binary & Unary Expressions
  BinaryExpression(BinaryExpression),
  UnaryExpression(UnaryExpression),
  
  // [ ] Call & Access Expressions
  Call(Call),
  Access(Access),
  AccessIndex(AccessIndex),

  // [ ] Tuples
  TupleAny(TupleAny),
  TupleSymbols(TupleSymbols),
  TupleString(TupleString),
  
  // [x] Atoms
  Atom(Atom),
}

/// new_test_token returns a Token with its col and row set to zero, for parser
/// testing purposes.
pub fn new_test_token(token_type: TokenType) -> Token {
  Token { col: 0, row: 0, typ: token_type }
}

pub fn new_test_symbol_token(name: &str) -> Token {
  Token { col: 0, row: 0, typ: TokenType::Symbol(name.to_string())}
}

pub fn new_test_symbol_node(name:&str) -> Node {
  NewAtom(new_test_token(TokenType::Symbol(name.to_string())))
}