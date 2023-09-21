use std::fmt;

/// Token represents a token in the ivy programming language.
#[derive(Debug, PartialEq)]
pub struct Token {
  pub col: usize,
  pub row: usize,
  pub typ: TokenType,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({}, {})\t{}", self.row, self.col, self.typ)
  }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
  // Atoms
  Integer(i32),   /* 42           */
  String(String), /* "hello"      */
  Symbol(String), /* user-defined */
  None,           /* None         */

  // Operators
  Plus,           /* +  */    Minus,      /* -  */
  PlusPlus,       /* ++ */    Star,       /* *  */
  Slash,          /* /  */    Bind,       /* =  */
  Eq,             /* == */    Not,        /* !  */
  NotEq,          /* != */    Greater,    /* >  */
  GreaterEqual,   /* >= */    Less,       /* <  */
  LessEqual,      /* <= */    And,        /* && */
  Or,             /* || */

  // Delimeters
  Bar,            /*  |  */   Arrow,          /*  -> */
  EqArrow,        /*  => */   Dot,            /*  .  */
  Comma,          /*  ,  */   Colon,          /*  :  */
  Semicolon,      /*  ;  */   DoubleColon,    /*  :: */
  LParen,         /*  (  */   RParen,         /*  )  */
  LBracket,       /*  [  */   RBracket,       /*  ]  */
  LCurly,         /*  {  */   RCurly,         /*  }  */
  CommentLeft,    /*  (- */   CommentRight,   /*  -) */
  At,             /*  @  */   Comment,        /*  -- */

  // Keywords
  Let,            /* let     */   Mut,        /* mut     */
  Fn,             /* fn      */   If,         /* if      */
  Then,           /* then    */   Else,       /* else    */
  Pub,            /* pub     */   Data,       /* data    */
  Struct,         /* struct  */   Package,    /* package */
  Import,         /* import  */   Match,      /* match   */
  With,           /* with    */   While,      /* while   */   
  Do,             /* do      */   Return,     /* return  */
  Trait,          /* trait   */   Impl,       /* impl    */
  For,            /* for     */

  // Decorators
  Decorator(String),

  EOF,            /* end of file */
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self {
      // Atoms
      TokenType::Integer(i)   => write!(f, "Integer '{}'", i),
      TokenType::String(s) => write!(f, "String '{}'", s),
      TokenType::Symbol(s) => write!(f, "Symbol '{}'", s),
      TokenType::None               => write!(f, "None"),
      
      // Operators
      TokenType::Plus         => write!(f, "+"),
      TokenType::PlusPlus     => write!(f, "++"),
      TokenType::Minus        => write!(f, "-"),
      TokenType::Star         => write!(f, "*"),
      TokenType::Slash        => write!(f, "/"),
      TokenType::Bind         => write!(f, "="),
      TokenType::Eq           => write!(f, "=="),
      TokenType::Not          => write!(f, "!"),
      TokenType::NotEq        => write!(f, "!="),
      TokenType::Greater      => write!(f, ">"),
      TokenType::GreaterEqual => write!(f, ">="),
      TokenType::Less         => write!(f, "<"),
      TokenType::LessEqual    => write!(f, "<="),
      TokenType::And          => write!(f, "&&"),
      TokenType::Or           => write!(f, "||"),
      
      // Delimeters
      TokenType::Bar          => write!(f, "|"),
      TokenType::EqArrow      => write!(f, "=>"),
      TokenType::Arrow        => write!(f, "->"),
      TokenType::Comma        => write!(f, ","),
      TokenType::Dot          => write!(f, "."),
      TokenType::Semicolon    => write!(f, ";"),
      TokenType::Colon        => write!(f, ":"),
      TokenType::DoubleColon  => write!(f, "::"),
      TokenType::LParen       => write!(f, "("),
      TokenType::RParen       => write!(f, ")"),
      TokenType::LBracket     => write!(f, "["),
      TokenType::RBracket     => write!(f, "]"),
      TokenType::LCurly       => write!(f, "{{"),
      TokenType::RCurly       => write!(f, "}}"),
      TokenType::Comment      => write!(f, "--"),
      TokenType::CommentLeft  => write!(f, "(-"),
      TokenType::CommentRight => write!(f, "-)"),
      TokenType::At           => write!(f, "@"),
      
      // Keywords
      TokenType::Let          => write!(f, "let"),
      TokenType::Fn           => write!(f, "fn"),
      TokenType::If           => write!(f, "if"),
      TokenType::Then         => write!(f, "then"),
      TokenType::Else         => write!(f, "else"),
      TokenType::Pub          => write!(f, "pub"),
      TokenType::Mut          => write!(f, "mut"),
      TokenType::Data         => write!(f, "Data"),
      TokenType::Import       => write!(f, "import"),
      TokenType::Package      => write!(f, "package"),
      TokenType::Struct       => write!(f, "struct"),
      TokenType::Match        => write!(f, "match"),
      TokenType::With         => write!(f, "with"),
      TokenType::While        => write!(f, "while"),
      TokenType::Do           => write!(f, "do"),
      TokenType::Return       => write!(f, "return"),
      TokenType::Trait        => write!(f, "trait"),
      TokenType::Impl         => write!(f, "impl"),
      TokenType::For          => write!(f, "for"),
      
      TokenType::Decorator(s) => write!(f, "Decorator: {}", s),

      TokenType::EOF          => write!(f, "EOF")
    }
  }
}
