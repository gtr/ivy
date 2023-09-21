use std::fmt;
use crate::lexer::tokens::*;
// use crate::parser::ast::*;

const VERT: &str        = "│";
const HRZT: &str        = "─";
const UP_LEFT: &str     = "┘";
const DOWN_RIGHT: &str  = "┌";
const ERR_START: &str   = "\x1b[91m";
const ERR_END: &str     = "\x1b[0m";

const LEXER_ERR: &str   = "Lexer error";
const PARSER_ERR: &str  = "Parser error";

/// Holds the two types of lexer errors.
enum LexerErrType {
  InvalidToken(char),
  UnterminatedString,
}

impl fmt::Display for LexerErrType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let msg = match *self {
      LexerErrType::UnterminatedString => format!("Unterminated string"),
      LexerErrType::InvalidToken(car) => format!("Invalid Token: `{car}`"),
    };
    write!(f, "{ERR_START}{LEXER_ERR}{ERR_END}: {msg}")
  }
}

/// Holds the two types of parser errors.
#[derive(Debug)]
enum ParserErrType {
  Expected(String),
  ExpecetedOneOf(Vec<String>),
}

impl fmt::Display for ParserErrType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let msg = match self {
      ParserErrType::Expected(car) => format!("Expected {}", car.clone()),
      ParserErrType::ExpecetedOneOf(car) => {
        let mut comma = false;
        let mut one_of = String::new();
        for tok in car {
          if comma {
            one_of += ", ";
          }
          one_of += &tok;
          comma = true;
        }
        format!("Expected one of: {one_of}")
      },
    };
    write!(f, "{ERR_START}{PARSER_ERR}{ERR_END}: {msg}")
  }
}

pub struct LexerError {
  row: usize,
  col: usize,
  typ: LexerErrType,
}

/// Creates a LexerError struct for an invalid token error.
pub fn new_invalid_token(tok: char, row: usize, col: usize) -> LexerError {
  LexerError { row, col, typ: LexerErrType::InvalidToken(tok) }
}

/// Creates a LexerError struct for an untermianted string error.
pub fn new_unterminated_str(row: usize, col: usize) -> LexerError {
  LexerError { row, col, typ: LexerErrType::UnterminatedString }
}


#[derive(Debug)]
pub struct ParserError {
  row: usize,
  col: usize,
  typ: ParserErrType,
}

pub fn new_parser_expected(row: usize, col: usize, val: String) -> ParserError {
  ParserError { row, col, typ: ParserErrType::Expected(val) }
}

pub fn new_parser_expected_one_of(
  row: usize, col: usize, val: Vec<String>
) -> ParserError {
  ParserError { row, col, typ: ParserErrType::ExpecetedOneOf(val) }
}

impl LexerError {
  pub fn show_error(&self, src: &str, input: &str) {
    let padding = get_padding(self.row);

    self.show_error_title();
    self.show_error_source(input, padding);
    self.show_code_snippet(padding, src);
  }

  fn show_error_title(&self) {
    let title = format!("{}", self.typ);
    println!("\n{title}");
  }

  fn show_error_source(&self, input: &str, padding: usize) {
    let indent = " ".repeat(padding);
    let (row, col) = (self.row, self.col);
    print!("{indent}{ERR_START}{DOWN_RIGHT}{HRZT}{HRZT}[");
    println!("{input} ({row}:{col})]{ERR_END}")
  }

  fn show_code_snippet(&self, padding: usize, src: &str) {
    let indent = " ".repeat(padding);
    let arrow_offset = " ".repeat(self.col);
    let left_bars = "─".repeat(padding);
    let row = self.row;
    let line = get_line(src, self.row);
    println!("{indent}{ERR_START}{VERT}{ERR_END}");
    println!(" {ERR_START}{row}{VERT} {ERR_END}{line}");
    println!("{indent}{ERR_START}{VERT}{arrow_offset}^");
    println!("{ERR_START}{left_bars}{UP_LEFT}");

    println!("{ERR_END}");
  }
}


impl ParserError {
  pub fn show_error(&self, src: &str, input: &str) {
    let padding = get_padding(self.row);

    self.show_error_title();
    self.show_error_source(input, padding);
    self.show_code_snippet(padding, src);
  }

  fn show_error_title(&self) {
    let title = format!("{}", self.typ);
    println!("\n{title}");
  }

  fn show_error_source(&self, input: &str, padding: usize) {
    let indent = " ".repeat(padding);
    let (row, col) = (self.row, self.col);
    print!("{indent}{ERR_START}{DOWN_RIGHT}{HRZT}{HRZT}[");
    println!("{input} ({row}:{col})]{ERR_END}")
  }

  fn show_code_snippet(&self, padding: usize, src: &str) {
    let indent = " ".repeat(padding);
    let arrow_offset = " ".repeat(self.col);
    let left_bars = "─".repeat(padding);
    let row = self.row;
    let line = get_line(src, self.row);
    println!("{indent}{ERR_START}{VERT}{ERR_END}");
    println!(" {ERR_START}{row}{VERT} {ERR_END}{line}");
    println!("{indent}{ERR_START}{VERT}{arrow_offset}^");
    println!("{ERR_START}{left_bars}{UP_LEFT}");

    println!("{ERR_END}");
  }   
}

/// Returns the number of characters a row number takes up.
/// e.g. row `34` takes up 2 characters.
fn get_line_size(row: usize) -> usize {
  (0..).take_while(|i| 10u64.pow(*i) <= row.try_into().unwrap()).count()
}

/// Given a row number, returns how much padding should be added to the left of
/// the error message.
fn get_padding(row: usize) -> usize {
  1 + get_line_size(row)
}

fn get_line(src: &str, row: usize) -> String {
  let mut line = String::new();
  let acc_row = row - 1;
  let mut cur_row = 0;
  let mut ptr = 0;

  while acc_row != cur_row {
    if src.chars().nth(ptr) == Some('\n') {
      cur_row += 1;
    }
    ptr += 1;
  }

  let mut ended = false;
  while !ended {
    match src.chars().nth(ptr) {
      Some('\n')  => ended = true,
      Some(col)   => line.push(col),
      _           => break,
    }
    ptr += 1;
  }

  return line;
}
