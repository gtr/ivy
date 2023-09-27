use std::fmt;

const VERT: &str        = "│";
const HRZT: &str        = "─";
const UP_LEFT: &str     = "┘";
const DOWN_RIGHT: &str  = "┌";
const ERR_START: &str   = "\x1b[91m";
const ERR_END: &str     = "\x1b[0m";

const LEXER_ERR: &str   = "Lexer error";
const PARSER_ERR: &str  = "Parser error";

pub fn new_invalid_token(tok: char, row: usize, col: usize) -> IvyError {
  IvyError {
    start: Location { row, col },
    typ: IvyErrorType::Lexer(LexerError::InvalidToken(tok)),
    file_name: None,
    input: None,
  }
}

pub fn new_unterminated_str(row: usize, col: usize) -> IvyError {
  IvyError {
    start: Location { row, col },
    typ: IvyErrorType::Lexer(LexerError::UnterminatedString),
    file_name: None,
    input: None,
  }
}

pub fn new_parser_expected(row: usize, col: usize, val: String) -> IvyError {
  IvyError {
    start: Location { row, col }, 
    typ: IvyErrorType::Parser(ParserError::Expected(val)),
    file_name: None,
    input: None,
  }
}

pub fn new_parser_expected_one_of(row: usize, col: usize, val: Vec<String>) -> IvyError {
  IvyError {
    start: Location { row, col },
    typ: IvyErrorType::Parser(ParserError::ExpectedOneOf(val)),
    file_name: None,
    input: None,
  }
}

#[derive(Debug)]
pub struct Location {
  row: usize,
  col: usize,
}

#[derive(Debug)]
enum LexerError {
  InvalidToken(char),
  UnterminatedString,
}

#[derive(Debug)]
enum ParserError {
  Expected(String),
  ExpectedOneOf(Vec<String>),
}

#[derive(Debug)]
enum IvyErrorType {
  Lexer(LexerError),
  Parser(ParserError),
}

impl fmt::Display for IvyErrorType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      IvyErrorType::Lexer(err) => write!(f, "{ERR_START}{LEXER_ERR}{ERR_END}: {}", err),
      IvyErrorType::Parser(err) => write!(f, "{ERR_START}{PARSER_ERR}{ERR_END}: {}", err),
    }
  }
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LexerError::InvalidToken(token) => write!(f, "Invalid Token: `{}`", token),
      LexerError::UnterminatedString => write!(f, "Unterminated string"),
    }
  }
}

impl fmt::Display for ParserError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ParserError::Expected(token) => write!(f, "Expected {}", token),
      ParserError::ExpectedOneOf(token) => {
        let one_of = token.join(", ");
        write!(f, "Expected one of: {}", one_of)
      },
    }
  }
}

#[derive(Debug)]
pub struct IvyError {
  input: Option<String>,
  file_name: Option<String>,
  start: Location,
  typ: IvyErrorType,
}

impl IvyError {
  pub fn enrich(&mut self, file_name: &str, input: &str) {
    self.input = Some(input.to_string());
    self.file_name = Some(file_name.to_string());
  }
}

impl fmt::Display for IvyError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let padding = get_padding(self.start.row);
    let indent = " ".repeat(padding);
    let (row, col) = (self.start.row, self.start.col);
    let line = get_line(self.input.clone().unwrap(), row);
    let file_name = self.file_name.as_ref().unwrap();
    let arrow_offset = " ".repeat(col);
    let left_bars = "─".repeat(padding);

    let mut output = String::new();

    output += &format!("\n{}\n", self.typ);
    output += &format!("{indent}{ERR_START}{DOWN_RIGHT}{HRZT}{HRZT}[");
    output += &format!("{file_name} ({row}:{col})]{ERR_END}\n");

    output += &format!("{indent}{ERR_START}{VERT}{ERR_END}\n");
    output += &format!(" {ERR_START}{row}{VERT} {ERR_END}{line}\n");
    output += &format!("{indent}{ERR_START}{VERT}{arrow_offset}^\n");
    output += &format!("{ERR_START}{left_bars}{UP_LEFT}\n{ERR_END}\n");


    write!(f, "{}", output)
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

fn get_line(src: String, row: usize) -> String {
  src.lines().nth(row - 1).unwrap_or("").to_string()
}
