package lexer

const (
	// Keywords
	PROGRAM = "PROGRAM"
	TREE    = "TREE"
	DEFINE  = "DEFINE"
	PRINTLN = "PRINTLN"
	DEFN    = "DEFN"
	PRINT   = "PRINT"
	LAMBDA  = "LAMBDA"
	MATCH   = "MATCH"
	IF      = "IF"

	// Operators
	OPERATOR = "OPERATOR"
	BAR      = "BAR"
	ARROW    = "ARROW"
	AND      = "AND"
	OR       = "OR"
	AT       = "AT"

	// Delimeters
	LPAREN  = "LPAREN"
	RPAREN  = "RPAREN"
	LBRACK  = "LBRACK"
	RBRACK  = "RBRACK"
	LCURLY  = "LCURLY"
	RCURLY  = "RCURLY"
	NEWLINE = "NEWLINE"
	PARAMS  = "PARAMS"

	// Literals
	SYMBOL = "SYMBOL"
	NUMBER = "NUMBER"
	STRING = "STRING"
	LIST   = "LIST"
	LABEL  = "LABEL"
	MAP    = "MAP"
	TRUE   = "TRUE"
	FALSE  = "FALSE"

	UNKNOWN = "UNKNOWN"
	COMMENT = "COMMENT"
	EOF     = "EOF"
)

// keywordLookup maps keywords.
var keywordLookup map[string]string = map[string]string{
	"program": PROGRAM,
	"println": PRINTLN,
	"print":   PRINT,
	"tree":    TREE,
	"defn":    DEFN,
	"def":     DEFINE,
	"lambda":  LAMBDA,
	"match":   MATCH,
	"list":    LIST,
	"map":     MAP,
	"if":      IF,
	"true":    TRUE,
	"false":   FALSE,
	"and":     AND,
	"or":      OR,
}

// Token represents a token in ivy source code.
type Token struct {
	Type  string
	Value string
}

func (t *Token) String() string {
	return "Token(" + t.Type + ", " + t.Value + ")"
}
