package lexer

import (
	"fmt"
)

// Lexer perfroms lexical analysis; breaks `src` down into tokens.
type Lexer struct {
	Src           string
	Position      int
	CurrCharacter byte
	Tokens        []Token
}

// NewLexer returns a pointer to a new Lexer object.
func NewLexer(src string) *Lexer {
	return &Lexer{Src: src, Position: 0, CurrCharacter: src[0]}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// Lex is the main entry point for lexical analysis.
func (l *Lexer) Lex() []Token {
	for l.Position < len(l.Src) {
		token := l.FetchNextToken()
		if token.Type != COMMENT {
			l.Tokens = append(l.Tokens, token)
		}
	}
	l.removeDuplicateNewLines()
	return l.Tokens
}

// FetchNextToken returns the next token in `src`.
func (l *Lexer) FetchNextToken() Token {
	l.skipWhitespace()
	switch l.CurrCharacter {
	case '#':
		l.skipComment()
		return Token{COMMENT, "#"}
	case ':':
		l.read()
		return Token{PARAMS, ":"}
	case '*':
		l.read()
		return Token{OPERATOR, "*"}
	case '/':
		l.read()
		return Token{OPERATOR, "/"}
	case '+':
		l.read()
		return Token{OPERATOR, "+"}
	case '-':
		l.read()
		if l.peek() == '>' {
			l.read()
			return Token{ARROW, "->"}
		}
		return Token{OPERATOR, "-"}
	case '=':
		l.read()
		if l.peek() == '=' {
			l.read()
			return Token{OPERATOR, "=="}
		}
		return Token{OPERATOR, "="}
	case '!':
		l.read()
		if l.peek() == '=' {
			l.read()
			return Token{OPERATOR, "!="}
		}
		l.read()
		return Token{OPERATOR, "!"}
	case '<':
		l.read()
		if l.peek() == '=' {
			l.read()
			return Token{OPERATOR, "<="}
		}
		return Token{OPERATOR, "<"}
	case '>':
		l.read()
		if l.peek() == '=' {
			l.read()
			return Token{OPERATOR, ">="}
		}
		return Token{OPERATOR, ">"}
	case '(':
		l.read()
		return Token{LPAREN, "("}
	case ')':
		l.read()
		return Token{RPAREN, ")"}
	case '[':
		l.read()
		return Token{LBRACK, "["}
	case ']':
		l.read()
		return Token{RBRACK, "]"}
	case '{':
		l.read()
		return Token{LCURLY, "{"}
	case '}':
		l.read()
		return Token{RCURLY, "}"}
	case '\n':
		l.read()
		return Token{NEWLINE, "\\n"}
	case '@':
		l.read()
		str := l.readIdentifier()
		return Token{LABEL, str}
	case '|':
		l.read()
		return Token{BAR, "|"}
	case '"':
		l.read()
		str := l.readString()
		return Token{STRING, str}
	default:
		if l.isLetter(l.CurrCharacter) {
			id := l.readIdentifier()
			if val, ok := keywordLookup[id]; ok {
				return Token{val, id}
			}
			return Token{SYMBOL, id}
		} else if l.isInteger(l.CurrCharacter) {
			return Token{NUMBER, l.readInteger()}
		} else if unknown := l.CurrCharacter; unknown != 0 {
			l.read()
			return Token{UNKNOWN, string(unknown)}
		}
	}
	// Current character is the end of the file.
	return Token{EOF, ""}
}

/*
 * PRIVATE METHODS --------------------------------------------------
 */

// read increments the `Position` pointer and sets the `CurrChar` variable.
func (l *Lexer) read() {
	l.Position++
	if l.Position > len(l.Src)-1 {
		l.CurrCharacter = 0
	} else {
		l.CurrCharacter = l.Src[l.Position]
	}
}

// peek looks ahead and returns the next character after `Position`.
func (l *Lexer) peek() byte {
	if l.Position < len(l.Src)-1 {
		return l.Src[l.Position]
	}
	return 0
}

// readInteger reads and returns an integer from the source.
func (l *Lexer) readInteger() string {
	result := ""
	for l.CurrCharacter != 0 && l.isInteger(l.CurrCharacter) {
		result = result + string(l.CurrCharacter)
		l.read()
	}
	return result
}

func (l *Lexer) readString() string {
	result := ""
	for l.CurrCharacter != 0 && l.CurrCharacter != '"' {
		result = result + string(l.CurrCharacter)
		l.read()
	}
	l.read()
	return result
}

func (l *Lexer) readIdentifier() string {
	result := ""
	for l.CurrCharacter != 0 && l.isLetter(l.CurrCharacter) {
		result = result + string(l.CurrCharacter)
		l.read()
	}
	return result
}

// skipWhitespace advances through `src` while the `currchar` is whitespace.
func (l *Lexer) skipWhitespace() {
	for l.CurrCharacter != 0 &&
		(l.CurrCharacter == ' ' || l.CurrCharacter == '\t') {
		l.read()
	}
}

// skipComment advances through `src` until the the end of the line.
func (l *Lexer) skipComment() {
	for l.CurrCharacter != 0 && l.CurrCharacter != '\n' {
		l.read()
	}
}

// isLetter returns true if the current byte is a letter.
func (l *Lexer) isLetter(b byte) bool {
	return b <= 'z' && b >= 'a' || b <= 'Z' && b >= 'A' || b == '_' || b == '-' || b == '?'
}

// isInteger returns true if the current byte is an integer.
func (l *Lexer) isInteger(b byte) bool {
	return (b <= '9' && b >= '0') || b == '.'
}

func (l *Lexer) removeDuplicateNewLines() {
	skip := false
	var newTokens []Token
	for i := 0; i < len(l.Tokens); i++ {
		if l.Tokens[i].Type != NEWLINE {
			newTokens = append(newTokens, l.Tokens[i])
			skip = false
		} else if !skip {
			newTokens = append(newTokens, l.Tokens[i])
			skip = true
		}
	}
	if newTokens[len(newTokens)-1].Type == NEWLINE {
		newTokens = newTokens[:len(newTokens)-1]
	}
	l.Tokens = newTokens
}

func (l *Lexer) printTokens() {
	for _, token := range l.Tokens {
		fmt.Println(token.String())
	}
}
