package parser

import (
	"fmt"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/lexer"
)

// Parser parses the lexed tokens into an abstract syntax tree.
type Parser struct {
	Tokens       []lexer.Token
	Position     int
	CurrentToken lexer.Token
}

// NewParser returns a pointer to a Parser object with the given tokens.
func NewParser(tokens []lexer.Token) *Parser {
	p := &Parser{Tokens: tokens, Position: 0}
	p.evaluateLists()
	p.fixNewLines()
	p.CurrentToken = p.Tokens[0]
	return p
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// Parse is the main entry point for parsing.
func (p *Parser) Parse() *ast.Node {
	root := p.parse()
	p.expandMatchBars(root)
	return root
}

/*
 * PRIVATE METHODS --------------------------------------------------
 */

// parse is the main entry point for parsing.
func (p *Parser) parse() *ast.Node {
	if p.Position >= len(p.Tokens) {
		return nil
	}
	token := p.read()
	if token.Type == lexer.LPAREN {
		return p.readExpression()
	} else if token.Type == lexer.RPAREN {
		return nil
	} else {
		return ast.NewFromToken(token)
	}
}

func (p *Parser) readExpression() *ast.Node {
	node := ast.NewFromToken(p.read())
	for p.CurrentToken.Type != lexer.RPAREN {
		node.Children = append(node.Children, p.parse())
	}
	p.read()
	return node
}

func (p *Parser) read() lexer.Token {
	curr := p.CurrentToken
	p.Position++
	if p.Position > len(p.Tokens)-1 {
		p.CurrentToken = lexer.Token{Type: lexer.EOF, Value: ""}
	} else {
		p.CurrentToken = p.Tokens[p.Position]
	}
	return curr
}

// TODO: it works but this can be cleaned up
func (p *Parser) fixNewLines() {
	var next lexer.Token
	open := false
	var newTokens []lexer.Token
	for i := 0; i < len(p.Tokens); i++ {
		curr := p.Tokens[i]
		if i+1 < len(p.Tokens) {
			next = p.Tokens[i+1]
		} else {
			next = lexer.Token{Type: lexer.EOF, Value: ""}
		}
		if curr.Type == lexer.NEWLINE {
			if next.Type == lexer.LPAREN || next.Type == lexer.RPAREN {
				if open {
					newTokens = append(newTokens, lexer.Token{Type: lexer.RPAREN, Value: ")"})
					open = false
				}
			} else {
				if open {
					newTokens = append(newTokens, lexer.Token{Type: lexer.RPAREN, Value: ")"})
					open = false
				}
				newTokens = append(newTokens, lexer.Token{Type: lexer.LPAREN, Value: "("})
				open = true
			}
		} else {
			newTokens = append(newTokens, curr)
		}
	}
	p.Tokens = newTokens

}

// evaluateLists turns [...] and {...} into (list ...) and (map ...)
// respectively.
func (p *Parser) evaluateLists() {
	var newTokens []lexer.Token
	for i := 0; i < len(p.Tokens); i++ {
		curr := p.Tokens[i]
		if curr.Type == lexer.LBRACK {
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.LPAREN,
				Value: "(",
			})
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.LIST,
				Value: "list",
			})
		} else if curr.Type == lexer.RBRACK {
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.RPAREN,
				Value: ")",
			})
		} else if curr.Type == lexer.LCURLY {
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.LPAREN,
				Value: "(",
			})
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.MAP,
				Value: "map",
			})
		} else if curr.Type == lexer.RCURLY {
			newTokens = append(newTokens, lexer.Token{
				Type:  lexer.RPAREN,
				Value: ")",
			})
		} else {
			newTokens = append(newTokens, curr)
		}
	}
	p.Tokens = newTokens
}

// expandMatchBars expands syntax for `bar` tokens in `match`
// operations into boolean expressions if they aren't already.
func (p *Parser) expandMatchBars(root *ast.Node) {
	if root.Type == lexer.MATCH {
		params := root.Children[0].Children
		p.expandMatchBarsHelper(root, params)
	} else {
		for _, v := range root.Children {
			p.expandMatchBars(v)
		}
	}
}

func (p *Parser) expandMatchBarsHelper(root *ast.Node, params []*ast.Node) {
	cases := root.Children[1:]
	for _, v := range cases {
		caseParams, res := getParams(v)
		newCaseParams := make([]*ast.Node, 0)
		for i, p := range caseParams {
			if p.Type != lexer.OPERATOR {
				eq := makeBoolExpr(p, params[i])
				newCaseParams = append(newCaseParams, eq)
			} else {
				newCaseParams = append(newCaseParams, p)
			}
		}
		for _, v := range res {
			newCaseParams = append(newCaseParams, v)
		}
		v.Children = newCaseParams
	}
}

func getParams(bar *ast.Node) ([]*ast.Node, []*ast.Node) {
	var params []*ast.Node
	j := 0
	for i, v := range bar.Children {
		if v.Type != lexer.ARROW {
			params = append(params, v)
		} else {
			j = i
			break
		}
	}
	return params, bar.Children[j:]
}

func makeBoolExpr(a, b *ast.Node) *ast.Node {
	eq := ast.Node{Type: lexer.OPERATOR, Value: "=="}
	eq.Children = []*ast.Node{a, b}
	return &eq
}

func printTokens(tokens []lexer.Token) {
	for _, token := range tokens {
		fmt.Println(token.String())
	}
}
