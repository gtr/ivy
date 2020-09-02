package core

import (
	"log"
	"strconv"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/lexer"
)

// Comparison represents a comparison in ivy.
type Comparison struct {
	Op   *ast.Node
	Args []*ast.Node
}

// NewComparison returns a pointer to a new Comparison object.
func NewComparison(op *ast.Node, args []*ast.Node) *Comparison {
	return &Comparison{
		Op:   op,
		Args: args,
	}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// Evaluate evaluates the comparison operation.
func (c *Comparison) Evaluate() *ast.Node {
	switch c.Op.Value {
	case "<":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs < rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs < rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	case ">":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs > rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs > rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	case "<=":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs <= rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs <= rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	case ">=":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs >= rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs >= rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	case "==":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs == rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs == rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	case "!=":
		if c.Args[0].Type == lexer.NUMBER {
			lhs, rhs := c.evaluateNumber()
			if lhs != rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		} else {
			lhs, rhs := c.evaluateNumber()
			if lhs != rhs {
				return &ast.Node{Type: lexer.TRUE, Value: "true"}
			} else {
				return &ast.Node{Type: lexer.FALSE, Value: "false"}
			}
		}
	default:
		// This won't happen btw.
		return &ast.Node{}
	}
}

/*
 * PRIVATE METHODS --------------------------------------------------
 */

func (c *Comparison) evaluateNumber() (float64, float64) {
	lhs, err := strconv.ParseFloat(c.Args[0].Value, 32)
	if err != nil {
		log.Fatal(err)
	}
	rhs, err := strconv.ParseFloat(c.Args[1].Value, 32)
	if err != nil {
		log.Fatal(err)
	}
	return lhs, rhs
}
