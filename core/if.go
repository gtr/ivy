package core

import (
	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/lexer"
)

// If represents an if expression in ivy.
type If struct {
	BoolExp  *ast.Node
	Branches []*ast.Node
}

// NewIf returns a pointer to a new If object.
func NewIf(node, boolExp *ast.Node) *If {
	branches := make([]*ast.Node, 0)
	branches = append(branches, node.Children[1])
	if len(node.Children) > 2 {
		branches = append(branches, node.Children[2])
	}
	return &If{
		BoolExp:  boolExp,
		Branches: branches,
	}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

func (i *If) Evaluate() *ast.Node {
	if i.BoolExp.Type == lexer.TRUE {
		return i.Branches[0]
	}
	if len(i.Branches) == 2 {
		return i.Branches[1]
	} else {
		return nil
	}
}
