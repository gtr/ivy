package core

import (
	"log"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/lexer"
)

func First(node *ast.Node) *ast.Node {
	if node.Type == lexer.LIST {
		return node.Children[0]
	}
	log.Panic("Cannot perform `first` on a non-list")
	return nil
}

func Last(node *ast.Node) *ast.Node {
	if node.Type == lexer.LIST {
		return node.Children[len(node.Children)-1]
	}
	log.Panic("Cannot perform `last` on a non-list")
	return nil
}

func Tail(node *ast.Node) *ast.Node {
	if node.Type == lexer.LIST {
		return &ast.Node{
			Type:     lexer.LIST,
			Value:    "list",
			Children: node.Children[1:],
		}
	}
	log.Panic("Cannot perform `tail` on a non-list")
	return nil
}
