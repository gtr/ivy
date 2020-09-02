package ast

import (
	"fmt"

	"github.com/gtr/ivy/lexer"
)

// Node represents a node in an ivy syntax tree.
type Node struct {
	Type     string
	Value    string
	Children []*Node
}

func NewNode(t, v string) *Node {
	return &Node{Type: t, Value: v}
}

func NewFromToken(token lexer.Token) *Node {
	return &Node{Type: token.Type, Value: token.Value, Children: nil}
}

func (n *Node) String() string {
	return "(" + n.Type + ", " + n.Value + ")"
}

func (n *Node) CloneTree() *Node {
	c := n.cloneNode()
	for _, v := range n.Children {
		c.Children = append(c.Children, v.CloneTree())
	}
	return c
}

func (n *Node) cloneNode() *Node {
	return &Node{Type: n.Type, Value: n.Value}
}

func printTree(root *Node, level int) {
	fmt.Println(root.String())
	level++
	for _, node := range root.Children {
		for i := 0; i <= level; i++ {
			fmt.Print(" ")
		}
		printTree(node, level)
	}
}
