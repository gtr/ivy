package core

import (
	"fmt"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/lexer"
)

func Print(nodes []*ast.Node) {
	for _, n := range nodes {
		if n.Type == lexer.LIST {
			fmt.Print("[")
			for idx, item := range n.Children {
				if item.Type == lexer.LIST {
					sl := []*ast.Node{item}
					Print(sl)
				} else if item.Type == lexer.STRING {
					fmt.Printf("\"%s\"", item.Value)
				} else {
					fmt.Print(item.Value)
				}
				if idx < len(n.Children)-1 {
					fmt.Print(" ")
				}
			}
			fmt.Print("]")
		} else {
			fmt.Print(n.Value)
		}
	}
}

func PrintNode(n *ast.Node) {
	if n.Type == lexer.LIST {
		fmt.Print("[")
		for idx, item := range n.Children {
			if item.Type == lexer.LIST {
				sl := []*ast.Node{item}
				Print(sl)
			} else {
				fmt.Print(item.Value)
			}
			if idx < len(n.Children)-1 {
				fmt.Print(" ")
			}
		}
		fmt.Print("]")
	} else {
		v := n.Value
		if v != "" {
			fmt.Println(v)
		}
	}
}
