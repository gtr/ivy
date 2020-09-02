package core

import (
	"errors"
	"fmt"

	"github.com/gtr/ivy/ast"
)

// Function represents a function written in ivy.
type Function struct {
	Root   *ast.Node
	Params []*ast.Node
}

// NewFunction returns a pointer to a new function.
func NewFunction(lambda *ast.Node) *Function {
	newLambda := lambda.CloneTree()
	return &Function{
		Root:   newLambda.Children[1],
		Params: newLambda.Children[0].Children,
	}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// MatchParams uses a map to match the caller's parameters to the function
// parameters and returns it.
func (f *Function) MatchParams(theirs []*ast.Node) (map[string]*ast.Node, error) {
	// fmt.Println("ours:", f.Params)
	// fmt.Println("theirs:", theirs)
	if len(theirs) != len(f.Params) {
		return nil, errors.New("Mismatched parameters")
	}

	matched := make(map[string]*ast.Node)
	for i, node := range theirs {
		param := f.Params[i].Value
		matched[param] = node
	}
	return matched, nil
}

func printMatch(env map[string]*ast.Node) {
	for k, v := range env {
		fmt.Println(k, "->", v)
	}
}
