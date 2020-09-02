package evaluator

import (
	"errors"
	"fmt"
	"log"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/core"
	"github.com/gtr/ivy/lexer"
)

var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

// Environment stores the environment variables as well as pointers to parent
// or children environments.
type Environment struct {
	Vars   map[string]*ast.Node
	Funcs  map[string][]*core.Function
	Maps   map[string]map[string]*ast.Node
	Parent *Environment
	Child  *Environment
}

// NewEnvironment returns a pointer to an empty Environment.
func NewEnvironment() *Environment {
	e := &Environment{
		Vars:  make(map[string]*ast.Node),
		Funcs: make(map[string][]*core.Function),
		Maps:  make(map[string]map[string]*ast.Node),
	}
	reserved := []string{"first", "last", "head", "tail"}
	e.reserveFunctions(reserved)
	return e
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// SetVar sets a new variable or replaces its value in the environment.
func (e *Environment) SetVar(variable string, value *ast.Node) {
	e.Vars[variable] = value
}

// SetFunc sets a new function or adds another definition in the environment.
func (e *Environment) SetFunc(function string, lambda *ast.Node) {
	if val, ok := e.Funcs[function]; ok {
		if len(lambda.Children[0].Children) == len(val[0].Params) {
			e.Funcs[function] = append(e.Funcs[function], core.NewFunction(lambda))
		} else {
			log.Panic("Could not set func: mismatched parameters")
		}
	} else {
		// We've never seen this function before.
		fs := make([]*core.Function, 0)
		fs = append(fs, core.NewFunction(lambda))
		e.Funcs[function] = fs
	}
}

func (e *Environment) SetTempFunc(lambda *ast.Node) *core.Function {
	lambda.Children = lambda.Children[:2]
	return core.NewFunction(lambda)
}

func (e *Environment) SetMap(variable string, myMap *ast.Node) {
	e.Maps[variable] = make(map[string]*ast.Node)
	for _, child := range myMap.Children {
		label := child.Value
		value := child.Children[0]
		e.Maps[variable][label] = value
	}
}

func (e *Environment) GetMap(variable string) map[string]*ast.Node {
	if v, ok := e.Maps[variable]; ok {
		return v
	}
	if e.Parent != nil {
		return e.Parent.GetMap(variable)
	} else {
		return nil
	}
}

// GetVar retrieves a variable in the environment.
func (e *Environment) GetVar(variable string) (*ast.Node, error) {
	if v, ok := e.Vars[variable]; ok {
		if v.Type != lexer.SYMBOL {
			return v, nil
		} else {
			return e.Parent.GetVar(v.Value)
		}
	}
	if e.Parent != nil {
		return e.Parent.GetVar(variable)
	} else {
		return nil, errors.New("No variable: " + variable)
	}
}

// GetFunc retrieves a function definition in the environment.
func (e *Environment) GetFunc(node *ast.Node, args []*ast.Node) (*core.Function, error) {
	symbol := node.Value
	if val, ok := e.Funcs[symbol]; ok {
		for _, currFunc := range val {
			if e.patternMatch(args, currFunc.Params) {
				currFunc.Root = currFunc.Root.CloneTree()
				return currFunc, nil
			}
		}
	}
	if e.Parent != nil {
		return e.Parent.GetFunc(node, args)
	} else {
		return nil, errors.New("No function")
	}
}

// ResolveSymbol resolves a symbol in the current environment as either a
// function, variable, or neither.
func (e *Environment) ResolveSymbol(symbol string) (string, error) {
	if _, ok := e.Funcs[symbol]; ok {
		return "func", nil
	}
	if _, ok := e.Vars[symbol]; ok {
		return "vars", nil
	}
	if _, ok := e.Maps[symbol]; ok {
		return "maps", nil
	}
	if e.Parent != nil {
		return e.Parent.ResolveSymbol(symbol)
	} else {
		return "", errors.New("Cannot resolve symbol:" + symbol)
	}
}

// SetLocalEnvironment defines a new local environment, which will be a child
// of the global environment.
func (e *Environment) SetLocalEnvironment(matched map[string]*ast.Node) *Environment {
	local := &Environment{
		Vars:   matched,
		Parent: e,
	}
	e.Child = local
	return local
}

func printEnv(env map[string]*ast.Node) {
	for k, v := range env {
		fmt.Println(k, "->", v)
	}
}

func (e *Environment) reserveFunctions(funcs []string) {
	for _, v := range funcs {
		e.Funcs[v] = makeFunctionSlice()
	}
}

func makeFunctionSlice() []*core.Function {
	s := make([]*core.Function, 0)
	s = append(s, &core.Function{})
	return s
}

func (e *Environment) patternMatch(theirs, ours []*ast.Node) bool {
	for i, param := range theirs {
		if ours[i].Type == lexer.SYMBOL {
			return true
		}
		if param.Type != ours[i].Type || param.Value != ours[i].Value {
			return false
		}
	}
	return true
}

func printParams(nodes []*ast.Node) {
	for _, k := range nodes {
		fmt.Print(k)
	}
	fmt.Println("")
}
