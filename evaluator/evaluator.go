package evaluator

import (
	"fmt"
	"log"
	"strings"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/core"
	"github.com/gtr/ivy/lexer"
)

// Evaluator evaluates and executes an ivy syntax tree.
type Evaluator struct {
	Tree *ast.Node
	Env  *Environment
}

// NewEvaluator returns a pointer to a new evaluator based on an abstract
// syntax tree and a default environment.
func NewEvaluator(root *ast.Node, env *Environment) *Evaluator {
	return &Evaluator{Tree: root, Env: env}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

func (e *Evaluator) Evaluate() *ast.Node {
	return e.eval(e.Tree)
}

/*
 * PRIVATE METHODS --------------------------------------------------
 */

// eval evaluates the program recursively.
func (e *Evaluator) eval(node *ast.Node) *ast.Node {
	if node == nil {
		return &ast.Node{}
	} else if node.Type == lexer.PROGRAM {
		for _, child := range node.Children {
			e.eval(child)
		}
	} else if node.Type == lexer.DEFINE {
		symbol := node.Children[0].Value
		rhs := e.eval(node.Children[1])
		if rhs.Type == lexer.LAMBDA {
			// We're defining a function.
			e.Env.SetFunc(symbol, rhs)
		} else if rhs.Type == lexer.MAP {
			// We're defining a map.
			e.Env.SetMap(symbol, rhs)
		} else {
			// We're defining a variable.
			e.Env.SetVar(symbol, rhs)
		}
	} else if node.Type == lexer.DEFN {
		symbol := node.Children[0].Value
		c := make([]*ast.Node, 0)
		c = append(c, node.Children[1], node.Children[2])
		rhs := &ast.Node{
			Type:     lexer.LAMBDA,
			Value:    "lambda",
			Children: c,
		}
		e.Env.SetFunc(symbol, rhs)
	} else if node.Type == lexer.SYMBOL {
		return e.evaluateSymbol(node)
	} else if node.Type == lexer.LAMBDA {
		if len(node.Children) > 2 {
			return e.evaluateTempFunction(node)
		}
		return e.evaluateLambdaExpr(node)
	} else if node.Type == lexer.NUMBER {
		return node
	} else if node.Type == lexer.STRING {
		return node
	} else if node.Type == lexer.TRUE {
		return node
	} else if node.Type == lexer.FALSE {
		return node
	} else if node.Type == lexer.LABEL {
		return node
	} else if node.Type == lexer.IF {
		boolExp := e.eval(node.Children[0])
		ivyIf := core.NewIf(node, boolExp)
		branch := ivyIf.Evaluate()
		if branch != nil {
			return e.eval(branch)
		}
	} else if node.Type == lexer.AND || node.Type == lexer.OR {
		lhs := e.eval(node.Children[0])
		rhs := e.eval(node.Children[1])
		if (lhs.Type == lexer.TRUE || lhs.Type == lexer.FALSE) &&
			(rhs.Type == lexer.TRUE || rhs.Type == lexer.FALSE) {
			return e.evaluateLogic(lhs, rhs, node.Type)
		} else {
			log.Panic("Cannot perform " + node.Type + " on non-bool")
		}
	} else if node.Type == lexer.OPERATOR {
		args := make([]*ast.Node, 0)
		for _, child := range node.Children {
			args = append(args, e.eval(child))
		}
		if strings.Contains("+-*/", node.Value) {
			arith := core.NewArithmetic(node, args)
			return &ast.Node{
				Type:     lexer.NUMBER,
				Value:    fmt.Sprint(arith.Evaluate()),
				Children: nil,
			}
		} else {
			comparison := core.NewComparison(node, args)
			return comparison.Evaluate()
		}
	} else if node.Type == lexer.BAR {
		return e.evaluateBar(node)
	} else if node.Type == lexer.MATCH {
		return e.evaluateMatch(node)
	} else if node.Type == lexer.PRINT {
		e.printSomething(node)
	} else if node.Type == lexer.PRINTLN {
		e.printSomething(node)
		fmt.Printf("\n")
	} else if node.Type == lexer.LIST {
		return node
	} else if node.Type == lexer.MAP {
		return node
	} else if node.Type == lexer.TREE {
		return node
	}
	return &ast.Node{}
}

func (e *Evaluator) printSomething(node *ast.Node) {
	args := make([]*ast.Node, 0)
	for _, child := range node.Children {
		args = append(args, e.eval(child))
	}
	core.Print(args)
}

func (e *Evaluator) evaluateLambdaExpr(lambda *ast.Node) *ast.Node {
	paramsNodes := lambda.Children[0].Children
	paramsStrings := make([]string, 0)
	for _, node := range paramsNodes {
		paramsStrings = append(paramsStrings, node.Value)
	}
	lambda = lambda.CloneTree()
	return e.findAndReplace(lambda, paramsStrings)
}

func (e *Evaluator) findAndReplace(node *ast.Node, params []string) *ast.Node {
	for k, v := range node.Children {
		node.Children[k] = e.findAndReplace(v, params)
	}
	if node.Type == lexer.SYMBOL {
		if !stringInSlice(node.Value, params) {
			resolved, err := e.Env.ResolveSymbol(node.Value)
			if err != nil {
				log.Fatal(err)
			}
			if resolved == "vars" {
				myVar, err := e.Env.GetVar(node.Value)
				if err != nil {
					log.Fatal(err)
				}
				return myVar
			}
			return node
		}
	}
	return node
}

func stringInSlice(a string, list []string) bool {
	for _, b := range list {
		if b == a {
			return true
		}
	}
	return false
}

func (e *Evaluator) evaluateSymbol(node *ast.Node) *ast.Node {
	resolved, err := e.Env.ResolveSymbol(node.Value)
	if err != nil {
		log.Fatal(err)
	}
	if resolved == "func" {
		if node.Value == "first" || node.Value == "head" {
			list := e.eval(node.Children[0])
			return core.First(list)
		} else if node.Value == "last" {
			list := e.eval(node.Children[0])
			return core.Last(list)
		} else if node.Value == "tail" {
			list := e.eval(node.Children[0])
			return core.Tail(list)
		}
		return e.evaluateFunction(node)
	} else if resolved == "maps" {
		label := e.eval(node.Children[0])
		m := e.Env.GetMap(node.Value)
		v := m[label.Value]
		return v
	} else if resolved == "vars" {
		val, err := e.Env.GetVar(node.Value)
		if err != nil {
			log.Fatal(err)
		}
		return val
	}
	return &ast.Node{}
}

func (e *Evaluator) evaluateLogic(lhs, rhs *ast.Node, op string) *ast.Node {
	if op == lexer.AND {
		if lhs.Type == lexer.TRUE && rhs.Type == lexer.TRUE {
			return &ast.Node{Type: lexer.TRUE, Value: "true", Children: nil}
		} else {
			return &ast.Node{Type: lexer.FALSE, Value: "false", Children: nil}
		}
	} else {
		if lhs.Type == lexer.TRUE || rhs.Type == lexer.TRUE {
			return &ast.Node{Type: lexer.TRUE, Value: "true", Children: nil}
		} else {
			return &ast.Node{Type: lexer.FALSE, Value: "false", Children: nil}
		}
	}
}

func (e *Evaluator) evaluateMatch(node *ast.Node) *ast.Node {
	bars := node.Children[1:]
	for _, v := range bars {
		ans := e.eval(v)
		if ans != nil {
			return ans
		}
	}
	return &ast.Node{}
}

func (e *Evaluator) evaluateBar(node *ast.Node) *ast.Node {
	exprs := make([]*ast.Node, 0)
	j := 0
	for i, v := range node.Children {
		if v.Type != lexer.ARROW {
			exprs = append(exprs, v)
		} else {
			j = i
			break
		}
	}
	for _, v := range exprs {
		b := e.eval(v)
		if b.Type == lexer.FALSE {
			return nil
		}
	}
	return e.eval(node.Children[j+1])
}

// evaluateFunction evaluates a function call.
func (e *Evaluator) evaluateFunction(node *ast.Node) *ast.Node {
	// Evaluate the args
	params := node.Children
	args := make([]*ast.Node, 0)
	for _, param := range params {
		args = append(args, e.eval(param))
	}

	// Get function from environment.
	function, err := e.Env.GetFunc(node, args)
	if err != nil {
		log.Fatal(err)
	}

	return e.evalFunction(function, args)
}

func (e *Evaluator) evaluateTempFunction(node *ast.Node) *ast.Node {
	params := node.Children[2:]
	args := make([]*ast.Node, 0)
	for _, param := range params {
		args = append(args, e.eval(param))
	}
	function := e.Env.SetTempFunc(node)

	return e.evalFunction(function, args)
}

func (e *Evaluator) evalFunction(function *core.Function, args []*ast.Node) *ast.Node {
	// Match the parameters.
	matched, err := function.MatchParams(args)
	if err != nil {
		log.Fatal(err)
	}

	// Create a local environment.
	local := e.Env.SetLocalEnvironment(matched)
	e.Env = local

	// Evaluate function.
	root := function.Root
	value := e.eval(root)

	// Get rid of local environment.
	e.Env = local.Parent
	return value
}
