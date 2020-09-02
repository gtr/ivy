package core

import (
	"log"
	"strconv"

	"github.com/gtr/ivy/ast"
)

// Arithmetic (n.) represents an artihmetic (adj.) operation in ivy.
type Arithmetic struct {
	Op   *ast.Node
	Args []*ast.Node
}

// NewArithmetic returns a pointer to a new Arithmetic object.
func NewArithmetic(op *ast.Node, args []*ast.Node) *Arithmetic {
	return &Arithmetic{Op: op, Args: args}
}

/*
 * PUBLIC METHODS ---------------------------------------------------
 */

// Evaluate evaluates the arithmetic operation.
func (a *Arithmetic) Evaluate() float64 {
	result, argFloats := a.getArgFloats()
	for _, arg := range argFloats {
		switch a.Op.Value {
		case "*":
			result *= arg
		case "+":
			result += arg
		case "-":
			result -= arg
		default:
			result /= arg
		}
	}
	return result
}

/*
 * PRIVATE METHODS --------------------------------------------------
 */

func (a *Arithmetic) getArgFloats() (float64, []float64) {
	argFloats := make([]float64, 0)
	for _, arg := range a.Args {
		curr, err := strconv.ParseFloat(arg.Value, 32)
		if err != nil {
			log.Fatal(err)
		}
		argFloats = append(argFloats, curr)
	}
	return argFloats[0], argFloats[1:]
}
