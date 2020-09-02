package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/gtr/ivy/ast"
	"github.com/gtr/ivy/core"
	"github.com/gtr/ivy/evaluator"
	"github.com/gtr/ivy/lexer"
	"github.com/gtr/ivy/parser"
)

func main() {
	args := os.Args
	treeFlag := false
	last := args[len(args)-1]
	if last == "--tree" || last == "-t" {
		treeFlag = true
		args = args[:len(args)-1]
	} else if last == "--help" || last == "-h" {
		printUsage()
		os.Exit(1)
	}
	if len(args) == 1 {
		startREPL(treeFlag)
	} else if len(args) == 2 {
		readFromFile(args[1], treeFlag)
	} else {
		printUsage()
	}
}

// startREPL starts the ivy REPL.
func startREPL(treeFlag bool) {
	printREPLmessage()
	env := evaluator.NewEnvironment()
	var Green = color("\033[1;32m%s\033[0m")
	var Ivy = Green
	for {
		reader := bufio.NewReader(os.Stdin)
		fmt.Print(Ivy("ivy> "))
		src, _ := reader.ReadString('\n')
		src = padREPLsrc(src)

		if src == "(quit)\n" {
			os.Exit(1)
		}
		core.PrintNode(interpret(src, treeFlag, env))
	}
}

// readFromFile reads an ivy source file.
func readFromFile(file string, treeFlag bool) {
	src, err := ioutil.ReadFile(file)
	if err != nil {
		fmt.Printf("ReadFile(): %s\n", err)
	}
	interpret(string(src), treeFlag, nil)
}

func interpret(src string, treeFlag bool, env *evaluator.Environment) *ast.Node {
	lexer := lexer.NewLexer(src)
	tokens := lexer.Lex()

	parser := parser.NewParser(tokens)
	tree := parser.Parse()

	if treeFlag {
		printTree(tree, 0)
	}

	if env == nil {
		env = evaluator.NewEnvironment()
	}

	eval := evaluator.NewEvaluator(tree, env)
	return eval.Evaluate()
}

func printTree(root *ast.Node, level int) {
	if root != nil {
		printNode(root, level)
		level++
		for _, node := range root.Children {
			for i := 0; i < level; i++ {
				fmt.Print("  ")
			}
			printTree(node, level)
		}
	} else {
		printNode(ast.NewNode("NULL", "null"), level)
	}
}

func printTokens(tokens []lexer.Token) {
	for _, token := range tokens {
		fmt.Println(token.String())
	}
}

func printNode(root *ast.Node, level int) {
	total := len(root.String()) + (level * 2)
	rest := 45 - total
	fmt.Print(root.String())
	for i := 0; i < rest; i++ {
		fmt.Print(" ")
	}
	fmt.Println(":@", level)
}

func padREPLsrc(src string) string {
	if src[0] != '(' && src[len(src)-1] != ')' {
		newSrc := "(" + src + ")"
		return newSrc
	}
	return src
}

func printUsage() {
	fmt.Print("Usage:\n ivy file [options]\n\n")
	fmt.Print(" file\t\tthe file containing ivy source code;\n")
	fmt.Print("\t\tstarts the ivy REPL if not specified\n")
	fmt.Print("Options:\n")
	fmt.Print(" -t, --tree\tprints a syntax tree of the source code\n")
	fmt.Print(" -h, --help\tprints this message\n\n")
}

func printREPLmessage() {
	fmt.Print("The ivy programming language (v0.1)\n")
	fmt.Print("enter (quit) to quit.\n\n")
}

func color(colorString string) func(...interface{}) string {
	sprint := func(args ...interface{}) string {
		return fmt.Sprintf(colorString,
			fmt.Sprint(args...))
	}
	return sprint
}
