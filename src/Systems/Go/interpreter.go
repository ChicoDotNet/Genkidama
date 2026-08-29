package main

import "fmt"

// Interpreter in idiomatic Go: grammar nodes are values that recursively
// interpret themselves. The tiny grammar is Expr := Number | Expr + Expr.
type Expr interface {
	Interpret() int
}

type Number struct {
	Value int
}

func (n Number) Interpret() int {
	return n.Value
}

type Add struct {
	Left  Expr
	Right Expr
}

func (a Add) Interpret() int {
	return a.Left.Interpret() + a.Right.Interpret()
}

func main() {
	expr := Add{
		Left: Number{Value: 2},
		Right: Add{
			Left:  Number{Value: 3},
			Right: Number{Value: 4},
		},
	}

	value := expr.Interpret()
	if value != 9 {
		panic(fmt.Sprintf("expected 9, got %d", value))
	}

	fmt.Printf("value=%d\n", value)
}
