package main

import "fmt"

type Command interface {
	Execute(balance int) int
}

type Deposit struct{ Amount int }

func (c Deposit) Execute(balance int) int { return balance + c.Amount }

type Withdraw struct{ Amount int }

func (c Withdraw) Execute(balance int) int { return balance - c.Amount }

func main() {
	queue := []Command{Deposit{Amount: 50}, Withdraw{Amount: 20}}
	balance := 100
	for _, command := range queue {
		balance = command.Execute(balance)
	}
	if balance != 130 || len(queue) != 2 {
		panic("Command contract failed")
	}
	fmt.Printf("balance=%d;commands=%d\n", balance, len(queue))
}
