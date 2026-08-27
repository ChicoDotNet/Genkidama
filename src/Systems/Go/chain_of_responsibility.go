package main

import (
	"fmt"
	"strings"
)

type Handler struct {
	name      string
	maxAmount int
	next      *Handler
}

func (h *Handler) setNext(next *Handler) *Handler {
	h.next = next
	return next
}

func (h *Handler) handle(amount int, visited *[]string) string {
	*visited = append(*visited, h.name)
	if h.maxAmount < 0 || amount <= h.maxAmount {
		return h.name
	}
	if h.next == nil {
		panic("No handler accepted the request.")
	}
	return h.next.handle(amount, visited)
}

func main() {
	faq := &Handler{name: "faq", maxAmount: 50}
	billing := &Handler{name: "billing", maxAmount: 500}
	escalation := &Handler{name: "escalation", maxAmount: -1}
	faq.setNext(billing).setNext(escalation)

	visited := []string{}
	handled := faq.handle(250, &visited)
	fmt.Printf("visited=%s;handled=%s;result=refund(250)\n", strings.Join(visited, ">"), handled)
}
