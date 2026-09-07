package main

import "fmt"

type colleague func(sender, message string) string

type checkoutMediator struct {
	colleagues map[string]colleague
}

func newCheckoutMediator() *checkoutMediator {
	return &checkoutMediator{colleagues: make(map[string]colleague)}
}

func (m *checkoutMediator) register(name string, receiver colleague) {
	m.colleagues[name] = receiver
}

func (m *checkoutMediator) send(sender, recipient, message string) (string, error) {
	receiver, ok := m.colleagues[recipient]
	if !ok {
		return "", fmt.Errorf("unknown colleague: %s", recipient)
	}
	return receiver(sender, message), nil
}

func verifyMediator() bool {
	mediator := newCheckoutMediator()
	mediator.register("payment", func(sender, message string) string {
		return "payment<-" + sender + ":" + message
	})
	mediator.register("inventory", func(sender, message string) string {
		return "inventory<-" + sender + ":" + message
	})

	forward, err := mediator.send("payment", "inventory", "paid")
	if err != nil || forward != "inventory<-payment:paid" {
		return false
	}
	back, err := mediator.send("inventory", "payment", "reserved")
	if err != nil || back != "payment<-inventory:reserved" {
		return false
	}
	_, err = mediator.send("payment", "shipping", "dispatch")
	return err != nil
}

func main() {
	if !verifyMediator() {
		panic("Go Mediator failed")
	}
	fmt.Println("Go Mediator: passed")
}
