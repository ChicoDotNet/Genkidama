package main

import "fmt"

type component interface {
	render() string
}

type plainMessage struct{}

func (plainMessage) render() string { return "alert" }

type auditDecorator struct{ inner component }

func (d auditDecorator) render() string { return "audit(" + d.inner.render() + ")" }

type encryptDecorator struct{ inner component }

func (d encryptDecorator) render() string { return "enc(" + d.inner.render() + ")" }

func main() {
	var base component = plainMessage{}
	fmt.Println("base=" + base.render())
	fmt.Println("audit=" + (auditDecorator{inner: base}).render())
	fmt.Println("encrypted=" + (encryptDecorator{inner: base}).render())
	fmt.Println("stacked=" + (auditDecorator{inner: encryptDecorator{inner: base}}).render())
}
