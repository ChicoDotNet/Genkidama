package main

import "fmt"

// Abstract Product
type Button interface {
    Render()
}

type Checkbox interface {
    Render()
}

// Concrete Product
type DarkButton struct{}

func (d DarkButton) Render() {
    fmt.Println("Dark Button")
}

type LightButton struct{}

func (l LightButton) Render() {
    fmt.Println("Light Button")
}

type DarkCheckbox struct{}

func (d DarkCheckbox) Render() {
    fmt.Println("Dark Checkbox")
}

type LightCheckbox struct{}

func (l LightCheckbox) Render() {
    fmt.Println("Light Checkbox")
}

// Abstract Factory
type UIFactory interface {
    CreateButton() Button
    CreateCheckbox() Checkbox
}

// Concrete Factory
type DarkFactory struct{}

func (d DarkFactory) CreateButton() Button {
    return DarkButton{}
}

func (d DarkFactory) CreateCheckbox() Checkbox {
    return DarkCheckbox{}
}

type LightFactory struct{}

func (l LightFactory) CreateButton() Button {
    return LightButton{}
}

func (l LightFactory) CreateCheckbox() Checkbox {
    return LightCheckbox{}
}

func createUIComponents(factory UIFactory) {
    button := factory.CreateButton()
    checkbox := factory.CreateCheckbox()
    button.Render()
    checkbox.Render()
}

func main() {
    darkFactory := DarkFactory{}
    lightFactory := LightFactory{}
    createUIComponents(darkFactory)
    createUIComponents(lightFactory)
}
