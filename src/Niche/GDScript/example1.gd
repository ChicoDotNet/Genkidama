extends Node

# Abstract Factory
func create_button(theme: String) -> Callable:
    if theme == "dark":
        return funcref(self, "dark_button")
    elif theme == "light":
        return funcref(self, "light_button")

func create_checkbox(theme: String) -> Callable:
    if theme == "dark":
        return funcref(self, "dark_checkbox")
    elif theme == "light":
        return funcref(self, "light_checkbox")

# Concrete Products
func dark_button():
    print("Dark Button")

func light_button():
    print("Light Button")

func dark_checkbox():
    print("Dark Checkbox")

func light_checkbox():
    print("Light Checkbox")

# Usage
func _ready():
    var button = create_button("dark")
    button.call_func()
    
    var checkbox = create_checkbox("light")
    checkbox.call_func()
