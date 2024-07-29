import Foundation

// Abstract Product
protocol Button {
    func render()
}

protocol Checkbox {
    func render()
}

// Concrete Product
class DarkButton: Button {
    func render() {
        print("Dark Button")
    }
}

class LightButton: Button {
    func render() {
        print("Light Button")
    }
}

class DarkCheckbox: Checkbox {
    func render() {
        print("Dark Checkbox")
    }
}

class LightCheckbox: Checkbox {
    func render() {
        print("Light Checkbox")
    }
}

// Abstract Factory
protocol UIFactory {
    func createButton() -> Button
    func createCheckbox() -> Checkbox
}

// Concrete Factory
class DarkFactory: UIFactory {
    func createButton() -> Button {
        return DarkButton()
    }
    func createCheckbox() -> Checkbox {
        return DarkCheckbox()
    }
}

class LightFactory: UIFactory {
    func createButton() -> Button {
        return LightButton()
    }
    func createCheckbox() -> Checkbox {
        return LightCheckbox()
    }
}

func createUIComponents(factory: UIFactory) {
    let button = factory.createButton()
    let checkbox = factory.createCheckbox()
    button.render()
    checkbox.render()
}

// Usage
createUIComponents(factory: DarkFactory())
createUIComponents(factory: LightFactory())
