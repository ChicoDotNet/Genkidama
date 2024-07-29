// Abstract Product
interface Button {
    fun render()
}

interface Checkbox {
    fun render()
}

// Concrete Product
class DarkButton : Button {
    override fun render() {
        println("Dark Button")
    }
}

class LightButton : Button {
    override fun render() {
        println("Light Button")
    }
}

class DarkCheckbox : Checkbox {
    override fun render() {
        println("Dark Checkbox")
    }
}

class LightCheckbox : Checkbox {
    override fun render() {
        println("Light Checkbox")
    }
}

// Abstract Factory
interface UIFactory {
    fun createButton(): Button
    fun createCheckbox(): Checkbox
}

// Concrete Factory
class DarkFactory : UIFactory {
    override fun createButton(): Button {
        return DarkButton()
    }
    
    override fun createCheckbox(): Checkbox {
        return DarkCheckbox()
    }
}

class LightFactory : UIFactory {
    override fun createButton(): Button {
        return LightButton()
    }
    
    override fun createCheckbox(): Checkbox {
        return LightCheckbox()
    }
}

fun createUIComponents(factory: UIFactory) {
    val button = factory.createButton()
    val checkbox = factory.createCheckbox()
    button.render()
    checkbox.render()
}

fun main() {
    createUIComponents(DarkFactory())
    createUIComponents(LightFactory())
}
