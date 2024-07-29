trait Button {
  def render(): Unit
}

class DarkButton extends Button {
  def render(): Unit = println("Dark Button")
}

class LightButton extends Button {
  def render(): Unit = println("Light Button")
}

trait Checkbox {
  def render(): Unit
}

class DarkCheckbox extends Checkbox {
  def render(): Unit = println("Dark Checkbox")
}

class LightCheckbox extends Checkbox {
  def render(): Unit = println("Light Checkbox")
}

trait UIFactory {
  def createButton(): Button
  def createCheckbox(): Checkbox
}

class DarkFactory extends UIFactory {
  def createButton(): Button = new DarkButton
  def createCheckbox(): Checkbox = new DarkCheckbox
}

class LightFactory extends UIFactory {
  def createButton(): Button = new LightButton
  def createCheckbox(): Checkbox = new LightCheckbox
}

object Example1 {
  def main(args: Array[String]): Unit = {
    val darkFactory: UIFactory = new DarkFactory
    val lightFactory: UIFactory = new LightFactory

    val darkButton = darkFactory.createButton()
    val darkCheckbox = darkFactory.createCheckbox()
    darkButton.render()
    darkCheckbox.render()

    val lightButton = lightFactory.createButton()
    val lightCheckbox = lightFactory.createCheckbox()
    lightButton.render()
    lightCheckbox.render()
  }
}
