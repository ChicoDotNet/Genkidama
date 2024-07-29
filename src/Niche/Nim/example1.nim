type
  Button = ref object of RootObj
  Checkbox = ref object of RootObj

proc render(b: Button) = discard
proc render(c: Checkbox) = discard

type
  DarkButton = ref object of Button
  LightButton = ref object of Button

proc render(b: DarkButton) =
  echo "Dark Button"

proc render(b: LightButton) =
  echo "Light Button"

type
  DarkCheckbox = ref object of Checkbox
  LightCheckbox = ref object of Checkbox

proc render(c: DarkCheckbox) =
  echo "Dark Checkbox"

proc render(c: LightCheckbox) =
  echo "Light Checkbox"

type
  UIFactory = ref object of RootObj

proc createButton(factory: UIFactory): Button = discard
proc createCheckbox(factory: UIFactory): Checkbox = discard

type
  DarkFactory = ref object of UIFactory
  LightFactory = ref object of UIFactory

proc createButton(factory: DarkFactory): Button =
  result = DarkButton()

proc createButton(factory: LightFactory): Button =
  result = LightButton()

proc createCheckbox(factory: DarkFactory): Checkbox =
  result = DarkCheckbox()

proc createCheckbox(factory: LightFactory): Checkbox =
  result = LightCheckbox()

proc createUIComponents(factory: UIFactory) =
  let button = factory.createButton()
  let checkbox = factory.createCheckbox()
  button.render()
  checkbox.render()

# Usage
let darkFactory = DarkFactory()
let lightFactory = LightFactory()
createUIComponents(darkFactory)
createUIComponents(lightFactory)
