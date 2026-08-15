type
  ProductFactory = proc (): string {.closure.}
  UIFactory = object
    createButton: ProductFactory
    createCheckbox: ProductFactory

let darkFactory = UIFactory(
  createButton: proc (): string = "Dark Button",
  createCheckbox: proc (): string = "Dark Checkbox"
)

let lightFactory = UIFactory(
  createButton: proc (): string = "Light Button",
  createCheckbox: proc (): string = "Light Checkbox"
)

proc createUIComponents(factory: UIFactory) =
  echo factory.createButton()
  echo factory.createCheckbox()

createUIComponents(darkFactory)
createUIComponents(lightFactory)
