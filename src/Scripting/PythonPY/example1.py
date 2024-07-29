class Button:
    def render(self):
        pass

class DarkButton(Button):
    def render(self):
        print("Dark Button")

class LightButton(Button):
    def render(self):
        print("Light Button")

class Checkbox:
    def render(self):
        pass

class DarkCheckbox(Checkbox):
    def render(self):
        print("Dark Checkbox")

class LightCheckbox(Checkbox):
    def render(self):
        print("Light Checkbox")

class UIFactory:
    def create_button(self):
        pass

    def create_checkbox(self):
        pass

class DarkFactory(UIFactory):
    def create_button(self):
        return DarkButton()

    def create_checkbox(self):
        return DarkCheckbox()

class LightFactory(UIFactory):
    def create_button(self):
        return LightButton()

    def create_checkbox(self):
        return LightCheckbox()

def create_ui_components(factory):
    button = factory.create_button()
    checkbox = factory.create_checkbox()
    button.render()
    checkbox.render()

# Usage
create_ui_components(DarkFactory())
create_ui_components(LightFactory())
