// Abstract Product
abstract class Button {
  void render();
}

abstract class Checkbox {
  void render();
}

// Concrete Product
class DarkButton implements Button {
  @override
  void render() {
    print('Dark Button');
  }
}

class LightButton implements Button {
  @override
  void render() {
    print('Light Button');
  }
}

class DarkCheckbox implements Checkbox {
  @override
  void render() {
    print('Dark Checkbox');
  }
}

class LightCheckbox implements Checkbox {
  @override
  void render() {
    print('Light Checkbox');
  }
}

// Abstract Factory
abstract class UIFactory {
  Button createButton();
  Checkbox createCheckbox();
}

// Concrete Factory
class DarkFactory implements UIFactory {
  @override
  Button createButton() {
    return DarkButton();
  }

  @override
  Checkbox createCheckbox() {
    return DarkCheckbox();
  }
}

class LightFactory implements UIFactory {
  @override
  Button createButton() {
    return LightButton();
  }

  @override
  Checkbox createCheckbox() {
    return LightCheckbox();
  }
}

void createUIComponents(UIFactory factory) {
  var button = factory.createButton();
  var checkbox = factory.createCheckbox();
  button.render();
  checkbox.render();
}

// Usage
void main() {
  createUIComponents(DarkFactory());
  createUIComponents(LightFactory());
}
