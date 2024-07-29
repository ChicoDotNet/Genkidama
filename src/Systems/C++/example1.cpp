#include <iostream>

// Abstract Product
class Button {
public:
    virtual void render() = 0;
};

class Checkbox {
public:
    virtual void render() = 0;
};

// Concrete Product
class DarkButton : public Button {
public:
    void render() override {
        std::cout << "Dark Button" << std::endl;
    }
};

class LightButton : public Button {
public:
    void render() override {
        std::cout << "Light Button" << std::endl;
    }
};

class DarkCheckbox : public Checkbox {
public:
    void render() override {
        std::cout << "Dark Checkbox" << std::endl;
    }
};

class LightCheckbox : public Checkbox {
public:
    void render() override {
        std::cout << "Light Checkbox" << std::endl;
    }
};

// Abstract Factory
class UIFactory {
public:
    virtual Button* createButton() = 0;
    virtual Checkbox* createCheckbox() = 0;
};

// Concrete Factory
class DarkFactory : public UIFactory {
public:
    Button* createButton() override {
        return new DarkButton();
    }
    Checkbox* createCheckbox() override {
        return new DarkCheckbox();
    }
};

class LightFactory : public UIFactory {
public:
    Button* createButton() override {
        return new LightButton();
    }
    Checkbox* createCheckbox() override {
        return new LightCheckbox();
    }
};

// Usage
void createUIComponents(UIFactory* factory) {
    Button* button = factory->createButton();
    Checkbox* checkbox = factory->createCheckbox();
    button->render();
    checkbox->render();
    delete button;
    delete checkbox;
}

int main() {
    UIFactory* darkFactory = new DarkFactory();
    UIFactory* lightFactory = new LightFactory();
    createUIComponents(darkFactory);
    createUIComponents(lightFactory);
    delete darkFactory;
    delete lightFactory;
    return 0;
}
