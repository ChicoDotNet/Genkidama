#include <iostream>
#include <memory>

// Abstract Product
class Button {
public:
    virtual ~Button() = default;
    virtual void render() const = 0;
};

class Checkbox {
public:
    virtual ~Checkbox() = default;
    virtual void render() const = 0;
};

// Concrete Product
class DarkButton final : public Button {
public:
    void render() const override {
        std::cout << "Dark Button" << std::endl;
    }
};

class LightButton final : public Button {
public:
    void render() const override {
        std::cout << "Light Button" << std::endl;
    }
};

class DarkCheckbox final : public Checkbox {
public:
    void render() const override {
        std::cout << "Dark Checkbox" << std::endl;
    }
};

class LightCheckbox final : public Checkbox {
public:
    void render() const override {
        std::cout << "Light Checkbox" << std::endl;
    }
};

// Abstract Factory
class UIFactory {
public:
    virtual ~UIFactory() = default;
    [[nodiscard]] virtual std::unique_ptr<Button> createButton() const = 0;
    [[nodiscard]] virtual std::unique_ptr<Checkbox> createCheckbox() const = 0;
};

// Concrete Factory
class DarkFactory final : public UIFactory {
public:
    [[nodiscard]] std::unique_ptr<Button> createButton() const override {
        return std::make_unique<DarkButton>();
    }

    [[nodiscard]] std::unique_ptr<Checkbox> createCheckbox() const override {
        return std::make_unique<DarkCheckbox>();
    }
};

class LightFactory final : public UIFactory {
public:
    [[nodiscard]] std::unique_ptr<Button> createButton() const override {
        return std::make_unique<LightButton>();
    }

    [[nodiscard]] std::unique_ptr<Checkbox> createCheckbox() const override {
        return std::make_unique<LightCheckbox>();
    }
};

// Usage
void createUIComponents(const UIFactory& factory) {
    const auto button = factory.createButton();
    const auto checkbox = factory.createCheckbox();
    button->render();
    checkbox->render();
}

int main() {
    const DarkFactory darkFactory;
    const LightFactory lightFactory;
    createUIComponents(darkFactory);
    createUIComponents(lightFactory);
    return 0;
}
