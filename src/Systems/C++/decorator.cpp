#include <iostream>
#include <memory>
#include <string>

class Component {
public:
    virtual ~Component() = default;
    [[nodiscard]] virtual std::string render() const = 0;
};

class PlainMessage final : public Component {
public:
    [[nodiscard]] std::string render() const override { return "alert"; }
};

class ComponentDecorator : public Component {
public:
    explicit ComponentDecorator(std::unique_ptr<Component> inner) : inner_(std::move(inner)) {}

protected:
    std::unique_ptr<Component> inner_;
};

class AuditDecorator final : public ComponentDecorator {
public:
    using ComponentDecorator::ComponentDecorator;
    [[nodiscard]] std::string render() const override { return "audit(" + inner_->render() + ")"; }
};

class EncryptDecorator final : public ComponentDecorator {
public:
    using ComponentDecorator::ComponentDecorator;
    [[nodiscard]] std::string render() const override { return "enc(" + inner_->render() + ")"; }
};

int main() {
    PlainMessage base;
    std::cout << "base=" << base.render() << '\n';
    std::cout << "audit=" << AuditDecorator(std::make_unique<PlainMessage>()).render() << '\n';
    std::cout << "encrypted=" << EncryptDecorator(std::make_unique<PlainMessage>()).render() << '\n';
    std::cout << "stacked="
              << AuditDecorator(std::make_unique<EncryptDecorator>(std::make_unique<PlainMessage>())).render()
              << '\n';
}
