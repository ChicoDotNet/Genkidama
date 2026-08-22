#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

class Prototype {
public:
    virtual ~Prototype() = default;
    [[nodiscard]] virtual std::unique_ptr<Prototype> clone() const = 0;
    [[nodiscard]] virtual std::string describe() const = 0;
};

class ServiceProfile final : public Prototype {
public:
    ServiceProfile(std::string name, std::vector<std::string> features)
        : name_(std::move(name)), features_(std::move(features)) {}

    [[nodiscard]] std::unique_ptr<Prototype> clone() const override {
        return std::make_unique<ServiceProfile>(*this);
    }

    void set_name(std::string name) { name_ = std::move(name); }
    void add_feature(std::string feature) { features_.push_back(std::move(feature)); }

    [[nodiscard]] std::string describe() const override {
        std::string result = name_ + ": ";
        for (std::size_t index = 0; index < features_.size(); ++index) {
            if (index != 0) {
                result += ',';
            }
            result += features_[index];
        }
        return result;
    }

private:
    std::string name_;
    std::vector<std::string> features_;
};

int main() {
    ServiceProfile original{"orders", {"metrics"}};
    auto clone = original.clone();
    auto* canary = dynamic_cast<ServiceProfile*>(clone.get());
    if (canary == nullptr) {
        return 1;
    }

    canary->set_name("orders-canary");
    canary->add_feature("tracing");

    std::cout << "original=" << original.describe() << '\n';
    std::cout << "clone=" << canary->describe() << '\n';
}
