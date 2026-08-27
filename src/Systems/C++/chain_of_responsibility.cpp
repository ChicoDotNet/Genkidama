#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

class RefundHandler {
public:
    virtual ~RefundHandler() = default;

    RefundHandler& setNext(RefundHandler& next) {
        next_ = &next;
        return next;
    }

    std::string handle(int amount, std::vector<std::string>& visited) const {
        visited.push_back(name());
        if (canHandle(amount)) {
            return name();
        }
        if (next_ == nullptr) {
            throw std::runtime_error("No handler accepted the request.");
        }
        return next_->handle(amount, visited);
    }

protected:
    virtual std::string name() const = 0;
    virtual bool canHandle(int amount) const = 0;

private:
    RefundHandler* next_ = nullptr;
};

class FaqHandler final : public RefundHandler {
protected:
    std::string name() const override { return "faq"; }
    bool canHandle(int amount) const override { return amount <= 50; }
};

class BillingHandler final : public RefundHandler {
protected:
    std::string name() const override { return "billing"; }
    bool canHandle(int amount) const override { return amount <= 500; }
};

class EscalationHandler final : public RefundHandler {
protected:
    std::string name() const override { return "escalation"; }
    bool canHandle(int) const override { return true; }
};

int main() {
    FaqHandler faq;
    BillingHandler billing;
    EscalationHandler escalation;
    faq.setNext(billing).setNext(escalation);

    std::vector<std::string> visited;
    const auto handled = faq.handle(250, visited);
    std::ostringstream path;
    for (std::size_t i = 0; i < visited.size(); ++i) {
        if (i > 0) path << '>';
        path << visited[i];
    }

    std::cout << "visited=" << path.str()
              << ";handled=" << handled
              << ";result=refund(250)\n";
}
