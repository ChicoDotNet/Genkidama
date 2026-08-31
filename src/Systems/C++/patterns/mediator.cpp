#include <functional>
#include <stdexcept>
#include <string>
#include <unordered_map>

using Receiver = std::function<std::string(const std::string&, const std::string&)>;

class CheckoutMediator
{
public:
    void register_colleague(const std::string& name, Receiver receiver)
    {
        colleagues_[name] = std::move(receiver);
    }

    std::string send(
        const std::string& sender,
        const std::string& recipient,
        const std::string& message) const
    {
        const auto found = colleagues_.find(recipient);
        if (found == colleagues_.end())
        {
            throw std::invalid_argument("unknown colleague: " + recipient);
        }

        return found->second(sender, message);
    }

private:
    std::unordered_map<std::string, Receiver> colleagues_;
};

bool run()
{
    CheckoutMediator mediator;
    mediator.register_colleague(
        "payment",
        [](const std::string& sender, const std::string& message) {
            return "payment<-" + sender + ":" + message;
        });
    mediator.register_colleague(
        "inventory",
        [](const std::string& sender, const std::string& message) {
            return "inventory<-" + sender + ":" + message;
        });

    const auto reserve = mediator.send("payment", "inventory", "reserve-order-42");
    const auto confirm = mediator.send("inventory", "payment", "reserved-order-42");

    bool rejected_unknown = false;
    try
    {
        (void)mediator.send("payment", "shipping", "dispatch-order-42");
    }
    catch (const std::invalid_argument& error)
    {
        rejected_unknown = std::string(error.what()) == "unknown colleague: shipping";
    }

    return reserve == "inventory<-payment:reserve-order-42"
        && confirm == "payment<-inventory:reserved-order-42"
        && rejected_unknown;
}
