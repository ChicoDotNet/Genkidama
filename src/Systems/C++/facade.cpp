#include <iostream>
#include <string>

class AuthService {
public:
    std::string authenticate(const std::string& user) const {
        return "auth(" + user + ")";
    }
};

class InventoryService {
public:
    std::string reserve(const std::string& sku) const {
        return "reserve(" + sku + ")";
    }
};

class BillingService {
public:
    std::string charge(int cents) const {
        return "charge(" + std::to_string(cents) + ")";
    }
};

class CheckoutFacade {
public:
    CheckoutFacade(const AuthService& auth, const InventoryService& inventory, const BillingService& billing)
        : auth_(auth), inventory_(inventory), billing_(billing) {}

    std::string checkout(const std::string& user, const std::string& sku, int cents) const {
        return auth_.authenticate(user) + ">" + inventory_.reserve(sku) + ">" + billing_.charge(cents);
    }

private:
    const AuthService& auth_;
    const InventoryService& inventory_;
    const BillingService& billing_;
};

int main() {
    AuthService auth;
    InventoryService inventory;
    BillingService billing;
    CheckoutFacade facade(auth, inventory, billing);

    std::cout << "checkout=" << facade.checkout("alice", "SKU-42", 499) << '\n';
}
