class AuthService {
  String authenticate(String user) => 'auth($user)';
}

class InventoryService {
  String reserve(String sku) => 'reserve($sku)';
}

class BillingService {
  String charge(int amount) => 'charge($amount)';
}

class CheckoutFacade {
  CheckoutFacade(this.auth, this.inventory, this.billing);

  final AuthService auth;
  final InventoryService inventory;
  final BillingService billing;

  String checkout(String user, String sku, int amount) {
    return 'checkout=${auth.authenticate(user)}>${inventory.reserve(sku)}>${billing.charge(amount)}';
  }
}

void main() {
  final facade = CheckoutFacade(
    AuthService(),
    InventoryService(),
    BillingService(),
  );

  print(facade.checkout('alice', 'SKU-42', 499));
}
