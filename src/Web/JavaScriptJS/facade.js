class AuthService {
  authenticate(user) {
    return `auth(${user})`;
  }
}

class InventoryService {
  reserve(sku) {
    return `reserve(${sku})`;
  }
}

class BillingService {
  charge(cents) {
    return `charge(${cents})`;
  }
}

class CheckoutFacade {
  constructor(auth, inventory, billing) {
    this.auth = auth;
    this.inventory = inventory;
    this.billing = billing;
  }

  checkout(user, sku, cents) {
    return [
      this.auth.authenticate(user),
      this.inventory.reserve(sku),
      this.billing.charge(cents),
    ].join(">");
  }
}

const facade = new CheckoutFacade(
  new AuthService(),
  new InventoryService(),
  new BillingService(),
);
console.log(`checkout=${facade.checkout("alice", "SKU-42", 499)}`);
