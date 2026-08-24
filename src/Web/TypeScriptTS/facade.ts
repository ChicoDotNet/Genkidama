class AuthService {
  authenticate(user: string): string {
    return `auth(${user})`;
  }
}

class InventoryService {
  reserve(sku: string): string {
    return `reserve(${sku})`;
  }
}

class BillingService {
  charge(cents: number): string {
    return `charge(${cents})`;
  }
}

class CheckoutFacade {
  constructor(
    private readonly auth: AuthService,
    private readonly inventory: InventoryService,
    private readonly billing: BillingService,
  ) {}

  checkout(user: string, sku: string, cents: number): string {
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
