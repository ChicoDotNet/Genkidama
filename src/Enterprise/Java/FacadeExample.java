final class AuthService {
    String authenticate(String user) {
        return "auth(" + user + ")";
    }
}

final class InventoryService {
    String reserve(String sku) {
        return "reserve(" + sku + ")";
    }
}

final class BillingService {
    String charge(int cents) {
        return "charge(" + cents + ")";
    }
}

final class CheckoutFacade {
    private final AuthService auth;
    private final InventoryService inventory;
    private final BillingService billing;

    CheckoutFacade(AuthService auth, InventoryService inventory, BillingService billing) {
        this.auth = auth;
        this.inventory = inventory;
        this.billing = billing;
    }

    String checkout(String user, String sku, int cents) {
        return String.join(">",
            auth.authenticate(user),
            inventory.reserve(sku),
            billing.charge(cents));
    }
}

public final class FacadeExample {
    public static void main(String[] args) {
        var facade = new CheckoutFacade(
            new AuthService(),
            new InventoryService(),
            new BillingService());

        System.out.println("checkout=" + facade.checkout("alice", "SKU-42", 499));
    }
}
