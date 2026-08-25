struct AuthService {
    func authenticate(_ user: String) -> String { "auth(\(user))" }
}

struct InventoryService {
    func reserve(_ sku: String) -> String { "reserve(\(sku))" }
}

struct BillingService {
    func charge(_ cents: Int) -> String { "charge(\(cents))" }
}

struct CheckoutFacade {
    let auth: AuthService
    let inventory: InventoryService
    let billing: BillingService

    func checkout(user: String, sku: String, cents: Int) -> String {
        "checkout=\(auth.authenticate(user))>\(inventory.reserve(sku))>\(billing.charge(cents))"
    }
}

let facade = CheckoutFacade(auth: AuthService(), inventory: InventoryService(), billing: BillingService())
print(facade.checkout(user: "alice", sku: "SKU-42", cents: 499))
