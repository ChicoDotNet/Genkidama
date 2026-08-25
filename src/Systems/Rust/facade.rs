struct AuthService;
impl AuthService {
    fn authenticate(&self, user: &str) -> String { format!("auth({user})") }
}

struct InventoryService;
impl InventoryService {
    fn reserve(&self, sku: &str) -> String { format!("reserve({sku})") }
}

struct BillingService;
impl BillingService {
    fn charge(&self, cents: i32) -> String { format!("charge({cents})") }
}

struct CheckoutFacade {
    auth: AuthService,
    inventory: InventoryService,
    billing: BillingService,
}

impl CheckoutFacade {
    fn checkout(&self, user: &str, sku: &str, cents: i32) -> String {
        [
            self.auth.authenticate(user),
            self.inventory.reserve(sku),
            self.billing.charge(cents),
        ]
        .join(">")
    }
}

fn main() {
    let facade = CheckoutFacade {
        auth: AuthService,
        inventory: InventoryService,
        billing: BillingService,
    };
    println!("checkout={}", facade.checkout("alice", "SKU-42", 499));
}
