class AuthService {
    fun authenticate(user: String) = "auth($user)"
}

class InventoryService {
    fun reserve(sku: String) = "reserve($sku)"
}

class BillingService {
    fun charge(cents: Int) = "charge($cents)"
}

class CheckoutFacade(
    private val auth: AuthService,
    private val inventory: InventoryService,
    private val billing: BillingService,
) {
    fun checkout(user: String, sku: String, cents: Int): String =
        "checkout=${auth.authenticate(user)}>${inventory.reserve(sku)}>${billing.charge(cents)}"
}

fun main() {
    val facade = CheckoutFacade(AuthService(), InventoryService(), BillingService())
    println(facade.checkout("alice", "SKU-42", 499))
}
