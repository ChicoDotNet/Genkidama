object FacadeExample:
  final class AuthService:
    def authenticate(user: String): String = s"auth($user)"

  final class InventoryService:
    def reserve(sku: String): String = s"reserve($sku)"

  final class BillingService:
    def charge(amount: Int): String = s"charge($amount)"

  final class CheckoutFacade(
      auth: AuthService,
      inventory: InventoryService,
      billing: BillingService
  ):
    def checkout(user: String, sku: String, amount: Int): String =
      List(
        auth.authenticate(user),
        inventory.reserve(sku),
        billing.charge(amount)
      ).mkString("checkout=", ">", "")

  def main(args: Array[String]): Unit =
    val facade = CheckoutFacade(AuthService(), InventoryService(), BillingService())
    println(facade.checkout("alice", "SKU-42", 499))
