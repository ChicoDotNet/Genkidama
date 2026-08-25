type
  AuthService = object
  InventoryService = object
  BillingService = object
  CheckoutFacade = object
    auth: AuthService
    inventory: InventoryService
    billing: BillingService

proc authenticate(_: AuthService; user: string): string =
  "auth(" & user & ")"

proc reserve(_: InventoryService; sku: string): string =
  "reserve(" & sku & ")"

proc charge(_: BillingService; amount: int): string =
  "charge(" & $amount & ")"

proc checkout(facade: CheckoutFacade; user, sku: string; amount: int): string =
  "checkout=" & facade.auth.authenticate(user) & ">" &
    facade.inventory.reserve(sku) & ">" & facade.billing.charge(amount)

let facade = CheckoutFacade(
  auth: AuthService(),
  inventory: InventoryService(),
  billing: BillingService(),
)

echo facade.checkout("alice", "SKU-42", 499)
