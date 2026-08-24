class AuthService:
    def authenticate(self, user: str) -> str:
        return f"auth({user})"


class InventoryService:
    def reserve(self, sku: str) -> str:
        return f"reserve({sku})"


class BillingService:
    def charge(self, cents: int) -> str:
        return f"charge({cents})"


class CheckoutFacade:
    def __init__(self, auth: AuthService, inventory: InventoryService, billing: BillingService) -> None:
        self._auth = auth
        self._inventory = inventory
        self._billing = billing

    def checkout(self, user: str, sku: str, cents: int) -> str:
        return ">".join(
            (
                self._auth.authenticate(user),
                self._inventory.reserve(sku),
                self._billing.charge(cents),
            )
        )


facade = CheckoutFacade(AuthService(), InventoryService(), BillingService())
print(f"checkout={facade.checkout('alice', 'SKU-42', 499)}")
