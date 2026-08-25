class AuthService:
    def authenticate(self, user):
        return "auth(%s)" % user


class InventoryService:
    def reserve(self, sku):
        return "reserve(%s)" % sku


class BillingService:
    def charge(self, cents):
        return "charge(%d)" % cents


class CheckoutFacade:
    def __init__(self, auth, inventory, billing):
        self.auth = auth
        self.inventory = inventory
        self.billing = billing

    def checkout(self, user, sku, cents):
        return "%s>%s>%s" % (
            self.auth.authenticate(user),
            self.inventory.reserve(sku),
            self.billing.charge(cents),
        )


facade = CheckoutFacade(AuthService(), InventoryService(), BillingService())
print("checkout=" + facade.checkout("alice", "SKU-42", 499))
