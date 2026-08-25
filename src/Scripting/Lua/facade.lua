local AuthService = {}
function AuthService.authenticate(user)
    return "auth(" .. user .. ")"
end

local InventoryService = {}
function InventoryService.reserve(sku)
    return "reserve(" .. sku .. ")"
end

local BillingService = {}
function BillingService.charge(cents)
    return "charge(" .. cents .. ")"
end

local CheckoutFacade = {}
function CheckoutFacade.checkout(user, sku, cents)
    return "checkout="
        .. AuthService.authenticate(user)
        .. ">"
        .. InventoryService.reserve(sku)
        .. ">"
        .. BillingService.charge(cents)
end

print(CheckoutFacade.checkout("alice", "SKU-42", 499))
