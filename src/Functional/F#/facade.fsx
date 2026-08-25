type AuthService() =
    member _.Authenticate(user: string) = $"auth({user})"

type InventoryService() =
    member _.Reserve(sku: string) = $"reserve({sku})"

type BillingService() =
    member _.Charge(cents: int) = $"charge({cents})"

type CheckoutFacade(auth: AuthService, inventory: InventoryService, billing: BillingService) =
    member _.Checkout(user: string, sku: string, cents: int) =
        [ auth.Authenticate(user); inventory.Reserve(sku); billing.Charge(cents) ]
        |> String.concat ">"

let facade = CheckoutFacade(AuthService(), InventoryService(), BillingService())
printfn "checkout=%s" (facade.Checkout("alice", "SKU-42", 499))
