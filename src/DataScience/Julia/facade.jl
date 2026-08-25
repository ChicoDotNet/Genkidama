auth_service(user) = "auth($(user))"
inventory_service(sku) = "reserve($(sku))"
billing_service(amount) = "charge($(amount))"

function checkout_facade(user, sku, amount)
    steps = (
        auth_service(user),
        inventory_service(sku),
        billing_service(amount),
    )
    return "checkout=" * join(steps, ">")
end

println(checkout_facade("alice", "SKU-42", 499))
