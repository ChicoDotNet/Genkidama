String authService(String user) {
    "auth(${user})"
}

String inventoryService(String sku) {
    "reserve(${sku})"
}

String billingService(int amount) {
    "charge(${amount})"
}

String checkoutFacade(String user, String sku, int amount) {
    [authService(user), inventoryService(sku), billingService(amount)].join('>')
}

println "checkout=${checkoutFacade('alice', 'SKU-42', 499)}"
