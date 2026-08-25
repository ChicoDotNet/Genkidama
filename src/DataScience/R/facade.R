auth_service <- function(user) {
  sprintf("auth(%s)", user)
}

inventory_service <- function(sku) {
  sprintf("reserve(%s)", sku)
}

billing_service <- function(amount) {
  sprintf("charge(%d)", amount)
}

checkout_facade <- function(user, sku, amount) {
  steps <- c(
    auth_service(user),
    inventory_service(sku),
    billing_service(amount)
  )
  paste0("checkout=", paste(steps, collapse = ">"))
}

cat(checkout_facade("alice", "SKU-42", 499), "\n", sep = "")
