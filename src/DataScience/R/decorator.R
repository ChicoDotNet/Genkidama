base_component <- function() "alert"

audit_decorator <- function(component) {
  function() paste0("audit(", component(), ")")
}

encrypt_decorator <- function(component) {
  function() paste0("enc(", component(), ")")
}

base <- base_component
audited <- audit_decorator(base)
encrypted <- encrypt_decorator(base)
stacked <- audit_decorator(encrypt_decorator(base))

cat("base=", base(), "\n", sep = "")
cat("audit=", audited(), "\n", sep = "")
cat("encrypted=", encrypted(), "\n", sep = "")
cat("stacked=", stacked(), "\n", sep = "")
