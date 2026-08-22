clone_profile <- function(profile) {
  list(
    name = profile$name,
    features = as.character(profile$features)
  )
}

describe <- function(profile) {
  paste0(profile$name, ": ", paste(profile$features, collapse = ","))
}

original <- list(name = "orders", features = c("metrics"))
canary <- clone_profile(original)
canary$name <- "orders-canary"
canary$features <- c(canary$features, "tracing")

cat("original=", describe(original), "\n", sep = "")
cat("clone=", describe(canary), "\n", sep = "")
