# Mediator: colleagues communicate through a coordinator, not directly.
events <- character()

routes <- new.env(parent = emptyenv())
routes$inventory <- function(sender, message) {
  events <<- c(events, paste0("inventory<-", sender, ":", message))
}
routes$payment <- function(sender, message) {
  events <<- c(events, paste0("payment<-", sender, ":", message))
}

mediator_send <- function(sender, recipient, message) {
  if (!exists(recipient, envir = routes, inherits = FALSE)) {
    stop(paste("unknown colleague", recipient))
  }
  get(recipient, envir = routes, inherits = FALSE)(sender, message)
}

payment <- function(message) mediator_send("payment", "inventory", message)
inventory <- function(message) mediator_send("inventory", "payment", message)

payment("paid")
inventory("reserved")
stopifnot(identical(events, c("inventory<-payment:paid", "payment<-inventory:reserved")))

rejected <- inherits(
  try(mediator_send("payment", "unknown", "ignored"), silent = TRUE),
  "try-error"
)
stopifnot(rejected)
