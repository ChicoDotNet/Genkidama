handlers <- list(
  list(name = "faq", accepts = function(amount) amount <= 50),
  list(name = "billing", accepts = function(amount) amount <= 500),
  list(name = "escalation", accepts = function(amount) TRUE)
)

route_request <- function(amount, chain, visited = character()) {
  if (length(chain) == 0) {
    stop("No handler accepted the request")
  }

  handler <- chain[[1]]
  visited_now <- c(visited, handler$name)
  if (handler$accepts(amount)) {
    return(list(visited = visited_now, handled = handler$name))
  }

  route_request(amount, chain[-1], visited_now)
}

amount <- 250
outcome <- route_request(amount, handlers)
cat(sprintf(
  "visited=%s;handled=%s;result=refund(%d)\n",
  paste(outcome$visited, collapse = ">"),
  outcome$handled,
  amount
))
