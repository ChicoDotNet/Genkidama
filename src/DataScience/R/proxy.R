backend <- new.env(parent = emptyenv())
backend$created <- 0L
backend$fetches <- 0L

make_proxy <- function(state) {
  cache <- new.env(parent = emptyenv())
  subject_created <- FALSE

  function(id) {
    key <- as.character(id)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }

    if (!subject_created) {
      state$created <- state$created + 1L
      subject_created <<- TRUE
    }

    state$fetches <- state$fetches + 1L
    value <- sprintf("doc(%s)", key)
    assign(key, value, envir = cache)
    value
  }
}

proxy_get <- make_proxy(backend)
first <- proxy_get(42)
second <- proxy_get(42)
cat(sprintf("backend=%d;fetches=%d;first=%s;second=%s\n", backend$created, backend$fetches, first, second))
