registry <- new.env(parent = emptyenv())
assign("count", 0L, envir = registry)

registry_instance <- function() registry

first <- registry_instance()
second <- registry_instance()
assign("count", get("count", envir = first) + 1L, envir = first)

cat(sprintf("same=%s\n", tolower(as.character(identical(first, second)))))
cat(sprintf("count=%d\n", get("count", envir = second)))
