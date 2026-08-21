registry <- new.env(parent = emptyenv())
registry$count <- 0L

registry_instance <- function() registry

first <- registry_instance()
second <- registry_instance()
first$count <- first$count + 1L

cat(sprintf("same=%s\n", tolower(as.character(identical(first, second)))))
cat(sprintf("count=%d\n", second$count))
