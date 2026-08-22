file_node <- function(bytes) {
  list(kind = "file", bytes = bytes)
}

folder_node <- function(...) {
  list(kind = "folder", children = list(...))
}

node_size <- function(node) {
  if (node$kind == "file") {
    return(node$bytes)
  }
  sum(vapply(node$children, node_size, numeric(1)))
}

readme <- file_node(2)
docs <- folder_node(file_node(3), file_node(5))
root <- folder_node(readme, docs)

cat(sprintf("leaf=%d\n", node_size(readme)))
cat(sprintf("docs=%d\n", node_size(docs)))
cat(sprintf("root=%d\n", node_size(root)))
