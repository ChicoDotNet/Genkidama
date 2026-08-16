text_builder <- function() {
  parts <- character()
  list(
    reset = function() { parts <<- character() },
    add_title = function(title) { parts <<- c(parts, paste0("# ", title)) },
    add_section = function(heading, body) { parts <<- c(parts, paste0("## ", heading), body) },
    build = function() paste(parts, collapse = "\n")
  )
}

html_builder <- function() {
  parts <- character()
  list(
    reset = function() { parts <<- character() },
    add_title = function(title) { parts <<- c(parts, paste0("<h1>", title, "</h1>")) },
    add_section = function(heading, body) {
      parts <<- c(parts, paste0("<h2>", heading, "</h2>"), paste0("<p>", body, "</p>"))
    },
    build = function() paste(parts, collapse = "")
  )
}

build_availability_report <- function(builder) {
  builder$reset()
  builder$add_title("Service status")
  builder$add_section("Availability", "99.95%")
  builder$build()
}

cat(build_availability_report(text_builder()), "\n---\n", sep = "")
cat(build_availability_report(html_builder()), "\n", sep = "")
