# Abstract Factory
create_button <- function(theme) {
  if (theme == "dark") {
    return(dark_button)
  } else if (theme == "light") {
    return(light_button)
  }
}

create_checkbox <- function(theme) {
  if (theme == "dark") {
    return(dark_checkbox)
  } else if (theme == "light") {
    return(light_checkbox)
  }
}

# Concrete Products
dark_button <- function() {
  cat("Dark Button\n")
}

light_button <- function() {
  cat("Light Button\n")
}

dark_checkbox <- function() {
  cat("Dark Checkbox\n")
}

light_checkbox <- function() {
  cat("Light Checkbox\n")
}

# Usage
button <- create_button("dark")
button()

checkbox <- create_checkbox("light")
checkbox()
