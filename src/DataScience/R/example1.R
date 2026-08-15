make_factory <- function(create_button, create_checkbox) {
  list(
    create_button = create_button,
    create_checkbox = create_checkbox
  )
}

dark_factory <- make_factory(
  function() "Dark Button",
  function() "Dark Checkbox"
)

light_factory <- make_factory(
  function() "Light Button",
  function() "Light Checkbox"
)

render_ui <- function(factory) {
  cat(factory$create_button(), "\n", sep = "")
  cat(factory$create_checkbox(), "\n", sep = "")
}

render_ui(dark_factory)
render_ui(light_factory)
