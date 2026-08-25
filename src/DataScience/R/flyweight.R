style_pool <- new.env(parent = emptyenv())

style_key <- function(font, size, color) paste(font, size, color, sep = "|")

get_style <- function(font, size, color) {
  key <- style_key(font, size, color)
  if (!exists(key, envir = style_pool, inherits = FALSE)) {
    assign(key, list(font = font, size = size, color = color), envir = style_pool)
  }
  get(key, envir = style_pool, inherits = FALSE)
}

red1 <- get_style("Inter", 12, "red")
red2 <- get_style("Inter", 12, "red")
blue <- get_style("Inter", 12, "blue")
stopifnot(identical(blue$color, "blue"))

cat(sprintf("styles=%d;shared=%s;text=ABC\n",
            length(ls(style_pool)),
            tolower(as.character(identical(red1, red2)))))
