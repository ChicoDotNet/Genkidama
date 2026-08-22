make_device <- function(name) {
  list(
    power_on = function() paste0(name, ":on"),
    mute = function() paste0(name, ":muted")
  )
}

basic_remote <- function(device) {
  list(activate = function() device$power_on())
}

mute_remote <- function(device) {
  list(activate = function() device$mute())
}

tv <- make_device("TV")
radio <- make_device("Radio")

cat("basic-tv=", basic_remote(tv)$activate(), "\n", sep = "")
cat("basic-radio=", basic_remote(radio)$activate(), "\n", sep = "")
cat("mute-tv=", mute_remote(tv)$activate(), "\n", sep = "")
cat("mute-radio=", mute_remote(radio)$activate(), "\n", sep = "")
