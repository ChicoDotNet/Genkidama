legacy_read_fahrenheit <- function() 86L

adapt_to_celsius <- function(read_fahrenheit) {
  function() as.integer((read_fahrenheit() - 32L) * 5L / 9L)
}

read_celsius <- adapt_to_celsius(legacy_read_fahrenheit)

cat(sprintf("legacy=%dF\n", legacy_read_fahrenheit()))
cat(sprintf("adapted=%dC\n", read_celsius()))
