read_fahrenheit() = 86
adapt_to_celsius(read_fahrenheit) = () -> round(Int, (read_fahrenheit() - 32) * 5 / 9)

read_celsius = adapt_to_celsius(read_fahrenheit)

println("legacy=$(read_fahrenheit())F")
println("adapted=$(read_celsius())C")
