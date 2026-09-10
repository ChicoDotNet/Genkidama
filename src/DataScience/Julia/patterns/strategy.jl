price(value::Int, strategy::Function) = strategy(value)
regular(value::Int) = value
vip(value::Int) = value * 80 ÷ 100

verify_strategy() = price(100, regular) == 100 && price(100, vip) == 80

verify_strategy() || error("Strategy contract failed")

if abspath(PROGRAM_FILE) == @__FILE__
    println("regular=100;vip=80")
end
