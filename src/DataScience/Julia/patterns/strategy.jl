price(value::Int, strategy::Function) = strategy(value)
regular(value::Int) = value
vip(value::Int) = value * 80 ÷ 100

@assert price(100, regular) == 100
@assert price(100, vip) == 80
println("regular=100;vip=80")
