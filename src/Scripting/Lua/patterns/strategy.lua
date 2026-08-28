local strategies = {
  sum = function(a,b) return a+b end,
  max = function(a,b) return math.max(a,b) end
}
local function calculate(strategy, a, b) return strategies[strategy](a,b) end
assert(calculate("sum", 2, 5) == 7)
assert(calculate("max", 2, 5) == 5)
return true
