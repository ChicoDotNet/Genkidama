local abstraction = { value = 1 }
local control = { increment = function(a) a.value = a.value + 1 end }
local presentation = { render = function(a) return "value=" .. a.value end }
control.increment(abstraction)
assert(presentation.render(abstraction) == "value=2")
return true
