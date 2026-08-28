local model = { count = 0 }
local view = { render = function(m) return "count=" .. m.count end }
local controller = { increment = function(m) m.count = m.count + 1 end }
controller.increment(model)
assert(view.render(model) == "count=1")
return true
