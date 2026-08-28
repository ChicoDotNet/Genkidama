local broker = { handlers = {} }
function broker:register(topic, fn) self.handlers[topic] = fn end
function broker:request(topic, payload) return self.handlers[topic](payload) end
broker:register("square", function(x) return x*x end)
assert(broker:request("square", 5) == 25)
return true
