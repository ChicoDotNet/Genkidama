local bus = { handlers = {} }
function bus:on(topic, fn)
  self.handlers[topic] = self.handlers[topic] or {}
  table.insert(self.handlers[topic], fn)
end
function bus:emit(topic, value) for _, fn in ipairs(self.handlers[topic] or {}) do fn(value) end end
local seen = 0
bus:on("add", function(v) seen = seen + v end)
bus:emit("add", 3); bus:emit("add", 4)
assert(seen == 7)
return true
