local subject = { subscribers = {} }
function subject:subscribe(fn) table.insert(self.subscribers, fn) end
function subject:publish(value) for _, fn in ipairs(self.subscribers) do fn(value) end end
local seen = {}
subject:subscribe(function(v) table.insert(seen, "a" .. v) end)
subject:subscribe(function(v) table.insert(seen, "b" .. v) end)
subject:publish("1")
assert(table.concat(seen, ",") == "a1,b1")
return true
