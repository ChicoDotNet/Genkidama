local topics = {}
local function subscribe(topic, fn)
  topics[topic] = topics[topic] or {}
  table.insert(topics[topic], fn)
end
local function publish(topic, value)
  for _, fn in ipairs(topics[topic] or {}) do fn(value) end
end
local received = {}
subscribe("news", function(v) table.insert(received, v) end)
publish("news", "release")
assert(received[1] == "release")
return true
