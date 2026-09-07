local events = {}

local Mediator = {}
Mediator.__index = Mediator

function Mediator.new()
  return setmetatable({ colleagues = {} }, Mediator)
end

function Mediator:register(name, receive)
  self.colleagues[name] = receive
end

function Mediator:send(sender, recipient, message)
  local receive = self.colleagues[recipient]
  assert(receive, "unknown colleague: " .. recipient)
  receive(sender, message)
end

local mediator = Mediator.new()
mediator:register("inventory", function(sender, message)
  table.insert(events, "inventory<-" .. sender .. ":" .. message)
end)
mediator:register("payment", function(sender, message)
  table.insert(events, "payment<-" .. sender .. ":" .. message)
end)

local payment = function(message)
  mediator:send("payment", "inventory", message)
end

local inventory = function(message)
  mediator:send("inventory", "payment", message)
end

payment("paid")
inventory("reserved")

assert(table.concat(events, ",") == "inventory<-payment:paid,payment<-inventory:reserved")
local ok = pcall(function()
  mediator:send("payment", "unknown", "ignored")
end)
assert(not ok)

return true
