local log = {}
local mediator = {}
function mediator:send(sender, message)
  table.insert(log, sender .. ":" .. message)
end
local alice = function(msg) mediator:send("alice", msg) end
local bob = function(msg) mediator:send("bob", msg) end
alice("ping"); bob("pong")
assert(table.concat(log, ",") == "alice:ping,bob:pong")
return true
