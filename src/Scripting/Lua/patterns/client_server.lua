local server = { handle = function(request) return "response:" .. request end }
local client = { send = function(request) return server.handle(request) end }
assert(client.send("ping") == "response:ping")
return true
