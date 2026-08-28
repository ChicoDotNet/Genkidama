local log = {}
local real = { write = function(_, msg) table.insert(log, msg) end }
local null = { write = function() end }
local function run(logger) logger:write("event") end
run(null); assert(#log == 0)
run(real); assert(log[1] == "event")
return true
