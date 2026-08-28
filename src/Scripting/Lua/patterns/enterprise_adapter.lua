local legacy = { get_total = function() return "42" end }
local adapter = { total = function() return tonumber(legacy.get_total()) end }
assert(adapter.total() == 42)
return true
