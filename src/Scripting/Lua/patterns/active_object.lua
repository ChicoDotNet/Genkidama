local queue, result = {}, {}
local function post(job) table.insert(queue, job) end
local function drain() while #queue > 0 do table.remove(queue, 1)() end end
post(function() table.insert(result, "a") end)
post(function() table.insert(result, "b") end)
assert(#result == 0)
drain()
assert(table.concat(result, "") == "ab")
return true
