local tasks = { "a", "b", "c" }
local followers = { "worker-1", "worker-2" }
local handled = {}
local leader = 1
while #tasks > 0 do
  local task = table.remove(tasks, 1)
  table.insert(handled, followers[leader] .. ":" .. task)
  leader = leader % #followers + 1
end
assert(table.concat(handled, ",") == "worker-1:a,worker-2:b,worker-1:c")
return true
