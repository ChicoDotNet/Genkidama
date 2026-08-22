local registry = { count = 0 }

local function instance()
    return registry
end

local first = instance()
local second = instance()
first.count = first.count + 1

print("same=" .. tostring(first == second))
print("count=" .. second.count)
