local remote = { fetch = function(id) return { id=id, value="ok" } end }
local proxy = { cache = {} }
function proxy:fetch(id)
  if not self.cache[id] then self.cache[id] = remote.fetch(id) end
  return self.cache[id]
end
local a, b = proxy:fetch("1"), proxy:fetch("1")
assert(a == b and a.value == "ok")
return true
