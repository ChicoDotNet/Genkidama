local pool = { free = { {id=1}, {id=2} }, used = {} }
function pool:acquire()
  local item = table.remove(self.free)
  self.used[item.id] = item
  return item
end
function pool:release(item)
  self.used[item.id] = nil
  table.insert(self.free, item)
end
local item = pool:acquire()
assert(pool.used[item.id] == item)
pool:release(item)
assert(pool.used[item.id] == nil and #pool.free == 2)
return true
