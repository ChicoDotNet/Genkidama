local store = {}
local uow = { pending = {} }
function uow:add(key, value) self.pending[key] = value end
function uow:commit()
  for key, value in pairs(self.pending) do store[key] = value end
  self.pending = {}
end
uow:add("a", 1)
assert(store.a == nil)
uow:commit()
assert(store.a == 1 and next(uow.pending) == nil)
return true
