local holder = { value = nil, creates = 0 }
function holder:get()
  if self.value == nil then
    self.creates = self.creates + 1
    self.value = { id = self.creates }
  end
  return self.value
end
local a, b = holder:get(), holder:get()
assert(a == b and holder.creates == 1)
return true
