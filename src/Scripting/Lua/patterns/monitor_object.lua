local monitor = { value = 0, locked = false }
function monitor:with_lock(fn)
  assert(not self.locked)
  self.locked = true
  local ok, result = pcall(fn, self)
  self.locked = false
  assert(ok, result)
  return result
end
monitor:with_lock(function(self) self.value = self.value + 1 end)
assert(monitor.value == 1 and monitor.locked == false)
return true
