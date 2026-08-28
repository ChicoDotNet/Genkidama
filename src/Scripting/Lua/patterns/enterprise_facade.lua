local inventory = function() return true end
local payment = function() return "paid" end
local shipping = function() return "tracking-1" end
local facade = {}
function facade.checkout()
  assert(inventory())
  return payment() .. ":" .. shipping()
end
assert(facade.checkout() == "paid:tracking-1")
return true
