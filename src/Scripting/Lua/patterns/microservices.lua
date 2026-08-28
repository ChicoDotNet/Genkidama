local catalog = function(id) return { id=id, price=10 } end
local billing = function(item) return item.price * 1.16 end
local item = catalog("sku-1")
assert(billing(item) == 11.6)
return true
