local collection = { 2, 4, 6 }
local function iterator(items)
  local i = 0
  return function()
    i = i + 1
    return items[i]
  end
end
local next_item = iterator(collection)
assert(next_item() == 2 and next_item() == 4 and next_item() == 6 and next_item() == nil)
return true
