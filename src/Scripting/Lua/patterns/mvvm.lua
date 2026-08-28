local model = { first = "Ada", last = "Lovelace" }
local viewmodel = {}
function viewmodel.full_name() return model.first .. " " .. model.last end
local view = function(vm) return "Hello " .. vm.full_name() end
assert(view(viewmodel) == "Hello Ada Lovelace")
return true
