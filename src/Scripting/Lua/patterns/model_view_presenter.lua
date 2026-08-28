local model = { name = "Genkidama" }
local view = { text = "" }
local presenter = {}
function presenter:show()
  view.text = "Project: " .. model.name
end
presenter:show()
assert(view.text == "Project: Genkidama")
return true
