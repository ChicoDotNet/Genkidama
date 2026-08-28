local function process(hooks)
  local result = { "load" }
  table.insert(result, hooks.transform())
  table.insert(result, "save")
  return table.concat(result, ">")
end
assert(process({ transform = function() return "normalize" end }) == "load>normalize>save")
return true
