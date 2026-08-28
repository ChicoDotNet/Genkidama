local nodes = { { kind="number", value=2 }, { kind="text", value="hi" } }
local visitor = {
  number = function(node) return node.value * 2 end,
  text = function(node) return string.upper(node.value) end
}
assert(visitor[nodes[1].kind](nodes[1]) == 4)
assert(visitor[nodes[2].kind](nodes[2]) == "HI")
return true
