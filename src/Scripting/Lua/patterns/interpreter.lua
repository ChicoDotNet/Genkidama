local env = { x = 7, y = 3 }
local expression = { left = "x", op = "+", right = "y" }
local function interpret(node, context)
  local a, b = context[node.left], context[node.right]
  if node.op == "+" then return a + b end
  error("unsupported operator")
end
assert(interpret(expression, env) == 10)
return true
