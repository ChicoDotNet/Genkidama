local visited = {}

local handlers = {
  {
    name = "faq",
    can_handle = function(amount) return amount <= 50 end
  },
  {
    name = "billing",
    can_handle = function(amount) return amount <= 500 end
  },
  {
    name = "escalation",
    can_handle = function(_) return true end
  }
}

local function handle(amount)
  for _, handler in ipairs(handlers) do
    table.insert(visited, handler.name)
    if handler.can_handle(amount) then
      return handler.name, string.format("refund(%d)", amount)
    end
  end
  error("unhandled request")
end

local handled, result = handle(250)
print(string.format("visited=%s;handled=%s;result=%s", table.concat(visited, ">"), handled, result))
