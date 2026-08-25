local StyleFactory = {}
StyleFactory.__index = StyleFactory

function StyleFactory.new()
  return setmetatable({ styles = {}, count = 0 }, StyleFactory)
end

function StyleFactory:get(font, size, color)
  local key = font .. "|" .. size .. "|" .. color
  local style = self.styles[key]
  if not style then
    style = { font = font, size = size, color = color }
    self.styles[key] = style
    self.count = self.count + 1
  end
  return style
end

local factory = StyleFactory.new()
local red1 = factory:get("Inter", 12, "red")
local red2 = factory:get("Inter", 12, "red")
factory:get("Inter", 12, "blue")

print(string.format("styles=%d;shared=%s;text=ABC", factory.count, tostring(red1 == red2)))
