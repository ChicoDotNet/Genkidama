local function service(clock)
  return { now = function() return "time:" .. clock() end }
end
local fake_clock = function() return "123" end
local app = service(fake_clock)
assert(app.now() == "time:123")
return true
