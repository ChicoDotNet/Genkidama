local locator = { services = {} }
function locator:register(name, service) self.services[name] = service end
function locator:get(name) assert(self.services[name], "missing service"); return self.services[name] end
locator:register("clock", function() return 123 end)
assert(locator:get("clock")() == 123)
return true
