local kernel = { plugins = {} }
function kernel:register(name, plugin) self.plugins[name] = plugin end
function kernel:execute(name, value) return self.plugins[name](value) end
kernel:register("double", function(x) return x * 2 end)
assert(kernel:execute("double", 6) == 12)
return true
