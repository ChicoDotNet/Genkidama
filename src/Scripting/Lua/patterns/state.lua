local door = { state = "closed" }
local transitions = {
  closed = function(self) self.state = "open" end,
  open = function(self) self.state = "closed" end
}
local function toggle(self) transitions[self.state](self) end
toggle(door); assert(door.state == "open")
toggle(door); assert(door.state == "closed")
return true
