local db = {}
local Record = {}
Record.__index = Record
function Record.new(id, name) return setmetatable({id=id,name=name}, Record) end
function Record:save() db[self.id] = { id=self.id, name=self.name } end
local item = Record.new(1, "one"); item:save()
assert(db[1].name == "one")
return true
