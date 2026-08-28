local rows = { {id=1,name="one"}, {id=2,name="two"} }
local repository = {}
function repository:get(id)
  for _, row in ipairs(rows) do if row.id == id then return { id=row.id, name=row.name } end end
end
assert(repository:get(2).name == "two")
return true
