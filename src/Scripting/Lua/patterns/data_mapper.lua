local db = { [1] = { id=1, name="Ada" } }
local mapper = {}
function mapper.to_domain(row) return { id=row.id, display_name=row.name } end
local person = mapper.to_domain(db[1])
assert(person.display_name == "Ada" and person.name == nil)
return true
