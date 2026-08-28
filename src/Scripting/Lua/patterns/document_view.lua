local document = { title = "Draft", body = "text" }
local compact_view = function(doc) return doc.title end
local detailed_view = function(doc) return doc.title .. ":" .. doc.body end
assert(compact_view(document) == "Draft")
assert(detailed_view(document) == "Draft:text")
return true
