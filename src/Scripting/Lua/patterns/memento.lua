local editor = { text = "draft" }
local function save(originator) return { text = originator.text } end
local function restore(originator, snapshot) originator.text = snapshot.text end
local snapshot = save(editor)
editor.text = "broken"
restore(editor, snapshot)
assert(editor.text == "draft")
return true
