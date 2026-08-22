local function file(bytes)
    return {
        size = function()
            return bytes
        end
    }
end

local function folder(children)
    return {
        size = function()
            local total = 0
            for _, child in ipairs(children) do
                total = total + child.size()
            end
            return total
        end
    }
end

local readme = file(2)
local docs = folder({file(3), file(5)})
local root = folder({readme, docs})

print("leaf=" .. readme.size())
print("docs=" .. docs.size())
print("root=" .. root.size())
