local ServiceProfile = {}
ServiceProfile.__index = ServiceProfile

function ServiceProfile.new(name, features)
    return setmetatable({ name = name, features = features }, ServiceProfile)
end

function ServiceProfile:clone()
    local features = {}
    for i, feature in ipairs(self.features) do
        features[i] = feature
    end
    return ServiceProfile.new(self.name, features)
end

function ServiceProfile:describe()
    return self.name .. ": " .. table.concat(self.features, ",")
end

local original = ServiceProfile.new("orders", { "metrics" })
local canary = original:clone()
canary.name = "orders-canary"
table.insert(canary.features, "tracing")

print("original=" .. original:describe())
print("clone=" .. canary:describe())
