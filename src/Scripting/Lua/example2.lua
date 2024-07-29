-- Abstract Factory
DatabaseFactory = {}
function DatabaseFactory:createDatabase(dbType)
    if dbType == "postgresql" then
        return Postgres:new()
    elseif dbType == "mysql" then
        return MySQL:new()
    end
end

-- Concrete Products
Postgres = {}
function Postgres:new()
    local newObj = {}
    self.__index = self
    return setmetatable(newObj, self)
end
function Postgres:connect()
    print("Connecting to PostgreSQL")
end
function Postgres:query()
    print("Querying PostgreSQL")
end

MySQL = {}
function MySQL:new()
    local newObj = {}
    self.__index = self
    return setmetatable(newObj, self)
end
function MySQL:connect()
    print("Connecting to MySQL")
end
function MySQL:query()
    print("Querying MySQL")
end

-- Usage
local dbFactory = DatabaseFactory
local db1 = dbFactory:createDatabase("postgresql")
db1:connect()
db1:query()

local db2 = dbFactory:createDatabase("mysql")
db2:connect()
db2:query()
