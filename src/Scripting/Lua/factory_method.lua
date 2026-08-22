local function use_database(create_database)
    local database = create_database()
    database.connect()
    database.query()
end

local function create_postgres()
    return {
        connect = function() print("PostgreSQL connect") end,
        query = function() print("PostgreSQL query") end,
    }
end

local function create_mysql()
    return {
        connect = function() print("MySQL connect") end,
        query = function() print("MySQL query") end,
    }
end

use_database(create_postgres)
use_database(create_mysql)
