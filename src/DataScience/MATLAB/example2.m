% Abstract Factory
function db = createDatabase(dbType)
    if strcmp(dbType, 'postgresql')
        db.connect = @connectPostgres;
        db.query = @queryPostgres;
    elseif strcmp(dbType, 'mysql')
        db.connect = @connectMySQL;
        db.query = @queryMySQL;
    end
end

% Concrete Implementations
function connectPostgres()
    disp('Connecting to PostgreSQL')
end

function queryPostgres()
    disp('Querying PostgreSQL')
end

function connectMySQL()
    disp('Connecting to MySQL')
end

function queryMySQL()
    disp('Querying MySQL')
end

% Usage
db = createDatabase('postgresql');
db.connect()
db.query()

db = createDatabase('mysql');
db.connect()
db.query()
