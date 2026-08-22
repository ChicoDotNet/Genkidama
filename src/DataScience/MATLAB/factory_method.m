function factory_method
%FACTORY_METHOD Keep useDatabase stable while varying the creation hook.
useDatabase(@createPostgres);
useDatabase(@createMySql);
end

function database = createPostgres
database = struct( ...
    'connect', @() fprintf('PostgreSQL connect\n'), ...
    'query', @() fprintf('PostgreSQL query\n'));
end

function database = createMySql
database = struct( ...
    'connect', @() fprintf('MySQL connect\n'), ...
    'query', @() fprintf('MySQL query\n'));
end

function useDatabase(createDatabase)
database = createDatabase();
database.connect();
database.query();
end
