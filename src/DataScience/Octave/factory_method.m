function factory_method()
  use_database(@create_postgres);
  use_database(@create_mysql);
end

function database = create_postgres()
  database.connect = @() disp('PostgreSQL connect');
  database.query = @() disp('PostgreSQL query');
end

function database = create_mysql()
  database.connect = @() disp('MySQL connect');
  database.query = @() disp('MySQL query');
end

function use_database(create_database)
  database = create_database();
  database.connect();
  database.query();
end
