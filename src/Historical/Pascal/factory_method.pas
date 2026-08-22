program FactoryMethod;

{$mode objfpc}{$H+}

type
  TDatabaseAction = procedure;

  TDatabase = record
    Connect: TDatabaseAction;
    Query: TDatabaseAction;
  end;

  TFactoryMethod = function: TDatabase;

procedure PostgresConnect;
begin
  WriteLn('PostgreSQL connect');
end;

procedure PostgresQuery;
begin
  WriteLn('PostgreSQL query');
end;

procedure MySqlConnect;
begin
  WriteLn('MySQL connect');
end;

procedure MySqlQuery;
begin
  WriteLn('MySQL query');
end;

function CreatePostgres: TDatabase;
begin
  Result.Connect := @PostgresConnect;
  Result.Query := @PostgresQuery;
end;

function CreateMySql: TDatabase;
begin
  Result.Connect := @MySqlConnect;
  Result.Query := @MySqlQuery;
end;

procedure UseDatabase(CreateDatabase: TFactoryMethod);
var
  Database: TDatabase;
begin
  Database := CreateDatabase();
  Database.Connect();
  Database.Query();
end;

begin
  UseDatabase(@CreatePostgres);
  UseDatabase(@CreateMySql);
end.
