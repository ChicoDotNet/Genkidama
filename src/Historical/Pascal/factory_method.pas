program FactoryMethod;

{$mode objfpc}{$H+}

type
  TDatabaseKind = (dkPostgres, dkMySql);
  TFactoryMethod = function: TDatabaseKind;

function CreatePostgres: TDatabaseKind;
begin
  Result := dkPostgres;
end;

function CreateMySql: TDatabaseKind;
begin
  Result := dkMySql;
end;

procedure UseDatabase(CreateDatabase: TFactoryMethod);
var
  Database: TDatabaseKind;
begin
  Database := CreateDatabase();
  case Database of
    dkPostgres:
      begin
        WriteLn('PostgreSQL connect');
        WriteLn('PostgreSQL query');
      end;
    dkMySql:
      begin
        WriteLn('MySQL connect');
        WriteLn('MySQL query');
      end;
  end;
end;

begin
  UseDatabase(@CreatePostgres);
  UseDatabase(@CreateMySql);
end.
