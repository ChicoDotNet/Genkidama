program Example2;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IDatabase = interface
    ['{8FDF6945-49BA-4BF3-9E1E-84E9B927B6B0}']
    procedure Connect;
    procedure Query;
  end;

  TPostgres = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    procedure Query;
  end;

  TMySQL = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    procedure Query;
  end;

  IDatabaseFactory = interface
    ['{8D104BD7-8F4D-4F2D-8A75-7FBC8166F6CB}']
    function CreateDatabase: IDatabase;
  end;

  TPostgresFactory = class(TInterfacedObject, IDatabaseFactory)
  public
    function CreateDatabase: IDatabase;
  end;

  TMySQLFactory = class(TInterfacedObject, IDatabaseFactory)
  public
    function CreateDatabase: IDatabase;
  end;

procedure TPostgres.Connect;
begin
  Writeln('Connecting to PostgreSQL');
end;

procedure TPostgres.Query;
begin
  Writeln('Querying PostgreSQL');
end;

procedure TMySQL.Connect;
begin
  Writeln('Connecting to MySQL');
end;

procedure TMySQL.Query;
begin
  Writeln('Querying MySQL');
end;

function TPostgresFactory.CreateDatabase: IDatabase;
begin
  Result := TPostgres.Create;
end;

function TMySQLFactory.CreateDatabase: IDatabase;
begin
  Result := TMySQL.Create;
end;

procedure UseDatabase(factory: IDatabaseFactory);
var
  db: IDatabase;
begin
  db := factory.CreateDatabase;
  db.Connect;
  db.Query;
end;

begin
  try
    UseDatabase(TPostgresFactory.Create);
    UseDatabase(TMySQLFactory.Create);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
