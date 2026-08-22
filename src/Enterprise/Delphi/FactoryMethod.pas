program FactoryMethod;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IDatabase = interface
    ['{DAD5D1A8-9D84-4D91-92E8-8C1A8B0FBB10}']
    procedure Connect;
    procedure Query;
  end;

  TPostgresDatabase = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    procedure Query;
  end;

  TMySqlDatabase = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    procedure Query;
  end;

  TDatabaseCreator = class abstract
  protected
    function CreateDatabase: IDatabase; virtual; abstract;
  public
    procedure UseDatabase;
  end;

  TPostgresCreator = class(TDatabaseCreator)
  protected
    function CreateDatabase: IDatabase; override;
  end;

  TMySqlCreator = class(TDatabaseCreator)
  protected
    function CreateDatabase: IDatabase; override;
  end;

procedure TPostgresDatabase.Connect;
begin
  Writeln('PostgreSQL connect');
end;

procedure TPostgresDatabase.Query;
begin
  Writeln('PostgreSQL query');
end;

procedure TMySqlDatabase.Connect;
begin
  Writeln('MySQL connect');
end;

procedure TMySqlDatabase.Query;
begin
  Writeln('MySQL query');
end;

procedure TDatabaseCreator.UseDatabase;
var
  Database: IDatabase;
begin
  Database := CreateDatabase;
  Database.Connect;
  Database.Query;
end;

function TPostgresCreator.CreateDatabase: IDatabase;
begin
  Result := TPostgresDatabase.Create;
end;

function TMySqlCreator.CreateDatabase: IDatabase;
begin
  Result := TMySqlDatabase.Create;
end;

begin
  with TPostgresCreator.Create do
  try
    UseDatabase;
  finally
    Free;
  end;

  with TMySqlCreator.Create do
  try
    UseDatabase;
  finally
    Free;
  end;
end.
