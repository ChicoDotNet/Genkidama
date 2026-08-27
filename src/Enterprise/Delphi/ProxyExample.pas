program ProxyExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  IDocumentStore = interface
    ['{857A576B-645D-4F8C-9A86-0E4F7E0A75A2}']
    function GetDocument(Id: Integer): string;
  end;

  TRemoteDocumentStore = class(TInterfacedObject, IDocumentStore)
  private
    FFetches: Integer;
  public
    function GetDocument(Id: Integer): string;
    property Fetches: Integer read FFetches;
  end;

  TDocumentStoreProxy = class(TInterfacedObject, IDocumentStore)
  private
    FBackend: TRemoteDocumentStore;
    FCache: TDictionary<Integer, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDocument(Id: Integer): string;
    function BackendCount: Integer;
    function FetchCount: Integer;
  end;

function TRemoteDocumentStore.GetDocument(Id: Integer): string;
begin
  Inc(FFetches);
  Result := Format('doc(%d)', [Id]);
end;

constructor TDocumentStoreProxy.Create;
begin
  inherited Create;
  FCache := TDictionary<Integer, string>.Create;
end;

destructor TDocumentStoreProxy.Destroy;
begin
  FCache.Free;
  FBackend.Free;
  inherited;
end;

function TDocumentStoreProxy.GetDocument(Id: Integer): string;
begin
  if FCache.TryGetValue(Id, Result) then
    Exit;

  if FBackend = nil then
    FBackend := TRemoteDocumentStore.Create;

  Result := FBackend.GetDocument(Id);
  FCache.Add(Id, Result);
end;

function TDocumentStoreProxy.BackendCount: Integer;
begin
  if Assigned(FBackend) then Result := 1 else Result := 0;
end;

function TDocumentStoreProxy.FetchCount: Integer;
begin
  if Assigned(FBackend) then Result := FBackend.Fetches else Result := 0;
end;

var
  Proxy: TDocumentStoreProxy;
  First, Second: string;
begin
  Proxy := TDocumentStoreProxy.Create;
  try
    First := Proxy.GetDocument(42);
    Second := Proxy.GetDocument(42);
    Assert(Proxy.BackendCount = 1);
    Assert(Proxy.FetchCount = 1);
    Writeln(Format('backend=%d;fetches=%d;first=%s;second=%s',
      [Proxy.BackendCount, Proxy.FetchCount, First, Second]));
  finally
    Proxy.Free;
  end;
end.
