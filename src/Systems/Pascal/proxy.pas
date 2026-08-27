program ProxyExample;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TDocumentStore = class
  public
    function GetDocument(Id: Integer): string; virtual; abstract;
  end;

  TRemoteDocumentStore = class(TDocumentStore)
  public
    Fetches: Integer;
    function GetDocument(Id: Integer): string; override;
  end;

  TDocumentStoreProxy = class(TDocumentStore)
  private
    FBackend: TRemoteDocumentStore;
    FHasCache: Boolean;
    FCachedId: Integer;
    FCachedValue: string;
  public
    destructor Destroy; override;
    function GetDocument(Id: Integer): string; override;
    function BackendCount: Integer;
    function FetchCount: Integer;
  end;

function TRemoteDocumentStore.GetDocument(Id: Integer): string;
begin
  Inc(Fetches);
  Result := 'doc(' + IntToStr(Id) + ')';
end;

function TDocumentStoreProxy.GetDocument(Id: Integer): string;
begin
  if FHasCache and (FCachedId = Id) then
    Exit(FCachedValue);

  if FBackend = nil then
    FBackend := TRemoteDocumentStore.Create;

  FCachedId := Id;
  FCachedValue := FBackend.GetDocument(Id);
  FHasCache := True;
  Result := FCachedValue;
end;

function TDocumentStoreProxy.BackendCount: Integer;
begin
  if FBackend = nil then Result := 0 else Result := 1;
end;

function TDocumentStoreProxy.FetchCount: Integer;
begin
  if FBackend = nil then Result := 0 else Result := FBackend.Fetches;
end;

destructor TDocumentStoreProxy.Destroy;
begin
  FBackend.Free;
  inherited Destroy;
end;

var
  Store: TDocumentStoreProxy;
  FirstValue, SecondValue: string;
begin
  Store := TDocumentStoreProxy.Create;
  try
    FirstValue := Store.GetDocument(42);
    SecondValue := Store.GetDocument(42);
    WriteLn('backend=', Store.BackendCount, ';fetches=', Store.FetchCount,
      ';first=', FirstValue, ';second=', SecondValue);
  finally
    Store.Free;
  end;
end.
