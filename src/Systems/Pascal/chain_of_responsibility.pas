program ChainOfResponsibilityExample;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  THandler = class
  private
    FName: string;
    FLimit: Integer;
    FNext: THandler;
  public
    constructor Create(const AName: string; ALimit: Integer; ANext: THandler);
    destructor Destroy; override;
    function Handle(Amount: Integer; var Visited: string): string;
  end;

constructor THandler.Create(const AName: string; ALimit: Integer; ANext: THandler);
begin
  inherited Create;
  FName := AName;
  FLimit := ALimit;
  FNext := ANext;
end;

destructor THandler.Destroy;
begin
  FNext.Free;
  inherited Destroy;
end;

function THandler.Handle(Amount: Integer; var Visited: string): string;
begin
  if Visited <> '' then
    Visited := Visited + '>';
  Visited := Visited + FName;

  if Amount <= FLimit then
    Exit(FName);

  if FNext = nil then
    raise Exception.Create('No handler accepted the request');

  Result := FNext.Handle(Amount, Visited);
end;

var
  Root: THandler;
  Visited, Handled: string;
  Amount: Integer;
begin
  Amount := 250;
  Root := THandler.Create('faq', 50,
    THandler.Create('billing', 500,
      THandler.Create('escalation', MaxInt, nil)));
  try
    Visited := '';
    Handled := Root.Handle(Amount, Visited);
    WriteLn(Format('visited=%s;handled=%s;result=refund(%d)',
      [Visited, Handled, Amount]));
  finally
    Root.Free;
  end;
end.
