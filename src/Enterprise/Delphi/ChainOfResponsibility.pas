program ChainOfResponsibility;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IRefundHandler = interface
    ['{DBE4CD59-4DA1-4F3D-B831-66C64EEB6578}']
    function Handle(AAmount: Integer; var AVisited: string): string;
  end;

  TRefundHandler = class(TInterfacedObject, IRefundHandler)
  private
    FName: string;
    FLimit: Integer;
    FNext: IRefundHandler;
  public
    constructor Create(const AName: string; ALimit: Integer; const ANext: IRefundHandler = nil);
    function Handle(AAmount: Integer; var AVisited: string): string;
  end;

constructor TRefundHandler.Create(const AName: string; ALimit: Integer; const ANext: IRefundHandler);
begin
  inherited Create;
  FName := AName;
  FLimit := ALimit;
  FNext := ANext;
end;

function TRefundHandler.Handle(AAmount: Integer; var AVisited: string): string;
begin
  if AVisited = '' then
    AVisited := FName
  else
    AVisited := AVisited + '>' + FName;

  if AAmount <= FLimit then
    Exit(Format('handled=%s;result=refund(%d)', [FName, AAmount]));

  if Assigned(FNext) then
    Exit(FNext.Handle(AAmount, AVisited));

  Result := 'handled=none;result=rejected';
end;

var
  Escalation: IRefundHandler;
  Billing: IRefundHandler;
  Faq: IRefundHandler;
  Visited: string;
  ResultText: string;
begin
  Escalation := TRefundHandler.Create('escalation', MaxInt);
  Billing := TRefundHandler.Create('billing', 500, Escalation);
  Faq := TRefundHandler.Create('faq', 50, Billing);
  Visited := '';
  ResultText := Faq.Handle(250, Visited);
  Writeln('visited=' + Visited + ';' + ResultText);
end.
