program MediatorExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TCheckoutMediator = class
  public
    function Route(const SenderName, TargetName, MessageText: string): string;
  end;

  TPaymentColleague = class
  private
    FMediator: TCheckoutMediator;
  public
    constructor Create(AMediator: TCheckoutMediator);
    function Send(const MessageText: string): string;
  end;

  TInventoryColleague = class
  private
    FMediator: TCheckoutMediator;
  public
    constructor Create(AMediator: TCheckoutMediator);
    function Send(const MessageText: string): string;
  end;

function TCheckoutMediator.Route(const SenderName, TargetName, MessageText: string): string;
begin
  if TargetName = 'inventory' then
    Result := 'inventory<-' + SenderName + ':' + MessageText
  else if TargetName = 'payment' then
    Result := 'payment<-' + SenderName + ':' + MessageText
  else
    raise Exception.Create('UnknownColleague:' + TargetName);
end;

constructor TPaymentColleague.Create(AMediator: TCheckoutMediator);
begin
  inherited Create;
  FMediator := AMediator;
end;

function TPaymentColleague.Send(const MessageText: string): string;
begin
  Result := FMediator.Route('payment', 'inventory', MessageText);
end;

constructor TInventoryColleague.Create(AMediator: TCheckoutMediator);
begin
  inherited Create;
  FMediator := AMediator;
end;

function TInventoryColleague.Send(const MessageText: string): string;
begin
  Result := FMediator.Route('inventory', 'payment', MessageText);
end;

procedure VerifyMediator;
var
  Mediator: TCheckoutMediator;
  Payment: TPaymentColleague;
  Inventory: TInventoryColleague;
  FailedAsExpected: Boolean;
begin
  Mediator := TCheckoutMediator.Create;
  try
    Payment := TPaymentColleague.Create(Mediator);
    Inventory := TInventoryColleague.Create(Mediator);
    try
      if Payment.Send('reserve') <> 'inventory<-payment:reserve' then
        raise Exception.Create('payment routing failed');
      if Inventory.Send('reserved') <> 'payment<-inventory:reserved' then
        raise Exception.Create('inventory routing failed');

      FailedAsExpected := False;
      try
        Mediator.Route('payment', 'shipping', 'probe');
      except
        on E: Exception do
        begin
          FailedAsExpected := E.Message = 'UnknownColleague:shipping';
          if not FailedAsExpected then
            raise;
        end;
      end;
      if not FailedAsExpected then
        raise Exception.Create('Expected UnknownColleague failure');

      Writeln('Delphi Mediator: passed');
    finally
      Inventory.Free;
      Payment.Free;
    end;
  finally
    Mediator.Free;
  end;
end;

begin
  VerifyMediator;
end.
