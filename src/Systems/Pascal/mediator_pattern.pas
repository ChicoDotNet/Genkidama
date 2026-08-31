unit Mediator_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation

type
  TColleague = class
  private
    FLastMessage: String;
  public
    procedure Receive(const Sender, MessageText: String);
    property LastMessage: String read FLastMessage;
  end;

  TRoute = record
    Name: String;
    Colleague: TColleague;
  end;

  TCheckoutMediator = class
  private
    FRoutes: array[0..1] of TRoute;
    FCount: Integer;
  public
    procedure RegisterColleague(const Name: String; Colleague: TColleague);
    function Send(const Sender, Recipient, MessageText: String): Boolean;
  end;

procedure TColleague.Receive(const Sender, MessageText: String);
begin
  FLastMessage := Sender + ':' + MessageText;
end;

procedure TCheckoutMediator.RegisterColleague(const Name: String; Colleague: TColleague);
begin
  if FCount > High(FRoutes) then Exit;
  FRoutes[FCount].Name := Name;
  FRoutes[FCount].Colleague := Colleague;
  Inc(FCount);
end;

function TCheckoutMediator.Send(const Sender, Recipient, MessageText: String): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to FCount - 1 do
    if FRoutes[Index].Name = Recipient then
    begin
      FRoutes[Index].Colleague.Receive(Sender, MessageText);
      Exit(True);
    end;
  Result := False;
end;

function Run:Boolean;
var
  Mediator: TCheckoutMediator;
  Inventory, Payment: TColleague;
begin
  Mediator := TCheckoutMediator.Create;
  Inventory := TColleague.Create;
  Payment := TColleague.Create;
  try
    Mediator.RegisterColleague('inventory', Inventory);
    Mediator.RegisterColleague('payment', Payment);

    Result := Mediator.Send('payment', 'inventory', 'paid')
      and (Inventory.LastMessage = 'payment:paid')
      and Mediator.Send('inventory', 'payment', 'reserved')
      and (Payment.LastMessage = 'inventory:reserved')
      and (not Mediator.Send('payment', 'unknown', 'ignored'));
  finally
    Payment.Free;
    Inventory.Free;
    Mediator.Free;
  end;
end;

end.
