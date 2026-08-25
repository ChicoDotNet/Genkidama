program FacadeExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TAuthService = class
    function Authenticate(const UserName: string): string;
  end;

  TInventoryService = class
    function Reserve(const Sku: string): string;
  end;

  TBillingService = class
    function Charge(Cents: Integer): string;
  end;

  TCheckoutFacade = class
  private
    FAuth: TAuthService;
    FInventory: TInventoryService;
    FBilling: TBillingService;
  public
    constructor Create(AAuth: TAuthService; AInventory: TInventoryService; ABilling: TBillingService);
    function Checkout(const UserName, Sku: string; Cents: Integer): string;
  end;

function TAuthService.Authenticate(const UserName: string): string;
begin
  Result := 'auth(' + UserName + ')';
end;

function TInventoryService.Reserve(const Sku: string): string;
begin
  Result := 'reserve(' + Sku + ')';
end;

function TBillingService.Charge(Cents: Integer): string;
begin
  Result := 'charge(' + IntToStr(Cents) + ')';
end;

constructor TCheckoutFacade.Create(AAuth: TAuthService; AInventory: TInventoryService; ABilling: TBillingService);
begin
  FAuth := AAuth;
  FInventory := AInventory;
  FBilling := ABilling;
end;

function TCheckoutFacade.Checkout(const UserName, Sku: string; Cents: Integer): string;
begin
  Result := FAuth.Authenticate(UserName) + '>' + FInventory.Reserve(Sku) + '>' + FBilling.Charge(Cents);
end;

var
  Auth: TAuthService;
  Inventory: TInventoryService;
  Billing: TBillingService;
  Facade: TCheckoutFacade;
begin
  Auth := TAuthService.Create;
  Inventory := TInventoryService.Create;
  Billing := TBillingService.Create;
  Facade := TCheckoutFacade.Create(Auth, Inventory, Billing);
  try
    Writeln('checkout=' + Facade.Checkout('alice', 'SKU-42', 499));
  finally
    Facade.Free;
    Billing.Free;
    Inventory.Free;
    Auth.Free;
  end;
end.
