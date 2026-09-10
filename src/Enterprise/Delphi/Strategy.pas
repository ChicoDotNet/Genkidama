program Strategy;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TPricingStrategy = class abstract
  public
    function Calculate(BasePrice: Integer): Integer; virtual; abstract;
  end;

  TRegularPricing = class(TPricingStrategy)
  public
    function Calculate(BasePrice: Integer): Integer; override;
  end;

  TVipPricing = class(TPricingStrategy)
  public
    function Calculate(BasePrice: Integer): Integer; override;
  end;

  TCampaignPricing = class(TPricingStrategy)
  public
    function Calculate(BasePrice: Integer): Integer; override;
  end;

  TPricingContext = class
  private
    FStrategy: TPricingStrategy;
  public
    constructor Create(AStrategy: TPricingStrategy);
    procedure SetStrategy(AStrategy: TPricingStrategy);
    function Price(BasePrice: Integer): Integer;
  end;

function TRegularPricing.Calculate(BasePrice: Integer): Integer;
begin
  Result := BasePrice;
end;

function TVipPricing.Calculate(BasePrice: Integer): Integer;
begin
  Result := (BasePrice * 80) div 100;
end;

function TCampaignPricing.Calculate(BasePrice: Integer): Integer;
begin
  if BasePrice >= 100 then
    Result := BasePrice - 25
  else
    Result := BasePrice;
end;

constructor TPricingContext.Create(AStrategy: TPricingStrategy);
begin
  inherited Create;
  FStrategy := AStrategy;
end;

procedure TPricingContext.SetStrategy(AStrategy: TPricingStrategy);
begin
  FStrategy := AStrategy;
end;

function TPricingContext.Price(BasePrice: Integer): Integer;
begin
  if not Assigned(FStrategy) then
    raise Exception.Create('pricing strategy is required');
  Result := FStrategy.Calculate(BasePrice);
end;

procedure RequireEqual(Actual, Expected: Integer; const MessageText: string);
begin
  if Actual <> Expected then
    raise Exception.CreateFmt('%s: expected %d, got %d', [MessageText, Expected, Actual]);
end;

var
  Context: TPricingContext;
  Regular: TRegularPricing;
  Vip: TVipPricing;
  Campaign: TCampaignPricing;
begin
  Regular := TRegularPricing.Create;
  Vip := TVipPricing.Create;
  Campaign := TCampaignPricing.Create;
  Context := TPricingContext.Create(Regular);
  try
    RequireEqual(Context.Price(100), 100, 'regular pricing');

    Context.SetStrategy(Vip);
    RequireEqual(Context.Price(100), 80, 'VIP pricing');

    Context.SetStrategy(Campaign);
    RequireEqual(Context.Price(100), 75, 'campaign pricing');
    RequireEqual(Context.Price(80), 80, 'campaign threshold boundary');

    Writeln('Delphi Strategy: passed');
  finally
    Context.Free;
    Campaign.Free;
    Vip.Free;
    Regular.Free;
  end;
end.
