program FlyweightExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TTextStyle = class
  public
    Font: string;
    FontSize: Integer;
    Color: string;
    constructor Create(const AFont: string; AFontSize: Integer; const AColor: string);
  end;

  TStyleFactory = class
  private
    FPool: TObjectDictionary<string, TTextStyle>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetStyle(const Font: string; FontSize: Integer; const Color: string): TTextStyle;
    function Count: Integer;
  end;

constructor TTextStyle.Create(const AFont: string; AFontSize: Integer; const AColor: string);
begin
  Font := AFont;
  FontSize := AFontSize;
  Color := AColor;
end;

constructor TStyleFactory.Create;
begin
  inherited Create;
  FPool := TObjectDictionary<string, TTextStyle>.Create([doOwnsValues]);
end;

destructor TStyleFactory.Destroy;
begin
  FPool.Free;
  inherited;
end;

function TStyleFactory.GetStyle(const Font: string; FontSize: Integer; const Color: string): TTextStyle;
var
  Key: string;
begin
  Key := Font + '|' + IntToStr(FontSize) + '|' + Color;
  if not FPool.TryGetValue(Key, Result) then
  begin
    Result := TTextStyle.Create(Font, FontSize, Color);
    FPool.Add(Key, Result);
  end;
end;

function TStyleFactory.Count: Integer;
begin
  Result := FPool.Count;
end;

var
  Factory: TStyleFactory;
  Red1, Red2, Blue: TTextStyle;
begin
  Factory := TStyleFactory.Create;
  try
    Red1 := Factory.GetStyle('Inter', 12, 'red');
    Red2 := Factory.GetStyle('Inter', 12, 'red');
    Blue := Factory.GetStyle('Inter', 12, 'blue');
    Assert(Blue <> Red1);
    Writeln(Format('styles=%d;shared=%s;text=ABC',
      [Factory.Count, LowerCase(BoolToStr(Red1 = Red2, True))]));
  finally
    Factory.Free;
  end;
end.
