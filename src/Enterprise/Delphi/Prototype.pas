program PrototypeExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes;

type
  TServiceProfile = class
  private
    FName: string;
    FFeatures: TStringList;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    function Clone: TServiceProfile;
    procedure AddFeature(const Feature: string);
    function Describe: string;
    property Name: string read FName write FName;
  end;

constructor TServiceProfile.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FFeatures := TStringList.Create;
  FFeatures.Delimiter := ',';
  FFeatures.StrictDelimiter := True;
end;

destructor TServiceProfile.Destroy;
begin
  FFeatures.Free;
  inherited Destroy;
end;

function TServiceProfile.Clone: TServiceProfile;
begin
  Result := TServiceProfile.Create(FName);
  Result.FFeatures.Assign(FFeatures);
end;

procedure TServiceProfile.AddFeature(const Feature: string);
begin
  FFeatures.Add(Feature);
end;

function TServiceProfile.Describe: string;
begin
  Result := FName + ': ' + FFeatures.DelimitedText;
end;

var
  Original: TServiceProfile;
  Canary: TServiceProfile;
begin
  Original := TServiceProfile.Create('orders');
  try
    Original.AddFeature('metrics');
    Canary := Original.Clone;
    try
      Canary.Name := 'orders-canary';
      Canary.AddFeature('tracing');

      Writeln('original=' + Original.Describe);
      Writeln('clone=' + Canary.Describe);
    finally
      Canary.Free;
    end;
  finally
    Original.Free;
  end;
end.
