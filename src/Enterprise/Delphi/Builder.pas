program Builder;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  IReportBuilder = interface
    ['{CE195E78-C992-45B9-AEAF-A57ED9547EC6}']
    procedure Reset;
    procedure AddTitle(const ATitle: string);
    procedure AddSection(const AHeading, ABody: string);
    function Build: string;
  end;

  TTextReportBuilder = class(TInterfacedObject, IReportBuilder)
  protected
    FParts: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure AddTitle(const ATitle: string); virtual;
    procedure AddSection(const AHeading, ABody: string); virtual;
    function Build: string;
  end;

  THtmlReportBuilder = class(TTextReportBuilder)
  public
    procedure AddTitle(const ATitle: string); override;
    procedure AddSection(const AHeading, ABody: string); override;
  end;

constructor TTextReportBuilder.Create;
begin
  inherited;
  FParts := TStringList.Create;
end;

destructor TTextReportBuilder.Destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TTextReportBuilder.Reset;
begin
  FParts.Clear;
end;

procedure TTextReportBuilder.AddTitle(const ATitle: string);
begin
  FParts.Add('# ' + ATitle);
end;

procedure TTextReportBuilder.AddSection(const AHeading, ABody: string);
begin
  FParts.Add('## ' + AHeading);
  FParts.Add(ABody);
end;

function TTextReportBuilder.Build: string;
begin
  Result := TrimRight(FParts.Text);
end;

procedure THtmlReportBuilder.AddTitle(const ATitle: string);
begin
  FParts.Add('<h1>' + ATitle + '</h1>');
end;

procedure THtmlReportBuilder.AddSection(const AHeading, ABody: string);
begin
  FParts.Add('<h2>' + AHeading + '</h2><p>' + ABody + '</p>');
end;

function BuildAvailabilityReport(const ABuilder: IReportBuilder): string;
begin
  ABuilder.Reset;
  ABuilder.AddTitle('Service status');
  ABuilder.AddSection('Availability', '99.95%');
  Result := ABuilder.Build;
end;

begin
  Writeln(BuildAvailabilityReport(TTextReportBuilder.Create));
  Writeln('---');
  Writeln(BuildAvailabilityReport(THtmlReportBuilder.Create));
end.
