program Example3;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IReport = interface
    ['{97A0B7E2-FC7B-4B8B-8A22-FE973D67223B}']
    procedure Generate;
  end;

  TPDFReport = class(TInterfacedObject, IReport)
  public
    procedure Generate;
  end;

  THTMLReport = class(TInterfacedObject, IReport)
  public
    procedure Generate;
  end;

  IReportFactory = interface
    ['{4C46B72E-79E4-4E18-85E2-4A577167F3D5}']
    function CreateReport: IReport;
  end;

  TPDFReportFactory = class(TInterfacedObject, IReportFactory)
  public
    function CreateReport: IReport;
  end;

  THTMLReportFactory = class(TInterfacedObject, IReportFactory)
  public
    function CreateReport: IReport;
  end;

procedure TPDFReport.Generate;
begin
  Writeln('Generating PDF report');
end;

procedure THTMLReport.Generate;
begin
  Writeln('Generating HTML report');
end;

function TPDFReportFactory.CreateReport: IReport;
begin
  Result := TPDFReport.Create;
end;

function THTMLReportFactory.CreateReport: IReport;
begin
  Result := THTMLReport.Create;
end;

procedure UseFactory(factory: IReportFactory);
var
  report: IReport;
begin
  report := factory.CreateReport;
  report.Generate;
end;

begin
  try
    UseFactory(TPDFReportFactory.Create);
    UseFactory(THTMLReportFactory.Create);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
