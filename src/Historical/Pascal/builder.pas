program BuilderExample;

{$mode objfpc}{$H+}

uses SysUtils;

type
  TBuilderKind = (bkText, bkHtml);
  TReportBuilder = record
    Kind: TBuilderKind;
    Parts: array of string;
  end;

procedure Reset(var B: TReportBuilder);
begin
  SetLength(B.Parts, 0);
end;

procedure AddPart(var B: TReportBuilder; const Value: string);
var
  N: Integer;
begin
  N := Length(B.Parts);
  SetLength(B.Parts, N + 1);
  B.Parts[N] := Value;
end;

procedure AddTitle(var B: TReportBuilder; const Title: string);
begin
  if B.Kind = bkText then
    AddPart(B, '# ' + Title)
  else
    AddPart(B, '<h1>' + Title + '</h1>');
end;

procedure AddSection(var B: TReportBuilder; const Heading, Body: string);
begin
  if B.Kind = bkText then
  begin
    AddPart(B, '## ' + Heading);
    AddPart(B, Body);
  end
  else
  begin
    AddPart(B, '<h2>' + Heading + '</h2>');
    AddPart(B, '<p>' + Body + '</p>');
  end;
end;

function Build(const B: TReportBuilder): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(B.Parts) do
  begin
    if (B.Kind = bkText) and (I > 0) then
      Result := Result + LineEnding;
    Result := Result + B.Parts[I];
  end;
end;

function BuildAvailabilityReport(var B: TReportBuilder): string;
begin
  Reset(B);
  AddTitle(B, 'Service status');
  AddSection(B, 'Availability', '99.95%');
  Result := Build(B);
end;

var
  TextBuilder, HtmlBuilder: TReportBuilder;
begin
  TextBuilder.Kind := bkText;
  HtmlBuilder.Kind := bkHtml;
  WriteLn(BuildAvailabilityReport(TextBuilder));
  WriteLn('---');
  WriteLn(BuildAvailabilityReport(HtmlBuilder));
end.
