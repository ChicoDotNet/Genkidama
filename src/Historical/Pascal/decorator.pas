program DecoratorExample;

{$mode objfpc}{$H+}

uses SysUtils;

type
  TComponent = class
    function Render: string; virtual; abstract;
  end;

  TPlainMessage = class(TComponent)
    function Render: string; override;
  end;

  TComponentDecorator = class(TComponent)
  protected
    FInner: TComponent;
  public
    constructor Create(AInner: TComponent);
  end;

  TAuditDecorator = class(TComponentDecorator)
    function Render: string; override;
  end;

  TEncryptDecorator = class(TComponentDecorator)
    function Render: string; override;
  end;

function TPlainMessage.Render: string;
begin
  Result := 'alert';
end;

constructor TComponentDecorator.Create(AInner: TComponent);
begin
  inherited Create;
  FInner := AInner;
end;

function TAuditDecorator.Render: string;
begin
  Result := 'audit(' + FInner.Render + ')';
end;

function TEncryptDecorator.Render: string;
begin
  Result := 'enc(' + FInner.Render + ')';
end;

var
  Plain, AuditOnly, EncryptOnly, InnerEncrypted, Stacked: TComponent;
begin
  Plain := TPlainMessage.Create;
  AuditOnly := TAuditDecorator.Create(Plain);
  EncryptOnly := TEncryptDecorator.Create(Plain);
  InnerEncrypted := TEncryptDecorator.Create(Plain);
  Stacked := TAuditDecorator.Create(InnerEncrypted);
  try
    Writeln('base=', Plain.Render);
    Writeln('audit=', AuditOnly.Render);
    Writeln('encrypted=', EncryptOnly.Render);
    Writeln('stacked=', Stacked.Render);
  finally
    Stacked.Free;
    InnerEncrypted.Free;
    EncryptOnly.Free;
    AuditOnly.Free;
    Plain.Free;
  end;
end.
