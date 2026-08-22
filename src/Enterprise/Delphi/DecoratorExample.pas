program DecoratorExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

type
  IComponent = interface
    ['{7A4C1B52-6A79-4A2B-8B29-0E49BA7C8F61}']
    function Render: string;
  end;

  TPlainMessage = class(TInterfacedObject, IComponent)
    function Render: string;
  end;

  TComponentDecorator = class(TInterfacedObject, IComponent)
  protected
    FInner: IComponent;
  public
    constructor Create(const AInner: IComponent);
    function Render: string; virtual;
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

constructor TComponentDecorator.Create(const AInner: IComponent);
begin
  inherited Create;
  FInner := AInner;
end;

function TComponentDecorator.Render: string;
begin
  Result := FInner.Render;
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
  Base, Audited, Encrypted, Stacked: IComponent;
begin
  Base := TPlainMessage.Create;
  Audited := TAuditDecorator.Create(Base);
  Encrypted := TEncryptDecorator.Create(Base);
  Stacked := TAuditDecorator.Create(TEncryptDecorator.Create(Base));

  Writeln('base=' + Base.Render);
  Writeln('audit=' + Audited.Render);
  Writeln('encrypted=' + Encrypted.Render);
  Writeln('stacked=' + Stacked.Render);
end.
