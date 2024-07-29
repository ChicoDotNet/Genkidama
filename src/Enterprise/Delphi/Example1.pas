program Example1;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IButton = interface
    ['{8FDF6945-49BA-4BF3-9E1E-84E9B927B6B0}']
    procedure Render;
  end;

  TDarkButton = class(TInterfacedObject, IButton)
  public
    procedure Render;
  end;

  TLightButton = class(TInterfacedObject, IButton)
  public
    procedure Render;
  end;

  ICheckbox = interface
    ['{BB4F7E84-25A2-4C5B-94CF-2A2974DFBBF5}']
    procedure Render;
  end;

  TDarkCheckbox = class(TInterfacedObject, ICheckbox)
  public
    procedure Render;
  end;

  TLightCheckbox = class(TInterfacedObject, ICheckbox)
  public
    procedure Render;
  end;

  IUIFactory = interface
    ['{8D104BD7-8F4D-4F2D-8A75-7FBC8166F6CB}']
    function CreateButton: IButton;
    function CreateCheckbox: ICheckbox;
  end;

  TDarkFactory = class(TInterfacedObject, IUIFactory)
  public
    function CreateButton: IButton;
    function CreateCheckbox: ICheckbox;
  end;

  TLightFactory = class(TInterfacedObject, IUIFactory)
  public
    function CreateButton: IButton;
    function CreateCheckbox: ICheckbox;
  end;

procedure TDarkButton.Render;
begin
  Writeln('Dark Button');
end;

procedure TLightButton.Render;
begin
  Writeln('Light Button');
end;

procedure TDarkCheckbox.Render;
begin
  Writeln('Dark Checkbox');
end;

procedure TLightCheckbox.Render;
begin
  Writeln('Light Checkbox');
end;

function TDarkFactory.CreateButton: IButton;
begin
  Result := TDarkButton.Create;
end;

function TDarkFactory.CreateCheckbox: ICheckbox;
begin
  Result := TDarkCheckbox.Create;
end;

function TLightFactory.CreateButton: IButton;
begin
  Result := TLightButton.Create;
end;

function TLightFactory.CreateCheckbox: ICheckbox;
begin
  Result := TLightCheckbox.Create;
end;

procedure CreateUIComponents(factory: IUIFactory);
var
  button: IButton;
  checkbox: ICheckbox;
begin
  button := factory.CreateButton;
  checkbox := factory.CreateCheckbox;
  button.Render;
  checkbox.Render;
end;

begin
  try
    CreateUIComponents(TDarkFactory.Create);
    CreateUIComponents(TLightFactory.Create);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
