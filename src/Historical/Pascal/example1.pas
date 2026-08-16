program AbstractFactoryExample;

{$mode objfpc}{$H+}

type
  TCreateProduct = function: string;

  TUIFactory = record
    CreateButton: TCreateProduct;
    CreateCheckbox: TCreateProduct;
  end;

function DarkButton: string;
begin
  Result := 'Dark Button';
end;

function DarkCheckbox: string;
begin
  Result := 'Dark Checkbox';
end;

function LightButton: string;
begin
  Result := 'Light Button';
end;

function LightCheckbox: string;
begin
  Result := 'Light Checkbox';
end;

function DarkFactory: TUIFactory;
begin
  Result.CreateButton := @DarkButton;
  Result.CreateCheckbox := @DarkCheckbox;
end;

function LightFactory: TUIFactory;
begin
  Result.CreateButton := @LightButton;
  Result.CreateCheckbox := @LightCheckbox;
end;

procedure RenderUI(const Factory: TUIFactory);
begin
  WriteLn(Factory.CreateButton());
  WriteLn(Factory.CreateCheckbox());
end;

begin
  RenderUI(DarkFactory());
  RenderUI(LightFactory());
end.
