program InterpreterExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  IExpression = interface
    ['{5C3FD1F8-6A6D-4E10-8A59-4A9CEB67F203}']
    function Interpret: Integer;
  end;

  TNumberExpression = class(TInterfacedObject, IExpression)
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);
    function Interpret: Integer;
  end;

  TAddExpression = class(TInterfacedObject, IExpression)
  private
    FLeft: IExpression;
    FRight: IExpression;
  public
    constructor Create(const ALeft, ARight: IExpression);
    function Interpret: Integer;
  end;

constructor TNumberExpression.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

function TNumberExpression.Interpret: Integer;
begin
  Result := FValue;
end;

constructor TAddExpression.Create(const ALeft, ARight: IExpression);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
end;

function TAddExpression.Interpret: Integer;
begin
  Result := FLeft.Interpret + FRight.Interpret;
end;

var
  Expression: IExpression;
  Value: Integer;
begin
  Expression := TAddExpression.Create(
    TAddExpression.Create(TNumberExpression.Create(2), TNumberExpression.Create(3)),
    TNumberExpression.Create(4));
  Value := Expression.Interpret;
  if Value <> 9 then
    raise Exception.Create('Unexpected interpreter result');
  Writeln('interpreter=', Value);
end.
