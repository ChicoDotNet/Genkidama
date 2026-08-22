program CompositeExample;

{$mode objfpc}{$H+}

type
  TComponent = class
    function Size: Integer; virtual; abstract;
  end;

  TFileLeaf = class(TComponent)
  private
    FBytes: Integer;
  public
    constructor Create(ABytes: Integer);
    function Size: Integer; override;
  end;

  TFolderComposite = class(TComponent)
  private
    FChildren: array of TComponent;
  public
    destructor Destroy; override;
    procedure Add(AChild: TComponent);
    function Size: Integer; override;
  end;

constructor TFileLeaf.Create(ABytes: Integer);
begin
  inherited Create;
  FBytes := ABytes;
end;

function TFileLeaf.Size: Integer;
begin
  Result := FBytes;
end;

destructor TFolderComposite.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FChildren) do
    FChildren[I].Free;
  inherited Destroy;
end;

procedure TFolderComposite.Add(AChild: TComponent);
var
  Index: Integer;
begin
  Index := Length(FChildren);
  SetLength(FChildren, Index + 1);
  FChildren[Index] := AChild;
end;

function TFolderComposite.Size: Integer;
var
  Child: TComponent;
begin
  Result := 0;
  for Child in FChildren do
    Inc(Result, Child.Size);
end;

var
  Readme: TComponent;
  Docs: TFolderComposite;
  Root: TFolderComposite;
begin
  Readme := TFileLeaf.Create(2);
  Docs := TFolderComposite.Create;
  Docs.Add(TFileLeaf.Create(3));
  Docs.Add(TFileLeaf.Create(5));

  Root := TFolderComposite.Create;
  Root.Add(Readme);
  Root.Add(Docs);

  WriteLn('leaf=', Readme.Size);
  WriteLn('docs=', Docs.Size);
  WriteLn('root=', Root.Size);

  Root.Free;
end.
