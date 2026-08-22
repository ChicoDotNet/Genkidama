program CompositeExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  ICompositeComponent = interface
    ['{53DF354C-936B-43F6-93F7-38C266DAAF02}']
    function Size: Integer;
  end;

  TFileLeaf = class(TInterfacedObject, ICompositeComponent)
  private
    FBytes: Integer;
  public
    constructor Create(ABytes: Integer);
    function Size: Integer;
  end;

  TFolderComposite = class(TInterfacedObject, ICompositeComponent)
  private
    FChildren: TList<ICompositeComponent>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Child: ICompositeComponent);
    function Size: Integer;
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

constructor TFolderComposite.Create;
begin
  inherited Create;
  FChildren := TList<ICompositeComponent>.Create;
end;

destructor TFolderComposite.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

procedure TFolderComposite.Add(const Child: ICompositeComponent);
begin
  FChildren.Add(Child);
end;

function TFolderComposite.Size: Integer;
var
  Child: ICompositeComponent;
begin
  Result := 0;
  for Child in FChildren do
    Inc(Result, Child.Size);
end;

var
  Readme: ICompositeComponent;
  DocsObject: TFolderComposite;
  Docs: ICompositeComponent;
  RootObject: TFolderComposite;
  Root: ICompositeComponent;
begin
  Readme := TFileLeaf.Create(2);

  DocsObject := TFolderComposite.Create;
  DocsObject.Add(TFileLeaf.Create(3));
  DocsObject.Add(TFileLeaf.Create(5));
  Docs := DocsObject;

  RootObject := TFolderComposite.Create;
  RootObject.Add(Readme);
  RootObject.Add(Docs);
  Root := RootObject;

  Writeln('leaf=', Readme.Size);
  Writeln('docs=', Docs.Size);
  Writeln('root=', Root.Size);
end.
