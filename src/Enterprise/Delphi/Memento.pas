unit Memento;

interface

type
  TMementoSnapshot = record
    Title: string;
    Tags: string;
  end;

  TDocument = class
  private
    FTitle: string;
    FTags: string;
  public
    constructor Create(const ATitle, ATags: string);
    function SaveMemento: TMementoSnapshot;
    procedure RestoreMemento(const Snapshot: TMementoSnapshot);
    property Title: string read FTitle write FTitle;
    property Tags: string read FTags write FTags;
  end;

procedure VerifyMementoCanonical;

implementation

uses
  SysUtils;

constructor TDocument.Create(const ATitle, ATags: string);
begin
  inherited Create;
  FTitle := ATitle;
  FTags := ATags;
end;

function TDocument.SaveMemento: TMementoSnapshot;
begin
  Result.Title := FTitle;
  Result.Tags := FTags;
end;

procedure TDocument.RestoreMemento(const Snapshot: TMementoSnapshot);
begin
  FTitle := Snapshot.Title;
  FTags := Snapshot.Tags;
end;

procedure VerifyMementoCanonical;
var
  Originator: TDocument;
  CaretakerSnapshot: TMementoSnapshot;
begin
  Originator := TDocument.Create('draft', 'pattern');
  try
    CaretakerSnapshot := Originator.SaveMemento;

    Originator.Title := 'published';
    Originator.Tags := 'pattern,edited';
    if (CaretakerSnapshot.Title <> 'draft') or (CaretakerSnapshot.Tags <> 'pattern') then
      raise Exception.Create('snapshot changed after originator mutation');

    Originator.RestoreMemento(CaretakerSnapshot);
    if (Originator.Title <> 'draft') or (Originator.Tags <> 'pattern') then
      raise Exception.Create('restore failed');

    Originator.Title := 'restored-edit';
    Originator.Tags := 'restored';
    if (CaretakerSnapshot.Title <> 'draft') or (CaretakerSnapshot.Tags <> 'pattern') then
      raise Exception.Create('snapshot aliases restored originator');
  finally
    Originator.Free;
  end;
end;

end.
