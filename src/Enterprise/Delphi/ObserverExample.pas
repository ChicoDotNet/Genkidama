program ObserverExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  IObserver = interface
    ['{E77BBD65-6143-47A8-9E44-9E3A657C7782}']
    procedure Update(const AState: string);
  end;

  TSubject = class
  private
    FObservers: TList<IObserver>;
  public
    constructor Create;
    destructor Destroy; override;
    function Subscribe(const AObserver: IObserver): Boolean;
    function Unsubscribe(const AObserver: IObserver): Boolean;
    procedure Publish(const AState: string);
  end;

  TCountingObserver = class(TInterfacedObject, IObserver)
  private
    FName: string;
    FCount: Integer;
    FLastState: string;
  public
    constructor Create(const AName: string);
    procedure Update(const AState: string);
    property Count: Integer read FCount;
    property LastState: string read FLastState;
  end;

constructor TSubject.Create;
begin
  inherited Create;
  FObservers := TList<IObserver>.Create;
end;

destructor TSubject.Destroy;
begin
  FObservers.Free;
  inherited Destroy;
end;

function TSubject.Subscribe(const AObserver: IObserver): Boolean;
begin
  Result := not FObservers.Contains(AObserver);
  if Result then
    FObservers.Add(AObserver);
end;

function TSubject.Unsubscribe(const AObserver: IObserver): Boolean;
begin
  Result := FObservers.Remove(AObserver) >= 0;
end;

procedure TSubject.Publish(const AState: string);
var
  Observer: IObserver;
begin
  for Observer in FObservers do
    Observer.Update(AState);
end;

constructor TCountingObserver.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TCountingObserver.Update(const AState: string);
begin
  Inc(FCount);
  FLastState := AState;
  Writeln(FName, ':', AState);
end;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

var
  Subject: TSubject;
  AuditObject: TCountingObserver;
  DashboardObject: TCountingObserver;
  Audit: IObserver;
  Dashboard: IObserver;
begin
  Subject := TSubject.Create;
  try
    AuditObject := TCountingObserver.Create('audit');
    DashboardObject := TCountingObserver.Create('dashboard');
    Audit := AuditObject;
    Dashboard := DashboardObject;

    Check(Subject.Subscribe(Audit), 'audit subscription must succeed');
    Check(Subject.Subscribe(Dashboard), 'dashboard subscription must succeed');
    Check(not Subject.Subscribe(Audit), 'duplicate subscription must be rejected');

    Subject.Publish('draft');
    Check((AuditObject.Count = 1) and (AuditObject.LastState = 'draft'), 'audit must receive draft');
    Check((DashboardObject.Count = 1) and (DashboardObject.LastState = 'draft'), 'dashboard must receive draft');

    Check(Subject.Unsubscribe(Dashboard), 'dashboard unsubscribe must succeed');
    Check(not Subject.Unsubscribe(Dashboard), 'second dashboard unsubscribe must be rejected');

    Subject.Publish('published');
    Check((AuditObject.Count = 2) and (AuditObject.LastState = 'published'), 'audit must receive published');
    Check((DashboardObject.Count = 1) and (DashboardObject.LastState = 'draft'), 'dashboard must stay unsubscribed');

    Writeln('OBSERVER_DELPHI_OK');
  finally
    Audit := nil;
    Dashboard := nil;
    Subject.Free;
  end;
end.
