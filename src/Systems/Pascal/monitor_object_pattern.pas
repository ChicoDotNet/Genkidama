unit Monitor_Object_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TMonitorCounter=class
private FValue:Integer;FGuard:TRTLCriticalSection;
public constructor Create;destructor Destroy;override;procedure Add(Amount:Integer);function Value:Integer;end;
constructor TMonitorCounter.Create;begin inherited Create;InitCriticalSection(FGuard);end;
destructor TMonitorCounter.Destroy;begin DoneCriticalSection(FGuard);inherited Destroy;end;
procedure TMonitorCounter.Add(Amount:Integer);begin EnterCriticalSection(FGuard);try Inc(FValue,Amount);finally LeaveCriticalSection(FGuard);end;end;
function TMonitorCounter.Value:Integer;begin EnterCriticalSection(FGuard);try Result:=FValue;finally LeaveCriticalSection(FGuard);end;end;
function Run:Boolean;var C:TMonitorCounter;begin C:=TMonitorCounter.Create;try C.Add(2);C.Add(3);Result:=C.Value=5;finally C.Free;end;end;
end.
