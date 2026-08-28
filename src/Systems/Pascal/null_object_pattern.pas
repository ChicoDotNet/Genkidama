unit Null_Object_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TLogger=class abstract public function Log(const MessageText:String):String;virtual;abstract;end;
     TNullLogger=class(TLogger)public function Log(const MessageText:String):String;override;end;
     TRealLogger=class(TLogger)public function Log(const MessageText:String):String;override;end;
function TNullLogger.Log(const MessageText:String):String;begin Result:='';end;
function TRealLogger.Log(const MessageText:String):String;begin Result:='log:'+MessageText;end;
function Run:Boolean;var N,R:TLogger;begin N:=TNullLogger.Create;R:=TRealLogger.Create;try Result:=(N.Log('x')='')and(R.Log('x')='log:x');finally N.Free;R.Free;end;end;
end.
