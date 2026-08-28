unit Service_Locator_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TService=function(const Value:String):String; TServiceKind=(skEmail,skAudit);
function Email(const Value:String):String;begin Result:='email>'+Value;end;
function Audit(const Value:String):String;begin Result:='audit>'+Value;end;
function Locate(Kind:TServiceKind):TService;begin if Kind=skEmail then Result:=@Email else Result:=@Audit;end;
function Run:Boolean;var S:TService;begin S:=Locate(skEmail);Result:=S('a@example.test')='email>a@example.test';S:=Locate(skAudit);Result:=Result and(S('created')='audit>created');end;
end.
