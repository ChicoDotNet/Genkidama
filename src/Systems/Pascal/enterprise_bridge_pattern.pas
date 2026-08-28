unit Enterprise_Bridge_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Send(const Transport,Kind,MessageText:String):String;begin Result:=Transport+'>'+Kind+':'+MessageText;end;
function Run:Boolean;begin Result:=(Send('kafka','ALERT','disk')='kafka>ALERT:disk')and(Send('queue','REMINDER','backup')='queue>REMINDER:backup');end;
end.
