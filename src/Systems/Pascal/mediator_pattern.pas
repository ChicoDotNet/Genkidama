unit Mediator_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
function Notify(const Sender,EventName:String):String;
begin
  if (Sender='button')and(EventName='click') then Result:='panel.refresh'
  else if (Sender='panel')and(EventName='loaded') then Result:='button.enable'
  else Result:='';
end;
function Run:Boolean;
begin Result:=(Notify('button','click')='panel.refresh')and(Notify('panel','loaded')='button.enable'); end;
end.
