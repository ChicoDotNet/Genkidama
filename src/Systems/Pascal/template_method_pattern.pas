unit Template_Method_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TTransform=function:String;
function Normalize:String;begin Result:='normalize';end;
function Pipeline(const ReadStep:String; Transform:TTransform):String;begin Result:=ReadStep+'>'+Transform()+'>publish';end;
function Run:Boolean;begin Result:=Pipeline('read-csv',@Normalize)='read-csv>normalize>publish';end;
end.
