unit Data_Mapper_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TPerson=record Id:Integer;Name:String;end;TRow=record Key:Integer;Name:String;end;
function ToRow(const P:TPerson):TRow;begin Result.Key:=1000+P.Id;Result.Name:=P.Name;end;
function FromRow(const R:TRow):TPerson;begin Result.Id:=R.Key-1000;Result.Name:=R.Name;end;
function Run:Boolean;var P,Q:TPerson;R:TRow;begin P.Id:=8;P.Name:='Grace';R:=ToRow(P);Q:=FromRow(R);Result:=(R.Key=1008)and(Q.Id=P.Id)and(Q.Name=P.Name);end;
end.
