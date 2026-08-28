unit Active_Record_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TRecordModel=record Id:Integer;Name:String;end;TRow=record Key:Integer;Name:String;end;
function Save(const Item:TRecordModel):TRow;begin Result.Key:=Item.Id;Result.Name:=Item.Name;end;
function Run:Boolean;var M:TRecordModel;R:TRow;begin M.Id:=7;M.Name:='Ada';R:=Save(M);Result:=(R.Key=7)and(R.Name='Ada');end;
end.
