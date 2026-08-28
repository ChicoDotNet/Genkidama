unit Document_View_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
uses SysUtils;
type TDocument=record Title:String;Words:Integer;end;
function Editor(const Doc:TDocument):String;begin Result:='editor:'+Doc.Title+':'+IntToStr(Doc.Words);end;
function Summary(const Doc:TDocument):String;begin Result:='summary:'+Doc.Title;end;
function Run:Boolean;var D:TDocument;begin D.Title:='Final';D.Words:=120;Result:=(Editor(D)='editor:Final:120')and(Summary(D)='summary:Final');end;
end.
