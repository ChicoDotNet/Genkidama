unit Peer_To_Peer_Pattern;
{$mode objfpc}{$H+}
interface
function Run:Boolean;
implementation
type TMessage=record FromPeer,ToPeer,BlockId:Integer;end;
function Send(FromPeer,ToPeer,BlockId:Integer):TMessage;begin Result.FromPeer:=FromPeer;Result.ToPeer:=ToPeer;Result.BlockId:=BlockId;end;
function Run:Boolean;var A,B:TMessage;begin A:=Send(1,2,42);B:=Send(1,3,42);Result:=(A.ToPeer=2)and(B.ToPeer=3)and(A.BlockId=B.BlockId);end;
end.
