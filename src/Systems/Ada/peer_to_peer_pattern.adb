with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Peer_To_Peer_Pattern return Boolean is
   Inbox : Unbounded_String := Null_Unbounded_String;

   procedure Send_Block (From_Peer, To_Peer, Data : String) is
   begin
      if Length (Inbox) > 0 then
         Append (Inbox, ">");
      end if;
      Append (Inbox, From_Peer & ">" & To_Peer & ":" & Data);
   end Send_Block;
begin
   Send_Block ("peer-a", "peer-b", "block-42");
   Send_Block ("peer-a", "peer-c", "block-42");
   return To_String (Inbox) = "peer-a>peer-b:block-42>peer-a>peer-c:block-42";
end Peer_To_Peer_Pattern;
