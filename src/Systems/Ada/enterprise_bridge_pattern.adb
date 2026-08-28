function Enterprise_Bridge_Pattern return Boolean is
   function Send (Transport, Kind, Message : String) return String is
   begin
      return Transport & ">" & Kind & ":" & Message;
   end Send;
begin
   return Send ("kafka", "ALERT", "disk") = "kafka>ALERT:disk"
     and then Send ("queue", "REMINDER", "backup") = "queue>REMINDER:backup";
end Enterprise_Bridge_Pattern;
