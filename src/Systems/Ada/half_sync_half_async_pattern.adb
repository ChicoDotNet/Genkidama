with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Half_Sync_Half_Async_Pattern return Boolean is
   type Job_Array is array (Positive range <>) of Unbounded_String;
   Jobs : constant Job_Array :=
     [To_Unbounded_String ("job-1"),
      To_Unbounded_String ("job-2"),
      To_Unbounded_String ("job-3")];
   Results : Unbounded_String := Null_Unbounded_String;
begin
   for Index in Jobs'Range loop
      if Length (Results) > 0 then
         Append (Results, ">");
      end if;
      Append (Results, "done:" & To_String (Jobs (Index)));
   end loop;
   return To_String (Results) = "done:job-1>done:job-2>done:job-3";
end Half_Sync_Half_Async_Pattern;
