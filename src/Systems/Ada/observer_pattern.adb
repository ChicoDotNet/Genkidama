with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Observer_Pattern return Boolean is
   Events : Unbounded_String := Null_Unbounded_String;

   procedure Audit (Id : Integer) is
   begin
      Append (Events, "audit:" & Integer'Image (Id));
   end Audit;

   procedure Dashboard (Id : Integer) is
   begin
      Append (Events, ">dashboard:" & Integer'Image (Id));
   end Dashboard;
begin
   Audit (42);
   Dashboard (42);
   return To_String (Events) = "audit: 42>dashboard: 42";
end Observer_Pattern;
