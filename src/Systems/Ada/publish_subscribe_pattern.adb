with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Publish_Subscribe_Pattern return Boolean is
   Results : Unbounded_String := Null_Unbounded_String;

   procedure Publish (Id : Integer) is
   begin
      Append (Results, "warehouse:" & Integer'Image (Id));
      Append (Results, ">analytics:" & Integer'Image (Id));
   end Publish;
begin
   Publish (51);
   return To_String (Results) = "warehouse: 51>analytics: 51";
end Publish_Subscribe_Pattern;
