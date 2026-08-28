with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Message_Bus_Pattern return Boolean is
   Events : Unbounded_String := Null_Unbounded_String;

   procedure Publish (Topic : String; Id : Integer) is
   begin
      Append (Events, "audit:" & Topic & ":" & Integer'Image (Id));
      Append (Events, ">billing:" & Topic & ":" & Integer'Image (Id));
   end Publish;
begin
   Publish ("order-created", 42);
   return To_String (Events) = "audit:order-created: 42>billing:order-created: 42";
end Message_Bus_Pattern;
