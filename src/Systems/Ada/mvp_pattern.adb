with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function MVP_Pattern return Boolean is
   Count : Integer := 0;
   Text  : Unbounded_String := Null_Unbounded_String;

   procedure Present is
   begin
      Count := Count + 1;
      Text := To_Unbounded_String ("count=" & Integer'Image (Count));
   end Present;
begin
   Present;
   return Count = 1 and then To_String (Text) = "count= 1";
end MVP_Pattern;
