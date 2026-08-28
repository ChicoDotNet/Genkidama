with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Active_Record_Pattern return Boolean is
   type Record_Model is record
      Id   : Integer;
      Name : Unbounded_String;
   end record;

   function Save (Item : Record_Model) return Record_Model is
   begin
      return Item;
   end Save;

   Saved : constant Record_Model := Save ((Id => 7, Name => To_Unbounded_String ("Ada")));
begin
   return Saved.Id = 7 and then To_String (Saved.Name) = "Ada";
end Active_Record_Pattern;
