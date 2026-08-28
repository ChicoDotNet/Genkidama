function Document_View_Pattern return Boolean is
   type Document is record
      Title : Character;
      Words : Integer;
   end record;

   function Editor (Item : Document) return Integer is
   begin
      return Item.Words;
   end Editor;

   function Summary (Item : Document) return Character is
   begin
      return Item.Title;
   end Summary;

   Item : constant Document := (Title => 'F', Words => 120);
begin
   return Editor (Item) = 120 and then Summary (Item) = 'F';
end Document_View_Pattern;
