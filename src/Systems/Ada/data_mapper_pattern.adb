with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Data_Mapper_Pattern return Boolean is
   type Person is record
      Id   : Integer;
      Name : Unbounded_String;
   end record;
   type Row is record
      Key  : Integer;
      Name : Unbounded_String;
   end record;

   function To_Row (Item : Person) return Row is
   begin
      return (Key => 1000 + Item.Id, Name => Item.Name);
   end To_Row;

   function From_Row (Item : Row) return Person is
   begin
      return (Id => Item.Key - 1000, Name => Item.Name);
   end From_Row;

   Original : constant Person := (Id => 8, Name => To_Unbounded_String ("Grace"));
   Mapped   : constant Row := To_Row (Original);
   Restored : constant Person := From_Row (Mapped);
begin
   return Mapped.Key = 1008
     and then Restored.Id = Original.Id
     and then Restored.Name = Original.Name;
end Data_Mapper_Pattern;
