with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Repository_Pattern return Boolean is
   type Int_Array is array (Positive range <>) of Integer;
   type Name_Array is array (Positive range <>) of Unbounded_String;
   Ids   : constant Int_Array := [1, 2];
   Names : constant Name_Array :=
     [To_Unbounded_String ("Ada"), To_Unbounded_String ("Grace")];
   Found : Unbounded_String := Null_Unbounded_String;
begin
   for Index in Ids'Range loop
      if Ids (Index) = 2 then
         Found := Names (Index);
      end if;
   end loop;
   return To_String (Found) = "Grace";
end Repository_Pattern;
