function Object_Pool_Pattern return Boolean is
   type Int_Array is array (Positive range <>) of Integer;
   Pool        : Int_Array (1 .. 2) := [1, 2];
   Borrowed    : constant Integer := Pool (2);
   Expected    : constant Int_Array (1 .. 2) := [1, 2];
   Checked_Out : Boolean;
begin
   Pool (2) := 0;
   Checked_Out := Pool (2) = 0;
   Pool (2) := Borrowed;
   return Checked_Out and then Pool = Expected;
end Object_Pool_Pattern;
