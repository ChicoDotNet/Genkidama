function Unit_Of_Work_Pattern return Boolean is
   type Int_Array is array (Positive range <>) of Integer;
   Pending  : Int_Array (1 .. 2) := [2, 3];
   Store    : Int_Array (1 .. 2) := [0, 0];
   Empty    : constant Int_Array (1 .. 2) := [0, 0];
   Expected : constant Int_Array (1 .. 2) := [2, 3];
begin
   Store := Pending;
   Pending := Empty;
   return Store = Expected and then Pending = Empty;
end Unit_Of_Work_Pattern;
