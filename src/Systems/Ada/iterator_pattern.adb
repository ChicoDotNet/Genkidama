function Iterator_Pattern return Boolean is
   type Int_Array is array (Positive range <>) of Integer;
   Values : constant Int_Array := [10, 20, 30];
   Seen   : Int_Array (Values'Range) := [others => 0];
begin
   for Index in Values'Range loop
      Seen (Index) := Values (Index);
   end loop;
   return Seen = Values;
end Iterator_Pattern;
