function Active_Object_Pattern return Boolean is
   Value : Integer := 0;
   procedure Add_Three is
   begin
      Value := Value + 3;
   end Add_Three;
   procedure Times_Four is
   begin
      Value := Value * 4;
   end Times_Four;
begin
   Add_Three;
   Times_Four;
   return Value = 12;
end Active_Object_Pattern;
