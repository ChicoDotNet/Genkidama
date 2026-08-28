function Microkernel_Pattern return Boolean is
   type Plugin is access function (Value : Integer) return Integer;
   type Plugin_Array is array (Positive range <>) of Plugin;
   function Double_Value (Value : Integer) return Integer is (Value * 2);
   function Square_Value (Value : Integer) return Integer is (Value * Value);
   Plugins : constant Plugin_Array := [Double_Value'Access, Square_Value'Access];
begin
   return Plugins (1) (4) = 8 and then Plugins (2) (4) = 16;
end Microkernel_Pattern;
