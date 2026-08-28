function Strategy_Pattern return Boolean is
   type Int_Strategy is access function (Value : Integer) return Integer;
   function Identity (Value : Integer) return Integer is (Value);
   function Discount (Value : Integer) return Integer is (Value * 80 / 100);

   function Price (Value : Integer; Strategy : Int_Strategy) return Integer is
   begin
      return Strategy (Value);
   end Price;
begin
   return Price (100, Identity'Access) = 100
     and then Price (100, Discount'Access) = 80;
end Strategy_Pattern;
