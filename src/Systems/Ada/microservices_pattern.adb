function Microservices_Pattern return Boolean is
   Stock : Integer := 7;

   function Reserve (Quantity : Integer) return Boolean is
   begin
      if Quantity > Stock then
         return False;
      end if;
      Stock := Stock - Quantity;
      return True;
   end Reserve;

   function Place (Quantity : Integer) return String is
   begin
      if Reserve (Quantity) then
         return "confirmed";
      end if;
      return "rejected";
   end Place;

   Result : constant String := Place (2);
begin
   return Result = "confirmed" and then Stock = 5;
end Microservices_Pattern;
