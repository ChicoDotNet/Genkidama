function MVVM_Pattern return Boolean is
   Amount : Integer := 10;

   function Text return String is
   begin
      return "$" & Integer'Image (Amount) & ".00";
   end Text;

   Before : constant String := Text;
begin
   Amount := Amount + 5;
   return Before = "$ 10.00" and then Text = "$ 15.00";
end MVVM_Pattern;
