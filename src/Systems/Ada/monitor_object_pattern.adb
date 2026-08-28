function Monitor_Object_Pattern return Boolean is
   protected Counter is
      procedure Add (Amount : Integer);
      function Value return Integer;
   private
      Current : Integer := 0;
   end Counter;

   protected body Counter is
      procedure Add (Amount : Integer) is
      begin
         Current := Current + Amount;
      end Add;

      function Value return Integer is
      begin
         return Current;
      end Value;
   end Counter;
begin
   Counter.Add (2);
   Counter.Add (3);
   return Counter.Value = 5;
end Monitor_Object_Pattern;
