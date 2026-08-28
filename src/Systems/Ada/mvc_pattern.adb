function MVC_Pattern return Boolean is
   Count : Integer := 0;

   function View return String is
   begin
      return "count=" & Integer'Image (Count);
   end View;

   Before : constant String := View;
begin
   Count := Count + 1;
   return Before = "count= 0" and then View = "count= 1";
end MVC_Pattern;
