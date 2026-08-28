function PAC_Pattern return Boolean is
   function View (Name : String; Value : Integer) return String is
   begin
      return Name & ":view=" & Integer'Image (Value);
   end View;
begin
   return View ("child", 42) = "child:view= 42"
     and then View ("root", 42) = "root:view= 42";
end PAC_Pattern;
