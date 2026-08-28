function Dependency_Injection_Pattern return Boolean is
   type Clock_Access is access function return String;
   function Fixed_Clock return String is ("10:00");

   function Service (Clock : Clock_Access) return String is
   begin
      return "at:" & Clock.all;
   end Service;
begin
   return Service (Fixed_Clock'Access) = "at:10:00";
end Dependency_Injection_Pattern;
