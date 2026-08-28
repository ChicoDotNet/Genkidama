function Lazy_Initialization_Pattern return Boolean is
   Builds : Integer := 0;
   Ready  : Boolean := False;

   procedure Ensure is
   begin
      if not Ready then
         Builds := Builds + 1;
         Ready := True;
      end if;
   end Ensure;
begin
   Ensure;
   Ensure;
   return Ready and then Builds = 1;
end Lazy_Initialization_Pattern;
