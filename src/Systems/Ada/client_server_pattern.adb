function Client_Server_Pattern return Boolean is
   function Server (Key : String) return String is
   begin
      if Key = "sku-1" then
         return "200:stock=7";
      end if;
      return "404:missing";
   end Server;
begin
   return Server ("sku-1") = "200:stock=7";
end Client_Server_Pattern;
