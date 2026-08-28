function Distributed_Proxy_Pattern return Boolean is
   function Remote (SKU : String) return Integer is
   begin
      if SKU = "sku-1" then
         return 7;
      end if;
      return 0;
   end Remote;

   function Proxy (SKU : String) return Integer is
   begin
      return Remote (SKU);
   end Proxy;
begin
   return Proxy ("sku-1") = 7 and then Proxy ("missing") = 0;
end Distributed_Proxy_Pattern;
