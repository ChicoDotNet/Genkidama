function Broker_Pattern return Boolean is
   type Text_Service is access function (Value : String) return String;
   function Inventory_Service (Value : String) return String is ("inventory:" & Value & "=7");
   function Customer_Service (Value : String) return String is ("customer:" & Value & "=active");
   Inventory : constant Text_Service := Inventory_Service'Access;
   Customer  : constant Text_Service := Customer_Service'Access;
begin
   return Inventory ("sku-1") = "inventory:sku-1=7"
     and then Customer ("17") = "customer:17=active";
end Broker_Pattern;
