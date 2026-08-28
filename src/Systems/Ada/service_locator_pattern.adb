function Service_Locator_Pattern return Boolean is
   type Text_Service is access function (Value : String) return String;
   function Email_Service (Value : String) return String is ("email>" & Value);
   function Audit_Service (Value : String) return String is ("audit>" & Value);
   Email : constant Text_Service := Email_Service'Access;
   Audit : constant Text_Service := Audit_Service'Access;
begin
   return Email ("a@example.test") = "email>a@example.test"
     and then Audit ("created") = "audit>created";
end Service_Locator_Pattern;
