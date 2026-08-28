function Enterprise_Facade_Pattern return Boolean is
   function CRM (Id : Integer) return String is
   begin
      return "crm:create:" & Integer'Image (Id);
   end CRM;

   function Billing (Id : Integer) return String is
   begin
      return "billing:open:" & Integer'Image (Id);
   end Billing;

   function Onboard (Id : Integer) return String is
   begin
      return CRM (Id) & ">" & Billing (Id);
   end Onboard;
begin
   return Onboard (77) = "crm:create: 77>billing:open: 77";
end Enterprise_Facade_Pattern;
