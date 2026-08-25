with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Facade is
   function Auth_Service (User : String) return String is
   begin
      return "auth(" & User & ")";
   end Auth_Service;

   function Inventory_Service (Sku : String) return String is
   begin
      return "reserve(" & Sku & ")";
   end Inventory_Service;

   function Billing_Service (Amount : Integer) return String is
   begin
      return "charge(" & Trim (Integer'Image (Amount), Both) & ")";
   end Billing_Service;

   function Checkout_Facade
     (User : String; Sku : String; Amount : Integer) return String is
   begin
      return Auth_Service (User) & ">" & Inventory_Service (Sku) & ">" &
        Billing_Service (Amount);
   end Checkout_Facade;
begin
   Put_Line ("checkout=" & Checkout_Facade ("alice", "SKU-42", 499));
end Facade;
