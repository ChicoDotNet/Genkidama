with Ada.Text_IO; use Ada.Text_IO;

procedure Chain_Of_Responsibility is
   function Trim_Image (Value : Positive) return String is
      Image : constant String := Positive'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Trim_Image;

   type Handler is record
      Name  : String (1 .. 10);
      Limit : Positive;
   end record;

   function Can_Handle (Current : Handler; Amount : Positive) return Boolean is
     (Amount <= Current.Limit);

   Faq        : constant Handler := (Name => "faq       ", Limit => 50);
   Billing    : constant Handler := (Name => "billing   ", Limit => 500);
   Escalation : constant Handler := (Name => "escalation", Limit => Positive'Last);
   Amount     : constant Positive := 250;
   Visits     : Natural := 1;
   Handled_By : Handler := Faq;
begin
   if Can_Handle (Faq, Amount) then
      Handled_By := Faq;
   else
      Visits := Visits + 1;
      if Can_Handle (Billing, Amount) then
         Handled_By := Billing;
      else
         Visits := Visits + 1;
         Handled_By := Escalation;
      end if;
   end if;

   if Visits = 2 and then Handled_By.Name = Billing.Name then
      Put_Line ("visited=faq>billing;handled=billing;result=refund(" & Trim_Image (Amount) & ")");
   else
      raise Program_Error with "unexpected handler traversal";
   end if;
end Chain_Of_Responsibility;
