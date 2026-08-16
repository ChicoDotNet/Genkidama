with Ada.Text_IO; use Ada.Text_IO;

procedure Builder is
   type Report_Format is (Text_Report, Html_Report);

   type Report_Builder is record
      Format : Report_Format;
   end record;

   procedure Reset (Target : in out Report_Builder) is
      pragma Unreferenced (Target);
   begin
      null;
   end Reset;

   procedure Add_Title (Target : Report_Builder; Title : String) is
   begin
      case Target.Format is
         when Text_Report => Put_Line ("# " & Title);
         when Html_Report => Put_Line ("<h1>" & Title & "</h1>");
      end case;
   end Add_Title;

   procedure Add_Section
     (Target  : Report_Builder;
      Heading : String;
      Body    : String) is
   begin
      case Target.Format is
         when Text_Report =>
            Put_Line ("## " & Heading);
            Put_Line (Body);
         when Html_Report =>
            Put_Line ("<h2>" & Heading & "</h2><p>" & Body & "</p>");
      end case;
   end Add_Section;

   procedure Build_Availability_Report (Target : in out Report_Builder) is
   begin
      Reset (Target);
      Add_Title (Target, "Service status");
      Add_Section (Target, "Availability", "99.95%");
   end Build_Availability_Report;

   Text : Report_Builder := (Format => Text_Report);
   Html : Report_Builder := (Format => Html_Report);
begin
   Build_Availability_Report (Text);
   Put_Line ("---");
   Build_Availability_Report (Html);
end Builder;
