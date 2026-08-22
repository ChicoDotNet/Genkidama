with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Builder is
   type Report_Format is (Text_Report, Html_Report);

   type Report_Builder is record
      Format : Report_Format;
      Parts  : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;

   procedure Reset (Target : in out Report_Builder) is
   begin
      Target.Parts := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Reset;

   procedure Add_Part
     (Target : in out Report_Builder;
      Value  : String) is
   begin
      if Ada.Strings.Unbounded.Length (Target.Parts) > 0 then
         Ada.Strings.Unbounded.Append
           (Target.Parts, Ada.Characters.Latin_1.LF);
      end if;
      Ada.Strings.Unbounded.Append (Target.Parts, Value);
   end Add_Part;

   procedure Add_Title
     (Target : in out Report_Builder;
      Title  : String) is
   begin
      case Target.Format is
         when Text_Report =>
            Add_Part (Target, "# " & Title);
         when Html_Report =>
            Add_Part (Target, "<h1>" & Title & "</h1>");
      end case;
   end Add_Title;

   procedure Add_Section
     (Target  : in out Report_Builder;
      Heading : String;
      Content : String) is
   begin
      case Target.Format is
         when Text_Report =>
            Add_Part (Target, "## " & Heading);
            Add_Part (Target, Content);
         when Html_Report =>
            Add_Part
              (Target, "<h2>" & Heading & "</h2><p>" & Content & "</p>");
      end case;
   end Add_Section;

   function Build (Target : Report_Builder) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Target.Parts);
   end Build;

   function Build_Availability_Report
     (Target : in out Report_Builder) return String is
   begin
      Reset (Target);
      Add_Title (Target, "Service status");
      Add_Section (Target, "Availability", "99.95%");
      return Build (Target);
   end Build_Availability_Report;

   Text : Report_Builder := (Format => Text_Report, others => <>);
   Html : Report_Builder := (Format => Html_Report, others => <>);
begin
   Put_Line (Build_Availability_Report (Text));
   Put_Line ("---");
   Put_Line (Build_Availability_Report (Html));
end Builder;
