with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Builder is
   type Report_Format is (Text_Report, Html_Report);

   type Report_Builder is record
      Format : Report_Format;
      Parts  : Unbounded_String := Null_Unbounded_String;
   end record;

   procedure Reset (Target : in out Report_Builder) is
   begin
      Target.Parts := Null_Unbounded_String;
   end Reset;

   procedure Append (Target : in out Report_Builder; Value : String) is
   begin
      if Length (Target.Parts) > 0 then
         Append (Target.Parts, ASCII.LF);
      end if;
      Append (Target.Parts, Value);
   end Append;

   procedure Add_Title
     (Target : in out Report_Builder;
      Title  : String) is
   begin
      case Target.Format is
         when Text_Report =>
            Append (Target, "# " & Title);
         when Html_Report =>
            Append (Target, "<h1>" & Title & "</h1>");
      end case;
   end Add_Title;

   procedure Add_Section
     (Target  : in out Report_Builder;
      Heading : String;
      Body    : String) is
   begin
      case Target.Format is
         when Text_Report =>
            Append (Target, "## " & Heading);
            Append (Target, Body);
         when Html_Report =>
            Append (Target, "<h2>" & Heading & "</h2><p>" & Body & "</p>");
      end case;
   end Add_Section;

   function Build (Target : Report_Builder) return String is
   begin
      return To_String (Target.Parts);
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
