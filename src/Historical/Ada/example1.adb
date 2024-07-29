with Ada.Text_IO; use Ada.Text_IO;

procedure Example1 is
   type Button is tagged abstract null record;
   type Checkbox is tagged abstract null record;

   procedure Render (B : in out Button) is abstract;
   procedure Render (C : in out Checkbox) is abstract;

   type Dark_Button is new Button with null record;
   type Light_Button is new Button with null record;

   type Dark_Checkbox is new Checkbox with null record;
   type Light_Checkbox is new Checkbox with null record;

   procedure Render (B : in out Dark_Button) is
   begin
      Put_Line ("Dark Button");
   end Render;

   procedure Render (B : in out Light_Button) is
   begin
      Put_Line ("Light Button");
   end Render;

   procedure Render (C : in out Dark_Checkbox) is
   begin
      Put_Line ("Dark Checkbox");
   end Render;

   procedure Render (C : in out Light_Checkbox) is
   begin
      Put_Line ("Light Checkbox");
   end Render;

   type UIFactory is abstract tagged null record;
   type Dark_Factory is new UIFactory with null record;
   type Light_Factory is new UIFactory with null record;

   function Create_Button (F : Dark_Factory) return Dark_Button is
   begin
      return (Dark_Button with null record);
   end Create_Button;

   function Create_Button (F : Light_Factory) return Light_Button is
   begin
      return (Light_Button with null record);
   end Create_Button;

   function Create_Checkbox (F : Dark_Factory) return Dark_Checkbox is
   begin
      return (Dark_Checkbox with null record);
   end Create_Checkbox;

   function Create_Checkbox (F : Light_Factory) return Light_Checkbox is
   begin
      return (Light_Checkbox with null record);
   end Create_Checkbox;

   procedure Create_UI_Components (F : UIFactory) is
   begin
      if F in Dark_Factory'Class then
         declare
            B : Dark_Button := Create_Button (Dark_Factory (F));
            C : Dark_Checkbox := Create_Checkbox (Dark_Factory (F));
         begin
            Render (B);
            Render (C);
         end;
      elsif F in Light_Factory'Class then
         declare
            B : Light_Button := Create_Button (Light_Factory (F));
            C : Light_Checkbox := Create_Checkbox (Light_Factory (F));
         begin
            Render (B);
            Render (C);
         end;
      end if;
   end Create_UI_Components;

begin
   declare
      Dark : Dark_Factory;
      Light : Light_Factory;
   begin
      Create_UI_Components (Dark);
      Create_UI_Components (Light);
   end;
end Example1;
