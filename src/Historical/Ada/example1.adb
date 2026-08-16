with Ada.Text_IO; use Ada.Text_IO;

procedure Example1 is
   type Button_Kind is (Dark_Button, Light_Button);
   type Checkbox_Kind is (Dark_Checkbox, Light_Checkbox);

   type Button_Creator is not null access function return Button_Kind;
   type Checkbox_Creator is not null access function return Checkbox_Kind;

   type UI_Factory is record
      Create_Button   : Button_Creator;
      Create_Checkbox : Checkbox_Creator;
   end record;

   function New_Dark_Button return Button_Kind is (Dark_Button);
   function New_Light_Button return Button_Kind is (Light_Button);
   function New_Dark_Checkbox return Checkbox_Kind is (Dark_Checkbox);
   function New_Light_Checkbox return Checkbox_Kind is (Light_Checkbox);

   Dark_Factory : constant UI_Factory :=
     (Create_Button   => New_Dark_Button'Access,
      Create_Checkbox => New_Dark_Checkbox'Access);

   Light_Factory : constant UI_Factory :=
     (Create_Button   => New_Light_Button'Access,
      Create_Checkbox => New_Light_Checkbox'Access);

   procedure Render (Button : Button_Kind) is
   begin
      case Button is
         when Dark_Button  => Put_Line ("Dark Button");
         when Light_Button => Put_Line ("Light Button");
      end case;
   end Render;

   procedure Render (Checkbox : Checkbox_Kind) is
   begin
      case Checkbox is
         when Dark_Checkbox  => Put_Line ("Dark Checkbox");
         when Light_Checkbox => Put_Line ("Light Checkbox");
      end case;
   end Render;

   procedure Create_UI_Components (Factory : UI_Factory) is
      Button   : constant Button_Kind := Factory.Create_Button.all;
      Checkbox : constant Checkbox_Kind := Factory.Create_Checkbox.all;
   begin
      Render (Button);
      Render (Checkbox);
   end Create_UI_Components;

begin
   Create_UI_Components (Dark_Factory);
   Create_UI_Components (Light_Factory);
end Example1;
