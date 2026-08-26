with Ada.Text_IO; use Ada.Text_IO;

procedure Flyweight is
   type Color_Kind is (Red, Blue);

   type Style is record
      Font  : String (1 .. 5);
      Size  : Positive;
      Color : Color_Kind;
      Used  : Boolean;
   end record;

   Pool : array (1 .. 2) of Style :=
     (others => (Font => "     ", Size => 1, Color => Red, Used => False));

   function Get_Style
     (Font : String; Size : Positive; Color : Color_Kind) return Positive is
   begin
      for I in Pool'Range loop
         if Pool (I).Used
           and then Pool (I).Font = Font
           and then Pool (I).Size = Size
           and then Pool (I).Color = Color
         then
            return I;
         end if;
      end loop;

      for I in Pool'Range loop
         if not Pool (I).Used then
            Pool (I) := (Font => Font, Size => Size, Color => Color, Used => True);
            return I;
         end if;
      end loop;

      raise Program_Error with "style pool exhausted";
   end Get_Style;

   Red_1   : constant Positive := Get_Style ("Inter", 12, Red);
   Red_2   : constant Positive := Get_Style ("Inter", 12, Red);
   Blue_Id : constant Positive := Get_Style ("Inter", 12, Blue);
   Count   : Natural := 0;
begin
   pragma Assert (Pool (Blue_Id).Color = Blue);
   for Item of Pool loop
      if Item.Used then
         Count := Count + 1;
      end if;
   end loop;

   Put_Line
     ("styles=" & Natural'Image (Count) (2 .. Natural'Image (Count)'Last) &
      ";shared=" & (if Red_1 = Red_2 then "true" else "false") &
      ";text=ABC");
end Flyweight;
