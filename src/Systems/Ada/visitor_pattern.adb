with Ada.Numerics;

function Visitor_Pattern return Boolean is
   type Shape_Kind is (Circle, Rectangle);
   type Shape (Kind : Shape_Kind := Circle) is record
      case Kind is
         when Circle =>
            Radius : Long_Float := 0.0;
         when Rectangle =>
            Width, Height : Long_Float := 0.0;
      end case;
   end record;

   function Area (Item : Shape) return Long_Float is
   begin
      case Item.Kind is
         when Circle =>
            return Long_Float (Ada.Numerics.Pi) * Item.Radius * Item.Radius;
         when Rectangle =>
            return Item.Width * Item.Height;
      end case;
   end Area;

   Total : constant Long_Float :=
     Area ((Kind => Circle, Radius => 2.0))
     + Area ((Kind => Rectangle, Width => 3.0, Height => 4.0));
   Expected : constant Long_Float := 4.0 * Long_Float (Ada.Numerics.Pi) + 12.0;
begin
   return abs (Total - Expected) < 1.0E-9;
end Visitor_Pattern;
