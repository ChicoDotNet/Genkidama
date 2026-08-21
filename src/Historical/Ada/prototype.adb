with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Prototype is
   type Feature_Array is array (Positive range 1 .. 4) of Unbounded_String;

   type Service_Profile is record
      Name          : Unbounded_String;
      Features      : Feature_Array;
      Feature_Count : Natural range 0 .. 4 := 0;
   end record;

   function Clone_Profile (Source : Service_Profile) return Service_Profile is
   begin
      return Source;
   end Clone_Profile;

   procedure Add_Feature (Profile : in out Service_Profile; Feature : String) is
   begin
      Profile.Feature_Count := Profile.Feature_Count + 1;
      Profile.Features (Profile.Feature_Count) := To_Unbounded_String (Feature);
   end Add_Feature;

   function Describe (Profile : Service_Profile) return String is
      Result : Unbounded_String := Profile.Name & ": ";
   begin
      for Index in 1 .. Profile.Feature_Count loop
         if Index > 1 then
            Append (Result, ",");
         end if;
         Append (Result, Profile.Features (Index));
      end loop;
      return To_String (Result);
   end Describe;

   Original : Service_Profile;
   Canary   : Service_Profile;
begin
   Original.Name := To_Unbounded_String ("orders");
   Add_Feature (Original, "metrics");

   Canary := Clone_Profile (Original);
   Canary.Name := To_Unbounded_String ("orders-canary");
   Add_Feature (Canary, "tracing");

   Put_Line ("original=" & Describe (Original));
   Put_Line ("clone=" & Describe (Canary));
end Prototype;
