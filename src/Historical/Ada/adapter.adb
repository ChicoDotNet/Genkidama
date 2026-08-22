with Ada.Text_IO; use Ada.Text_IO;

procedure Adapter is
   function Read_Fahrenheit return Integer is (86);

   function Read_Celsius return Integer is
     (((Read_Fahrenheit - 32) * 5) / 9);
begin
   Put_Line ("legacy=" & Integer'Image (Read_Fahrenheit) & "F");
   Put_Line ("adapted=" & Integer'Image (Read_Celsius) & "C");
end Adapter;
