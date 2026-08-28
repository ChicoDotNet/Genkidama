function Null_Object_Pattern return Boolean is
   type Logger_Access is access function (Message : String) return String;

   function Null_Log (Message : String) return String is
      pragma Unreferenced (Message);
   begin
      return "";
   end Null_Log;

   function Real_Log (Message : String) return String is ("log:" & Message);
   Null_Logger : constant Logger_Access := Null_Log'Access;
   Real_Logger : constant Logger_Access := Real_Log'Access;
begin
   return Null_Logger ("x") = "" and then Real_Logger ("x") = "log:x";
end Null_Object_Pattern;
