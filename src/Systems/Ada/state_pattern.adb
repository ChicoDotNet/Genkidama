with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function State_Pattern return Boolean is
   function Transition (State, Action : String) return String is
   begin
      if State = "locked" and then Action = "unlock" then
         return "unlocked";
      elsif State = "unlocked" and then Action = "lock" then
         return "locked";
      end if;
      return State;
   end Transition;

   Current : Unbounded_String := To_Unbounded_String ("locked");
begin
   Current := To_Unbounded_String (Transition (To_String (Current), "unlock"));
   Current := To_Unbounded_String (Transition (To_String (Current), "lock"));
   return To_String (Current) = "locked";
end State_Pattern;
