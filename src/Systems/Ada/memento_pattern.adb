with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Memento_Pattern return Boolean is
   State         : Unbounded_String := To_Unbounded_String ("draft");
   Snapshot      : constant Unbounded_String := State;
   Was_Published : Boolean;
begin
   State := To_Unbounded_String ("published");
   Was_Published := To_String (State) = "published";
   State := Snapshot;
   return Was_Published and then To_String (State) = "draft";
end Memento_Pattern;
